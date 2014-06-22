;;;; weblocks-cms-import-export-data.lisp

(in-package #:weblocks-cms-import-export-data)

;;; "weblocks-cms-import-export-data" goes here. Hacks and glory await!

(defmacro with-yaclml (&body body)
  "A wrapper around cl-yaclml with-yaclml-stream macro."
  `(yaclml:with-yaclml-stream weblocks:*weblocks-output-stream*
     ,@body))

(defun get-store-type (store)
  (loop for i in (weblocks-stores:list-store-types) do
        (when (find-symbol (string-upcase (type-of store))
                           (find-package (alexandria:make-keyword (format nil "WEBLOCKS-~A" (string-upcase i)))))
          (return-from get-store-type i))))

(defun write-model-export-data (model s)
  (json:with-array (s)
                   (loop for i in (all-of model)
                         do 
                         (json:as-array-member (s)
                           (json:with-object (s)
                             (loop for j in (weblocks-stores:class-visible-slots model) 
                                   do 

                                   (json:encode-object-member 
                                     (alexandria:make-keyword (c2mop:slot-definition-name j))
                                     (cond 
                                       ((typep (slot-value i (c2mop:slot-definition-name j)) 'standard-object)
                                        (format nil "#+lisp-object(~A . ~A)" 
                                                (write-to-string (type-of (slot-value i (c2mop:slot-definition-name j)))) 
                                                (weblocks-stores:object-id (slot-value i (c2mop:slot-definition-name j)))))
                                       ((typep (slot-value i (c2mop:slot-definition-name j)) 'hash-table)
                                        (format nil "#+hash-table~A" (write-to-string (alexandria:hash-table-alist (slot-value i (c2mop:slot-definition-name j))))))
                                       (t (slot-value i (c2mop:slot-definition-name j))))
                                     s)))))))

(defun get-model-export-data (model)
  (with-output-to-string (s)
    (write-model-export-data model s)))

(defun get-models-export-data (model-classes)
  (with-output-to-string (s)
    (json:with-object (s)
      (loop for i in model-classes do 
            (json:as-object-member (i s)
              (write-model-export-data i s))))))

(defvar *update-meta-callbacks* nil)

(defun import-model-data-item (store model item-data &key (update-meta t) (testp nil))
  (declare (special *update-meta-callbacks*))

  (let ((item (make-instance model))
        (slot-name))

    (loop for i in (weblocks-stores:class-visible-slots model) 
          do 
          (setf slot-name (c2mop:slot-definition-name i))
          (setf (slot-value item slot-name)
                (cdr (assoc (alexandria:make-keyword slot-name) item-data))))

    (flet ((update-meta ()
             (loop for i in (weblocks-stores:class-visible-slots model) 
                   do 
                   (let* ((slot-name (c2mop:slot-definition-name i))
                          (slot-value (slot-value item slot-name)))
                     (when (stringp slot-value)
                       (ppcre:register-groups-bind 
                         (data)
                         ((ppcre:create-scanner "#\\+lisp-code(.*)$" :single-line-mode t)  slot-value)
                         (let ((data (read-from-string data)))
                           (if (listp data)
                             (setf data (loop for i in data 
                                              collect (progn 
                                                        (if (and (listp i) (equal (car i) :lisp-object))
                                                          (or 
                                                            (first-by-values (second i) :id (third i))
                                                            (error "Cannot find ~A with id ~A" (second i) (third i))
                                                            )
                                                          i)))))
                           (setf (slot-value item slot-name) data)))
                       (ppcre:register-groups-bind 
                         (data)
                         ("#\\+lisp-object(.*)$" slot-value)
                         (setf data (read-from-string data))
                         (setf (slot-value item slot-name)
                               (or 
                                 (first-by-values (car data) :id (cdr data) :store store)
                                 (error "Not found model ~A with id ~A" (write-to-string (car data)) (cdr data)))))
                       (ppcre:register-groups-bind 
                         (data)
                         ((ppcre:create-scanner "#\\+hash-table(.*)$" :single-line-mode t) slot-value)
                         (setf data (read-from-string data))
                         (let ((result-hash (make-hash-table)))
                           (loop for (key . value) in data do 
                                 (setf (gethash key result-hash) value))
                           (setf (slot-value item slot-name) result-hash)))
                       )))))
      (if update-meta 
        (update-meta)
        (push #'update-meta *update-meta-callbacks*)))
    (unless testp 
      (weblocks-stores:persist-object store item))
    item))

(defun update-model-id-counter (store model)
  (let ((max (apply #'max (list* -1 (mapcar #'weblocks-stores:object-id (all-of model :store store))))))
    (ignore-errors (setf (slot-value (weblocks-prevalence::get-root-object store model) 'weblocks-prevalence::next-id) max))))

(defun import-model-data (store model data &key (update-meta t) (testp nil))
  (unless testp 
    (weblocks-utils:delete-all model :store store))

  (prog1 
    (loop for i in data collect
          (import-model-data-item store model i :update-meta update-meta :testp testp))

    (update-model-id-counter store model)))

(defun run-update-meta-callbacks ()
  (declare (special *update-meta-callbacks*))
  (mapcar #'funcall *update-meta-callbacks*))

(defun import-model-data-with-meta-deferred (store data data-package &key (testp nil))
  (let ((*update-meta-callbacks* nil))
    (declare (special *update-meta-callbacks*))
    (import-model-data store data data-package :update-meta nil :testp testp)
    (run-update-meta-callbacks)))

(defun import-models-data (store data &optional data-package)
  "Imports all data specified for store."
  (unless data-package 
    (setf data-package (get-store-data-package store)))
  (let ((*update-meta-callbacks* nil))
    (declare (special *update-meta-callbacks*))

    (loop for (key . value) in data do 
          (import-model-data store (intern (string key) data-package)
                             value
                             :update-meta nil))

    (run-update-meta-callbacks)))

(defun get-store-data-package (store)
  (let ((packages (remove-duplicates (mapcar #'symbol-package (weblocks-stores:list-model-classes store)))))
    (when (second packages)
      (error "There are several packages possible for store ~A" store))
    (intern (package-name (first packages)))))


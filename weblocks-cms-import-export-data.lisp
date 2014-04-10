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
                                     (if (consp (slot-value i (c2mop:slot-definition-name j)))
                                       (format nil "#+lisp-code~A" (write-to-string (slot-value i (c2mop:slot-definition-name j))))
                                       (if (typep (slot-value i (c2mop:slot-definition-name j)) 'standard-object)
                                         (format nil "#+lisp-object(~A . ~A)" 
                                                 (write-to-string (type-of (slot-value i (c2mop:slot-definition-name j)))) 
                                                 (weblocks-stores:object-id (slot-value i (c2mop:slot-definition-name j))))
                                         (slot-value i (c2mop:slot-definition-name j))))
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

(defun import-model-data-item (store model item-data &key (update-meta t))
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
                         ("#\\+lisp-code(.*)$"  slot-value)
                         (setf (slot-value item slot-name) (read-from-string data)))
                       (ppcre:register-groups-bind 
                         (data)
                         ("#\\+lisp-object(.*)$" slot-value)
                         (setf data (read-from-string data))
                         (setf (slot-value item slot-name)
                               (or 
                                 (first-by-values (car data) :id (cdr data) :store store)
                                 (error "Not found model ~A with id ~A" (write-to-string (car data)) (cdr data))))))))))
      (if update-meta 
        (update-meta)
        (push #'update-meta *update-meta-callbacks*)))
    (weblocks-stores:persist-object store item)))

(defun update-model-id-counter (store model)
  (let ((max (apply #'max (list* -1 (mapcar #'weblocks-stores:object-id (all-of model :store store))))))
    (ignore-errors (setf (slot-value (weblocks-prevalence::get-root-object store model) 'weblocks-prevalence::next-id) max))))

(defun import-model-data (store model data &key (update-meta t))
  (weblocks-utils:delete-all model :store store)

  (loop for i in data do
        (import-model-data-item store model i :update-meta update-meta))

  (update-model-id-counter store model))

(defun run-update-meta-callbacks ()
  (declare (special *update-meta-callbacks*))
  (mapcar #'funcall *update-meta-callbacks*))

(defun import-model-data-with-meta-deferred (store data data-package)
  (let ((*update-meta-callbacks* nil))
    (declare (special *update-meta-callbacks*))
    (import-model-data store data data-package :update-meta nil)
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


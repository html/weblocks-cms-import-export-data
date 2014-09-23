;;;; weblocks-cms-import-export-data.lisp

(in-package #:weblocks-cms-import-export-data)

;;; "weblocks-cms-import-export-data" goes here. Hacks and glory await!

(defmacro with-yaclml (&body body)
  "A wrapper around cl-yaclml with-yaclml-stream macro."
  `(yaclml:with-yaclml-stream weblocks:*weblocks-output-stream*
     ,@body))

(defun get-store-type (store)
  (if (equal "HU.DWIM.PEREC" (package-name (symbol-package (type-of store))) )
    (return-from get-store-type :perec))

  (loop for i in (weblocks-stores:list-store-types) do
        (when (find-symbol (string-upcase (type-of store))
                           (find-package (alexandria:make-keyword (format nil "WEBLOCKS-~A" (string-upcase i)))))
          (return-from get-store-type i))))

(defun write-model-export-data (model s)
  (write-string (weblocks-stores:serialize (all-of model) :format :json) s))

(defun get-model-export-data (model)
  (with-output-to-string (s)
    (write-model-export-data model s)))

(defun get-models-export-data (model-classes)
  (with-output-to-string (s)
    (json:with-object (s)
      (loop for i in model-classes do 
            (json:as-object-member (i s)
              (write-model-export-data i s))))))

(defvar *update-meta-deferred* nil)
(defvar *update-meta-callbacks* nil)

(defun update-model-id-counter (store model)
  (let ((max (apply #'max (list* -1 (mapcar #'weblocks-stores:object-id (all-of model :store store))))))
    (ignore-errors (setf (slot-value (weblocks-prevalence::get-root-object store model) 'weblocks-prevalence::next-id) max))))

(defun import-model-data (store model json-string &key (update-meta t) (testp nil))
  (unless testp 
    (weblocks-utils:delete-all model :store store))

  (let* ((data (json:decode-json-from-string json-string))
         (result-records (loop for i in data collect
                               (weblocks-stores:unserialize (json:encode-json-alist-to-string i) :format :json))))
    (unless testp 
      (loop for i in result-records do 
            (weblocks-stores:persist-object store i)))

    (update-model-id-counter store model)

    result-records))

(defun run-update-meta-callbacks ()
  (declare (special *update-meta-callbacks*))
  (mapcar #'funcall *update-meta-callbacks*))

(defun import-model-data-with-meta-deferred (store model data &key (testp nil))
  (let ((*update-meta-callbacks* nil)
        (*update-meta-deferred* t))
    (declare (special *update-meta-callbacks*))
    (import-model-data store model data :update-meta nil :testp testp)
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


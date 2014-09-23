(in-package :weblocks-cms-import-export-data)

(defgeneric serialization-link-to-data-object (object)
  (:method ((obj standard-object))
   `(:eval 
      (first-by-id ',(type-of obj)
                   ,(weblocks-stores:object-id obj)))))

(defmethod weblocks-stores::serialize-impl (obj &key (format (eql :json)))
  (labels ((lisp-code (value inside-lisp-code-p)
             (if inside-lisp-code-p 
               value 
               (format nil "#+lisp~A" (prin1-to-string value))))
           (serialize-value (value &optional inside-lisp-code-p)
             (cond 
               ((null value)
                nil)
               ((typep value 'standard-object)
                (lisp-code 
                  (serialization-link-to-data-object value)
                  inside-lisp-code-p))
               ((typep value 'hash-table)
                (lisp-code 
                  `(:eval 
                     (alexandria:alist-hash-table 
                       ',(loop for (key . val) in (alexandria:hash-table-alist value)
                               collect
                               (cons (serialize-value key t) (serialize-value val t)))))
                  inside-lisp-code-p))
               ((consp value)
                (lisp-code
                  (cons 
                    (serialize-value (car value) t)
                    (serialize-value (cdr value) t))
                  inside-lisp-code-p))
               ((stringp value)
                value)
               (t (lisp-code value inside-lisp-code-p)))))
    (let ((model (type-of obj)))
      (json:with-object ()
        (json:encode-object-member :cl-type (prin1-to-string model))
        (loop for j in (weblocks-stores:class-visible-slots model) 
              ;if (equal (c2mop:slot-definition-name j) 'weblocks-cms::social-networks)
              do 
              (let ((value (slot-value obj (c2mop:slot-definition-name j))))
                (json:encode-object-member 
                  (alexandria:make-keyword (c2mop:slot-definition-name j))
                  (serialize-value value))))))))

(defmethod weblocks-stores:serialize ((obj standard-object) &key (format (eql :json)))
  (with-output-to-string (json:*json-output*)
    (weblocks-stores::serialize-impl obj :format format)))

(defmethod weblocks-stores:serialize ((list list) &key (format (eql :json)))
  (with-output-to-string (json:*json-output*)
    (json:with-array 
      (json:*json-output*)
      (loop for i in list
            do 
            (json:as-array-member (json:*json-output*)
              (format json:*json-output* (weblocks-stores:serialize i :format :json)))))))

(defun first-by-id (model id)
  (first-by-values model :id id))

(defmethod weblocks-stores:unserialize (obj &key (format (eql :json)))
  (let* ((item-data (json:decode-json-from-string obj))
         (model (read-from-string (cdr (assoc :cl-type item-data))))
         (item (make-instance model))
         (slot-name))

    (loop for i in (weblocks-stores:class-visible-slots model) 
          do 
          (setf slot-name (c2mop:slot-definition-name i))
          (setf (slot-value item slot-name)
                (cdr (assoc (alexandria:make-keyword slot-name) item-data))))

    (labels ((unserialize-value (item slot-name slot-value)
               (cond 
                 ((and (stringp slot-value) (string= "f" slot-value))
                  t)
                 ((stringp slot-value)
                  (ppcre:register-groups-bind 
                    (data )
                    ((ppcre:create-scanner "#\\+lisp(.*)$" :single-line-mode t) slot-value)
                    (setf data (read-from-string data)) 
                    (when (and (consp data) (equal :eval (car data)))
                      (if *update-meta-deferred*
                        (progn 
                          (push 
                            (lambda ()
                              (setf (slot-value item slot-name)
                                    (eval (second data))))
                            *update-meta-callbacks*)
                          (return-from unserialize-value :deferred-value))
                        (setf data (eval (second data)))))
                    
                    (return-from unserialize-value data))
                  slot-value)
                 (t slot-value))))
      (flet ((update-meta ()
               (loop for i in (weblocks-stores:class-visible-slots model) 
                     do 
                     (let* ((slot-name (c2mop:slot-definition-name i))
                            (slot-value (slot-value item slot-name)))
                       (setf (slot-value item slot-name)
                             (unserialize-value item slot-name slot-value))))))
        
        (update-meta)))

    item))


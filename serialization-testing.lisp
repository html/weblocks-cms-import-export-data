(in-package :weblocks-cms-import-export-data)

(defun reserialized-copy (object)
  "Serializes object and than unserializes it"
  (ignore-errors 
    (weblocks-stores:unserialize 
      (weblocks-stores:serialize object :format :json)
      :format :json)))

(defun equalp-improved (obj1 obj2)
  (cond 
    ((typep obj1 'standard-object)
     (equal (weblocks-stores:object-id obj1) (weblocks-stores:object-id obj2)))
    ((consp obj1)
     (progn 
       (and 
         (equalp-improved (car obj1) (car obj2))
         (equalp-improved (cdr obj1) (cdr obj2)))))
    ((hash-table-p obj1)
     (equalp-improved 
       (sort (alexandria:hash-table-alist obj1) #'string>
             :key #'car)
       (sort (alexandria:hash-table-alist obj2) #'string> 
             :key #'car)))
    (t (equal obj1 obj2))))


(defun serialization-error-p (object)
  "T if reserialized copy is not equal to original"
  (flet ((objects-differ-p (obj1 obj2)
           (cdr (clos-diff:diff obj1 obj2 :test #'equalp-improved))))
    (objects-differ-p 
      (reserialized-copy object)
      object)))

(defun smart-describe-to-string (obj)
  (with-output-to-string (s) 
    (cond 
      ((hash-table-p obj) 
       (format s "~A" (sort (alexandria:hash-table-alist obj) #'string> :key #'car)))
      (t (describe obj  s)))))

(defun print-objects-diff (obj1 obj2)
  (let ((difference (cdr (clos-diff:diff obj1 obj2 :test #'equalp-improved))))
    (when difference 
      (loop for (dummy slot value) in difference do 
            (format t "Slot ~A is different~%~%Object 1 value - ~%~%~A~%Object 2 value - ~%~%~A~%~%" 
                    (prin1-to-string slot)
                    (smart-describe-to-string (slot-value obj1 slot))
                    (smart-describe-to-string (slot-value obj2 slot)))))))

(defun print-records-serialized-with-errors (model)
  "Reserializes all records for specific `model`, if any reserialized record is different from original, shows a message"
  (loop for i in (all-of model)
        if (serialization-error-p i)
        do 
        (describe i)
        (print-objects-diff i (reserialized-copy i))))

(defun model-reserialization-ok-p (model)
  "Reserializes all records for specific `model`, returns true if all records reserialized correctly, nil in other case"
  
  (loop for i in (all-of model)
        if (serialization-error-p i)
        do
        (return-from model-reserialization-ok-p nil))

  t)

(defun store-reserialization-ok-p (store)
  "Reserializes all records for all models of store. If all records reserialized correctly, returns true, nil in other case"
  (loop for model in (weblocks-stores:list-model-classes store)
        if (not (model-reserialization-ok-p model))
        do (return-from store-reserialization-ok-p nil))
  t)

(defun all-stores-reserialization-ok-p ()
  "Reserializes all records in all models of all stores. If all records reserialized correctly, returns true, nil in other case"
  (loop for i in weblocks-stores:*store-names* 
        if (not (store-reserialization-ok-p  (symbol-value i)))
        do 
        (return-from all-stores-reserialization-ok-p nil))
  t)

(defun records-with-reserialization-errors (model)
  "Reserializes all records for specific `model`, if any reserialized record is different from original, shows a message"
  (loop for i in (all-of model)
        if (serialization-error-p i)
        collect i))

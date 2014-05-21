(in-package :weblocks-cms-import-export-data)

(defun import-callback (callback)
  (ps:ps-inline* 
    `(initiate-action 
       ,(weblocks:make-action 
          (lambda (&rest args)
            (weblocks:do-page 
              (weblocks:make-quickform 
                (weblocks:defview nil (:type weblocks:form :persistp nil)
                                  (data :requiredp t 
                                        :present-as textarea))
                :satisfies (lambda (form data)
                             (let ((ret (handler-case 
                                          (progn 
                                            (funcall callback (json:decode-json-from-string (slot-value data 'data)))
                                            t)
                                          (t (var) (with-output-to-string (s)
                                                     (let ((*print-escape* nil))
                                                       (print-object var s)))))))
                               (if (equal ret t)
                                 t
                                 (values  nil `((data . ,(format nil "Importing data failed - ~A" ret)))))))
                :on-success (lambda/cc (form data)
                                       (progn 
                                         (weblocks:do-information "Import finished")
                                         (weblocks:answer form t)))
                :on-cancel (lambda (form)
                             (weblocks:answer form t))
                :answerp nil))))
       ,(weblocks:session-name-string-pair))))

(defun import-export-data-ui (&rest args)
  (with-yaclml 
    (<ul
      (loop for i in weblocks-stores:*store-names* do 
            (let ((i-copy i))
              (<li 
                (<i (<:as-html (string-downcase (format nil "~a::~a" (package-name (symbol-package i)) i))))
                (if (symbol-value i)
                  (<:as-is (format nil "&nbsp;of type ~A" (write-to-string (get-store-type (symbol-value i)))))
                  (<:as-is "&nbsp;(closed)"))
                (<:as-is " ")
                (<a :href (weblocks:add-get-param-to-url 
                            (weblocks:make-action-url 
                              (weblocks:make-action 
                                (lambda (&rest args)
                                  (setf (hunchentoot:header-out :content-type) "text/x-json")
                                  (get-models-export-data (weblocks-stores:list-model-classes (symbol-value i))))))
                            "pure"
                            "true")
                    :target "_blank" "export data")
                (<:as-is " | ")
                (<a :href "javascript:;"
                    :onclick (import-callback 
                               (lambda (data)
                                 (import-models-data (symbol-value i-copy) data)))
                    "import data")
                (<:as-is " | ")
                (<a :href "javascript:;"
                    :onclick (ps:ps-inline* 
                               `(initiate-action 
                                  ,(weblocks:make-action 
                                     (lambda/cc (&rest args)
                                                (if (equal :ok 
                                                           (weblocks:do-confirmation 
                                                             (format nil "Are you sure you want to delete all data for ~A store ?" (write-to-string i-copy))))
                                                  (progn 
                                                    (weblocks-stores:clean-store (symbol-value i-copy))
                                                    (weblocks:do-information (format nil "All data deleted for ~A" (write-to-string i-copy)))))))
                                  ,(weblocks:session-name-string-pair)))
                    "delete all data")
                (<ul 
                  (loop for j in (weblocks-stores:list-model-classes (symbol-value i)) do 
                        (let ((j-copy j))
                          (<li (<:as-html (string-downcase (write-to-string j)))
                               (<:format " (records count - ~A)" (weblocks-utils:count-of j))
                               " - "
                               (<a :target "_blank"
                                   :href (weblocks:add-get-param-to-url 
                                           (weblocks:make-action-url 
                                             (weblocks:make-action 
                                               (lambda (&rest args)
                                                 (setf (hunchentoot:header-out :content-type) "text/x-json")

                                                 (get-model-export-data j-copy))))
                                           "pure"
                                           "true") "export data")
                               (<:as-is " | ")
                               (<a :href "javascript:;"
                                   :onclick (import-callback 
                                              (lambda (data)
                                                (import-model-data-with-meta-deferred (symbol-value i-copy) j-copy data)))
                                   "import data")
                               (<:as-is " | ")
                               (<a :href "javascript:;"
                                   :onclick (ps:ps-inline* 
                                              `(initiate-action 
                                                 ,(weblocks:make-action 
                                                    (lambda/cc (&rest args)
                                                               (if (equal :ok 
                                                                          (weblocks:do-confirmation 
                                                                            (format nil "Are you sure you want to delete all data for ~A model ?" (write-to-string j-copy))))
                                                                 (progn 
                                                                   (weblocks-stores:delete-model-class (symbol-value i) j-copy)
                                                                   (weblocks:do-information (format nil "All data deleted for ~A" (write-to-string j-copy)))))))
                                                 ,(weblocks:session-name-string-pair)))
                                   "delete all data")))))))))))

(push (lambda () 
        (list (weblocks::translate "Import/Export Data")
              #'weblocks-cms-import-export-data:import-export-data-ui
              "import-export-data"))
      weblocks-cms:*admin-menu-widgets*)

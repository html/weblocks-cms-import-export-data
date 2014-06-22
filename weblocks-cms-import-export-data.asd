;;;; weblocks-cms-import-export-data.asd

(asdf:defsystem #:weblocks-cms-import-export-data
  :serial t
  :description "Import/export separate model data, all models data"
  :author "Olexiy Zamkoviy <olexiy.z@gmail.com>"
  :version (:read-from-file "version.lisp-expr")
  :license "LLGPL"
  :depends-on (#:weblocks
               #:weblocks-cms 
               #:weblocks-prevalence 
               #:weblocks-stores 
               #:weblocks-utils 
               #:closer-mop 
               #:cl-json 
               #:clos-diff)
  :components 
  ((:file "package")
   (:file "serialization")
   (:file "serialization-testing")
   (:file "weblocks-cms-import-export-data")
   (:file "import-export-data-ui")))


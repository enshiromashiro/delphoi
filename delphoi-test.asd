#|
  This file is a part of delphoi project.
  Copyright (c) 2014 subaru45
|#

(in-package :cl-user)
(defpackage delphoi-test-asd
  (:use :cl :asdf))
(in-package :delphoi-test-asd)

(defsystem delphoi-test
  :author "subaru45"
  :license "NYSL"
  :depends-on (:delphoi
               :cl-test-more)
  :components ((:file "delphoi-test"))
  :perform (load-op :after (op c) (asdf:clear-system c)))

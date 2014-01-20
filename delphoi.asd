#|
  This file is a part of delphoi project.
  Copyright (c) 2014 subaru45
|#

#|
  Author: subaru45
|#

(in-package :cl-user)
(defpackage delphoi-asd
  (:use :cl :asdf))
(in-package :delphoi-asd)

(defsystem delphoi
  :version "0.1"
  :author "subaru45"
  :license "NYSL"
  :depends-on (:cl-annot
               :cl-json
               :cl-oauth
               :series)
  :components ((:file "delphoi"))
  :description "ridicurous bot"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op delphoi-test))))

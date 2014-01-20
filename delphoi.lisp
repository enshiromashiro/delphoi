#|
  This file is a part of delphoi project.
  Copyright (c) 2014 subaru45
|#

(in-package :cl-user)
(defpackage delphoi
  (:use :cl))
(in-package :delphoi)

(cl-annot:enable-annot-syntax)


;; drakma settings
(setf drakma:*drakma-default-external-format* :utf-8)
(pushnew '("application" . "json") drakma:*text-content-types* :test #'equal)

@export
(defmacro make-access-token ()
  (let ((s (series:collect-first (series:scan-file "delphoi.conf"))))
    `(defparameter *access-token*
       (oauth:make-access-token :consumer (oauth:make-consumer-token
                                           :key ,(getf s :consumer-key)
                                           :secret ,(getf s :consumer-secret))
                                :key ,(getf s :access-key)
                                :secret ,(getf s :access-secret)))))

(make-access-token)


@export
(defun home-timeline (&key count)
  (let ((option (if count
                    (format nil "?count=~a" count)
                    "")))
    (json:decode-json-from-string
     (oauth:access-protected-resource
      (format nil "https://api.twitter.com/1.1/statuses/home_timeline.json~a" option)
      *access-token*))))

@export
(defun tweet (message)
  (when message
    (json:decode-json-from-string
     (oauth:access-protected-resource
      "https://api.twitter.com/1.1/statuses/update.json"
      *access-token*
      :request-method :post
      :user-parameters `(("status" . ,message))))))


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

(defmacro make-access-token ()
  (let ((s (series:collect-first (series:scan-file "delphoi.conf"))))
    `(defparameter *access-token*
       (oauth:make-access-token :consumer (oauth:make-consumer-token
                                           :key ,(getf s :consumer-key)
                                           :secret ,(getf s :consumer-secret))
                                :key ,(getf s :access-key)
                                :secret ,(getf s :access-secret)))))


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
      :user-parameters `(("status" . ,message))))
    message))


(defparameter *output-stream* t)

(defun print-tweet (json-string)
  (ignore-errors
    (json:with-decoder-simple-clos-semantics
      (let* ((json:*json-symbols-package* :delphoi)
             (x (json:decode-json-from-string json-string)))
        (with-slots (text user) x
          (with-slots (name screen--name) user
            (format *output-stream* "~& ~%~a (~a)~&~a~%"
                    screen--name name text)))))))

(defmacro do-user-stream (fn)
  (let ((in (gensym))
        (line (gensym)))
  `(lambda ()
     (with-open-stream (,in (oauth:access-protected-resource
                             "https://userstream.twitter.com/1.1/user.json"
                             *access-token*
                             :drakma-args '(:want-stream t)))
       (loop for ,line = (read-line ,in nil)
          while ,line
          do (funcall ,fn ,line))))))


(defparameter *user-stream-thread* nil)

(flet ((make-and-set-thread (fn)
         (setf *user-stream-thread* (bordeaux-threads:make-thread
                                     (do-user-stream fn)
                                     :name "user-stream"))))
  @export
  (defun start-user-stream (&optional fn)
    (if (not (null *user-stream-thread*))
        (bordeaux-threads:destroy-thread *user-stream-thread*))
    (make-and-set-thread (if (null fn) #'print-tweet fn))))

@export
(defun stop-user-stream ()
  (bordeaux-threads:destroy-thread *user-stream-thread*))


@export
(defun init (out)
  (make-access-token)
  (setf *output-stream* out))

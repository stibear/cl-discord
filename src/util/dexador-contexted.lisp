#|
  This file is a part of cl-discord project.
  Copyright (c) 2017 stibear (stibear.lisp@gmail.com)
|#

(in-package :cl-user)
(defpackage dexador-contexted
  (:use :cl)
  (:shadow :get)
  (:export :get :post)
  (:nicknames :dexc))
(in-package :dexador-contexted)

(defun get (uri &rest args
            &key version headers basic-auth cookie-jar keep-alive use-connection-pool timeout
	      max-redirects force-binary want-stream ssl-key-file ssl-cert-file ssl-key-password stream
	      verbose proxy insecure)
  (declare (ignore version headers basic-auth cookie-jar keep-alive use-connection-pool timeout
	      max-redirects force-binary want-stream ssl-key-file ssl-cert-file ssl-key-password stream
	      verbose proxy insecure))
  (let ((ctx (cl+ssl:make-context :verify-mode cl+ssl:+ssl-verify-none+)))
    (cl+ssl:with-global-context (ctx)
      (apply #'dexador:get uri args))))

(defun post (uri &rest args
             &key version content headers basic-auth cookie-jar keep-alive use-connection-pool
	       timeout force-binary want-stream ssl-key-file ssl-cert-file ssl-key-password stream verbose
	       proxy insecure)
  (declare (ignore version content headers basic-auth cookie-jar keep-alive use-connection-pool
	       timeout force-binary want-stream ssl-key-file ssl-cert-file ssl-key-password stream verbose
	       proxy insecure))
  (let ((ctx (cl+ssl:make-context :verify-mode cl+ssl:+ssl-verify-none+)))
    (cl+ssl:with-global-context (ctx)
      (apply #'dexador:post uri args))))

(defun put ())

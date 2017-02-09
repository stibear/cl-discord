#|
  This file is a part of cl-discord project.
  Copyright (c) 2017 stibear (stibear.lisp@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-discord.core
  (:use :cl)
  (:export
   #:*token*
   #:send))
(in-package :cl-discord.core)

(defparameter *token* "YOUR TOKEN")

(defparameter *api-base-url* "https://discordapp.com/api")

(defun send (params &key (method :get) content)
  (let ((headers `(("Authorization" . ,(format nil "Bot ~A" *token*))
		   ("User-Agent" . "DiscordBot"))))
    (jsown:parse
     (case method
       (:get
	(dexc:get (format nil "~A/~{~A~^/~}" *api-base-url* params) :headers headers))
       (:post
	(dexc:post (format nil "~A/~{~A~^/~}" *api-base-url* params)
		   :headers (cons '("Content-Type" . "application/json") headers)
		   :content content))
       (:put)))))

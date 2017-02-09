#|
  This file is a part of cl-discord project.
  Copyright (c) 2017 stibear (stibear.lisp@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-discord.guild
  (:use :cl :cl-discord.core)
  (:export
   #:create-guild
   #:get-guild))
(in-package :cl-discord.guild)

(defun send-guilds (params &rest args &key method content)
  (declare (ignore method content))
  (apply #'send (cons "guilds" params) args))

(defun create-guild (guild-obj)
  (send-guilds '() :method :post :content guild-obj))

(defun get-guild (guild-id)
  (send-guilds `(,guild-id)))

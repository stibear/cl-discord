#|
  This file is a part of cl-discord project.
  Copyright (c) 2017 stibear (stibear.lisp@gmail.com)
|#

#|
  Discord API Library for Common Lisp

  Author: stibear (stibear.lisp@gmail.com)
|#



(in-package :cl-user)
(defpackage cl-discord-asd
  (:use :cl :asdf))
(in-package :cl-discord-asd)


(defsystem cl-discord
  :version "0.1"
  :author "stibear"
  :mailto "stibear.lisp@gmail.com"
  :license "MIT License"
  :depends-on (:cl-async
	       :websocket-driver-client
	       :dexador
	       :jsown
	       :bordeaux-threads
	       :cl+ssl
	       :event-emitter
	       
	       :string-case)
  :components ((:module "src"
                :components
                ((:module "util"
		  :components
		  ((:file "dexador-contexted")))
		 (:file "core")
		 (:file "channel")
		 (:file "guild")
		 (:file "gateway")
		 (:file "package"))))
  :serial t
  :description "Discord API Library for Common Lisp"
  :in-order-to ((test-op (test-op :cl-discord.test))))

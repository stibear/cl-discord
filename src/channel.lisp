#|
  This file is a part of cl-discord project.
  Copyright (c) 2017 stibear (stibear.lisp@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-discord.channel
  (:use :cl :cl-discord.core)
  (:export
   #:get-channel
   #:modify-channel
   #:delete-channel
   #:get-channel-messages
   #:get-channel-message
   #:create-message
   #:create-reaction
   #:delete-own-reaction
   #:delete-user-reaction
   #:get-reactions
   #:delete-all-reactios
   #:edit-message
   #:delete-message
   #:bulk-delete-message
   #:edit-channel-permissions
   #:get-channel-invites
   #:create-channel-invite
   #:delete-channel-permission
   #:trigger-typing-indicator
   #:get-pinned-messages
   #:add-pinned-channel-message
   #:delete-pinned-channel-message
   #:group-dm-add-recipient
   #:group-dm-remove-recipient))
(in-package :cl-discord.channel)

(defun send-channels (params &rest args &key method content)
  (declare (ignore method content))
  (apply #'send (cons "channels" params) args))

(defun get-channel (channel-id)
  (send-channels `(,channel-id)))

(defun modify-channel (channel-id)
  (send-channels `(,channel-id) :method :put :content nil))

(defun delete-channel (channel-id)
  (send-channels `(,channel-id) :method :delete :content nil))

(defun get-channel-messages (channel-id)
  (send-channels `(,channel-id "messages")))

(defun get-channel-message (channel-id message-id)
  (send-channels `(,channel-id "messages" ,message-id)))

(defun create-message (channel-id content &key nonce tts file embed)
  (declare (ignorable nonce tts file embed))
  (send-channels `(,channel-id "messages") :method :post
					   :content (jsown:to-json
						     (jsown:new-js ("content" content)))))

(defun create-reaction (channel-id message-id emoji)
  (send-channels `(,channel-id "messages" message-id "reactions" emoji "@me") :method :put))

(defun delete-own-reaction (channel-id message-id emoji)
  (send-channels `(,channel-id "messages" message-id "reactions" emoji "@me") :method :delete))

(defun delete-user-reaction (channel-id message-id emoji)
  (send-channels `(,channel-id "messages" message-id "reactions" emoji) :method :delete))

(defun get-reactions (channel-id message-id emoji)
  (send-channels `(,channel-id "messages" message-id "reactions" emoji)))

(defun delete-all-reactios (channel-id message-id)
  (send-channels `(,channel-id "messages" message-id "reactions") :method :delete))

(defun edit-message (channel-id message-id content)
  (send-channels `(,channel-id "messages" message-id) :method :patch :content content))

(defun delete-message ())
(defun bulk-delete-message ())
(defun edit-channel-permissions ())
(defun get-channel-invites ())
(defun create-channel-invite ())
(defun delete-channel-permission ())
(defun trigger-typing-indicator ())
(defun get-pinned-messages ())
(defun add-pinned-channel-message ())
(defun delete-pinned-channel-message ())
(defun group-dm-add-recipient ())
(defun group-dm-remove-recipient ())

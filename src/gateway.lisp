#|
  This file is a part of cl-discord project.
  Copyright (c) 2017 stibear (stibear.lisp@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-discord.gateway
  (:use :cl :cl-discord.core :event-emitter :string-case)
  (:export
   #:get-gateway
   #:discord-client
   #:make-discord-client
   #:connect
   #:disconnect
   #:reconnect
   #:start-heartbeat
   #:stop-heartbeat
   #:identify
   #:status-update
   #:voice-status-update
   #:resume
   #:request-guild-members
   #:id
   #:shard-count
   #:seq
   #:client
   #:user
   #:private-channels
   #:guilds
   #:session-id
   ;; event-emitter
   #:add-listener
   #:emit
   #:listener-count
   #:listeners
   #:on
   #:once
   #:remove-all-listeners
   #:remove-listener))
(in-package :cl-discord.gateway)

(defun get-gateway ()
  (send '("gateway" "bot")))

(defclass discord-client (event-emitter)
  ((id :initarg :id
       :reader id)
   (shard-count :accessor shard-count)
   (seq :initform 0
	:accessor seq)
   (client :accessor client)
   (gateway :accessor gateway)
   (heartbeat-fn :accessor heartbeat-fn)
   (v :accessor v)
   (user :accessor user)
   (private-channels :accessor private-channels)
   (guilds :accessor guilds)
   (session-id :initform nil
	       :accessor session-id)))

(defmethod initialize-instance :after ((client discord-client) &key)
  (let ((gateway (get-gateway)))
    (setf (shard-count client) (jsown:val gateway "shards"))
    (setf (gateway client) (format nil "~A?v=5&encoding=json" (jsown:val gateway "url")))))

(defun make-discord-client (&key (id 0))
  (make-instance 'discord-client :id id))

(defun ready-init (client v user private-channels guilds session-id)
  (setf (v client) v
	(user client) user
	(private-channels client) private-channels
	(guilds client) guilds
	(session-id client) session-id))

(defun message-emission (client message)
  (let* ((parsed (jsown:parse message))
	 (d (jsown:val parsed "d"))
	 (s (jsown:val parsed "s")))
    (setf (seq client) s)
    (string-case ((jsown:val parsed "t"))
      ("READY" (ready-init client
			   (jsown:val d "v")
			   (jsown:val d "user")
			   (jsown:val d "private_channels")
			   (jsown:val d "guilds")
			   (jsown:val d "session_id"))
	       (emit :ready client d s))
      ("RESUMED" (emit :resumed client d s))
      ("CHANNEL_CREATE" (emit :channel-create client d s))
      ("CHANNEL_UPDATE" (emit :channel-update client d s))
      ("CHANNEL_DELETE" (emit :channel-delete client d s))
      ("GUILD_CREATE" (emit :guild-create client d s))
      ("GUILD_UPDATE" (emit :guild-update client d s))
      ("GUILD_DELETE" (emit :guild-delete client d s))
      ("GUILD_BAN_ADD" (emit :guild-ban-add client d s))
      ("GUILD_BAN_REMOVE" (emit :guild-ban-remove client d s))
      ("GULID_EMOJIS_UPDATE" (emit :guild-emojis-update client d s))
      ("GUILD_INTEGRATIONS_UPDATE" (emit :guild-integrations-update client d s))
      ("GUILD_MEMBER_ADD" (emit :guild-member-add client d s))
      ("GUILD_MEMBER_REMOVE" (emit :guild-member-remove client d s))
      ("GUILD_MEMBER_UPDATE" (emit :guild-member-update client d s))
      ("GUILD_MEMBERS_CHUNK" (emit :guild-member-chunk client d s))
      ("GUILD_ROLE_CREATE" (emit :guild-role-create client d s))
      ("GUILD_ROLE_UPDATE" (emit :guild-role-update client d s))
      ("GUILD_ROLE_DELETE" (emit :guild-role-delete client d s))
      ("MESSAGE_CREATE" (emit :message-create client d s))
      ("MESSAGE_UPDATE" (emit :message-update client d s))
      ("MESSAGE_DELETE" (emit :message-delete client d s))
      ("MESSAGE_DELETE_BULK" (emit :message-delete-bulk client d s))
      ("PRESENCE_UPDATE" (emit :presence-update client d s))
      ("TYPING_START" (emit :typing-start client d s))
      ("USER_SETTINGS_UPDATE" (emit :user-settings-update client d s))
      ("USER_UPDATE" (emit :user-update client d s))
      ("VOICE_STATE_UPDATE" (emit :voice-state-update client d s))
      ("VOICE_SERVER_UPDATE" (emit :voice-server-update client d s)))))

(defun connect (client)
  (setf (client client) (wsd:make-client (gateway client)))
  (on :open (client client) (lambda () (format t "Connected.~%")))
  (on :error (client client)
      (lambda (error) (format t "Got an error: ~S~%" error)))
  (on :close (client client)
      (lambda (&rest args &key code reason)
	(declare (ignorable code reason))
	(format t "Closed: ~S~%" args)))
  (on :message (client client)
      (lambda (message)
	(let ((op (jsown:val (jsown:parse message) "op")))
	  (case op
	    (0
	     (message-emission client message))
	    (7
	     (reconnect client))
	    (9
	     (format t "WARN: Invalid session, attempting to re-identify...")
	     (identify client))
	    (10
	     (start-heartbeat client message)
	     (if (session-id client)
		 (resume client)
		 (identify client)))))))
  (wsd:start-connection (client client)))

(defun disconnect (client)
  (stop-heartbeat client)
  (wsd:close-connection (client client)))

(defun reconnect (client)
  (when (eql (wsd:ready-state (client client)) :open) (disconnect client))
  (connect client))

(defun start-heartbeat (client message)
  (let* ((obj (jsown:parse message))
	 (interval (jsown:val (jsown:val obj "d") "heartbeat_interval")))
    (setf (heartbeat-fn client)
	  (as:interval (lambda ()
			 (wsd:send (client client)
				   (jsown:to-json
				    (jsown:new-js ("op" 1) ("d" (seq client))))))
		       :time (/ interval 1000)))))

(defun stop-heartbeat (client)
  (as:remove-interval (heartbeat-fn client)))

(defun identify (client)
  (wsd:send (client client)
	    (jsown:to-json
	     (jsown:new-js
	       ("op" 2)
	       ("d" (jsown:new-js
		      ("token" *token*)
		      ("properties" (jsown:new-js
				      ("$os" (software-type))
				      ("$browser" "lisp_discord")
				      ("$device" "lisp_discord")
				      ("$referrer" "")
				      ("$referring_domain" "")))
		      ("compress" :f)
		      ("large_threshold" 100)
		      ("shard" (list (id client) (shard-count client)))))
	       ("t") ("s")))))

(defun status-update (client idle-since game-name)
  (wsd:send (client client)
	    (jsown:to-json
	     (jsown:new-js
	       ("op" 3)
	       ("d" (jsown:new-js
		      ("idle_since" (or idle-since :null))
		      ("game" (if game-name (jsown:new-js ("name" game-name)) :null))))
	       ("t") ("s")))))

(defun voice-status-update (client guild-id channel-id self-mute self-deaf)
  (wsd:send (client client)
	    (jsown:to-json
	     (jsown:new-js
	       ("op" 4)
	       ("d" (jsown:new-js
		      ("guild_id" guild-id)
		      ("channel_id" (or channel-id :null))
		      ("self_mute" (or self-mute :f))
		      ("self_deaf" (or self-deaf :f))))
	       ("t") ("s")))))

(defun resume (client)
  (wsd:send (client client)
	    (jsown:to-json
	     (jsown:new-js
	       ("op" 6)
	       ("d" (jsown:new-js
		      ("token" *token*)
		      ("session_id" (session-id client))
		      ("seq" (seq client))))
	       ("t") ("s")))))

(defun request-guild-members (client guild-id query limit)
  (wsd:send (client client)
	    (jsown:to-json
	     (jsown:new-js
	       ("op" 8)
	       ("d" (jsown:new-js 
		      ("guild_id" guild-id)
		      ("query" query)
		      ("limit" limit)))
	       ("t") ("s")))))


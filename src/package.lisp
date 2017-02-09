#|
  This file is a part of cl-discord project.
  Copyright (c) 2017 stibear (stibear.lisp@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-discord
  (:use
   :cl
   :cl-discord.core
   :cl-discord.channel
   :cl-discord.gateway)
  (:nicknames :discord)
  (:export
   ;; cl-discord.core
   #:*token*
   ;; cl-discord.channels
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
   #:group-dm-remove-recipient
   ;; cl-discord.gateway
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
   #:add-listener
   #:emit
   #:listener-count
   #:listeners
   #:on
   #:once
   #:remove-all-listeners
   #:remove-listener
   ))

;;; pubsub.el --- A basic publish/subscribe system -*- lexical-binding: t -*-

;; Author: Sid Kasivajhula <sid@countvajhula.com>
;; URL: https://github.com/countvajhula/pubsub
;; Version: 0.0
;; Package-Requires: ((emacs "24.1"))

;; This file is NOT a part of Gnu Emacs.

;; This work is "part of the world."  You are free to do whatever you
;; like with it and it isn't owned by anybody, not even the
;; creators.  Attribution would be appreciated and is a valuable
;; contribution in itself, but it is not strictly necessary nor
;; required.  If you'd like to learn more about this way of doing
;; things and how it could lead to a peaceful, efficient, and creative
;; world, and how you can help, visit https://drym.org.
;;
;; This paradigm transcends traditional legal and economic systems, but
;; for the purposes of any such systems within which you may need to
;; operate:
;;
;; This is free and unencumbered software released into the public domain.
;; The authors relinquish any copyright claims on this work.

;;; Commentary:

;; A basic publish/subscribe system.

;;; Code:

(defvar pubsub-board
  (make-hash-table :test 'equal)
  "All topics and lists of their subscribers.")

(defvar pubsub-subscriber-directory
  (make-hash-table :test 'equal)
  "Subscriber names and callbacks.

This allows us to subscribe and unsubscribe by name, rather than
directly by callback which may not be reliably identifiable if it is
an anonymous lambda.")

(defun pubsub-publish (topic notice)
  "Publish NOTICE to TOPIC.

This notifies each subscriber to TOPIC of the fresh NOTICE.

The notification is performed as a simple function invocation, where
each subscriber function (callback) to TOPIC is invoked with the fresh
NOTICE as the only argument."
  (dolist (subscriber-name (gethash topic pubsub-board))
    (let ((callback (gethash subscriber-name
                             pubsub-subscriber-directory)))
      (condition-case err
          (funcall callback notice)
        (error
         (pubsub-unsubscribe topic subscriber-name)
         (message "Error in subscriber %s on receiving notice %s on topic %s:\n%s\n They have been unsubscribed. Please fix the error and resubscribe."
                  subscriber-name
                  notice
                  topic
                  (error-message-string err)))))))

(defun pubsub-subscribe (topic subscriber-name callback)
  "Subscribe to TOPIC.

This adds CALLBACK as a subscriber to TOPIC, using SUBSCRIBER-NAME to
identify the subscriber.  The SUBSCRIBER-NAME is used to identify
duplicate subscribers (e.g., if a subscriber with that name already
exists, the new one will not be added redundantly) and also may be
used to subsequently unsubscribe the CALLBACK from TOPIC.

CALLBACK must be a function accepting a single argument.  It will be
invoked with each fresh notice on TOPIC."
  (puthash topic
           (delete-dups
            (cons subscriber-name
                  (gethash topic pubsub-board)))
           pubsub-board)
  (puthash subscriber-name
           callback
           pubsub-subscriber-directory))

(defun pubsub-unsubscribe (topic subscriber-name)
  "Unsubscribe SUBSCRIBER-NAME from TOPIC.

This removes CALLBACK from the list of subscribers to TOPIC.

Note that, in general, only named callbacks may be unsubscribed,
as anonymous lambdas cannot easily be identified for removal."
  (puthash topic
           (remove subscriber-name
                   (gethash topic pubsub-board))
           pubsub-board))

(defun pubsub-subscribers (topic)
  "List subscribers to TOPIC."
  (gethash topic pubsub-board))


(provide 'pubsub)
;;; pubsub.el ends here

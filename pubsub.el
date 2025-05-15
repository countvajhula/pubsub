;;; pubsub.el --- Pubsubs, not macros! -*- lexical-binding: t -*-

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

;; Pubsubs, not macros!

;;; Code:

(defvar pubsub-board
  (make-hash-table)
  "All topics and lists of their subscribers.")

(defun pubsub-publish (topic notice)
  "Publish NOTICE to TOPIC.

This notifies each subscriber to TOPIC of the fresh NOTICE.

The notification is performed as a simple function invocation, where
each subscriber function (callback) to TOPIC is invoked with the fresh
NOTICE as the only argument."
  (dolist (subscriber (gethash topic pubsub-board))
    (funcall subscriber notice)))

(defun pubsub-subscribe (topic callback)
  "Subscribe to TOPIC.

This adds CALLBACK to the list of subscribers to TOPIC.

CALLBACK must be a function accepting a single argument.  It will be
invoked with each fresh notice on TOPIC."
  (puthash topic
           (cons callback
                 (gethash topic pubsub-board))
           pubsub-board))

(provide 'pubsub)
;;; pubsub.el ends here

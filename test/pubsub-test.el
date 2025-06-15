;; Note: we want to retain dynamic binding for these tests because the
;; ERT "fixtures" rely on it.

;; To run the tests from within Emacs, you must `eval-buffer` this test
;; buffer first. Then, run tests using `ert-run-tests-interactively`.
;; But, to avoid having to evaluate the changes (which may affect the live
;; environment), it may be preferable to `make test` at the shell, instead.

;; Notes:
;; - If you see "lisp nesting exceeds max-lisp-eval-depth"
;;   while running these tests, it could be that you have a duplicate
;;   "body" invocation within one of the nested fixtures. Since these
;;   are dynamically bound, every fixture needs to have a distinct
;;   name for the body argument.
;; - If you see errors like "(void-function t)", "(void-function nil)"
;;   and "invalid function nil . 0"
;;   then you probably are using a fixture without wrapping the body
;;   in a lambda

;; Add source paths to load path so the tests can find the source files
;; Adapted from:
;; https://github.com/Lindydancer/cmake-font-lock/blob/47687b6ccd0e244691fb5907aaba609e5a42d787/test/cmake-font-lock-test-setup.el#L20-L27
(defvar pubsub-test-setup-directory
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

(dolist (dir '("." ".."))
  (add-to-list 'load-path
               (concat pubsub-test-setup-directory dir)))

;;

(require 'pubsub)

(require 'cl-lib)

;;
;; Fixtures
;;


;; fixture recipe from:
;; https://www.gnu.org/software/emacs/manual/html_node/ert/Fixtures-and-Test-Suites.html


(defmacro with-fixture (fixture &rest test)
  "Run TEST using FIXTURE."
  (declare (indent 1))
  `(,fixture
    (lambda ()
      ,@test)))

(defun fixture-empty-board (body)
  (let ((pubsub-board (make-hash-table))
        (pubsub-subscriber-directory (make-hash-table)))
    (unwind-protect
        (funcall body)
      ;; perhaps aid garbage collection
      (setq pubsub-board nil))))

(defconst fixture-topic-1 "topic-1")

(defconst fixture-topic-2 "topic-2")

(defconst fixture-subscriber-name "hks")

(defconst fixture-subscriber-name-2 "vjs")

(defconst fixture-subscriber-with-error-name "oops")

(defun fixture-subscriber-callback (notice)
  notice)

(defun fixture-single-subscriber (body)
  (let* ((result nil)
         (callback (lambda (notice)
                     (setq result notice))))
    (pubsub-subscribe fixture-topic-1
                      fixture-subscriber-name
                      callback)
    (funcall body)))

(defun fixture-single-subscriber-with-error (body)
  (let* ((result nil)
         (callback (lambda (notice)
                     (error "whoopsies")
                     (setq result notice))))
    (pubsub-subscribe fixture-topic-1
                      fixture-subscriber-with-error-name
                      callback)
    (let ((output-buffer (generate-new-buffer "*captured messages*")))
      (unwind-protect
          ;; TODO: this doesn't work to suppress output
          (with-output-to-temp-buffer output-buffer
            (funcall body))
        (kill-buffer output-buffer)))))

(defun fixture-many-subscribers-to-one-topic (body)
  (let* ((result nil)
         (result2 nil)
         (callback (lambda (notice)
                     (setq result notice)))
         (callback2 (lambda (notice)
                      (setq result2 notice))))
    (pubsub-subscribe fixture-topic-1
                      fixture-subscriber-name
                      callback)
    (pubsub-subscribe fixture-topic-1
                      fixture-subscriber-name-2
                      callback2)
    (funcall body)))

(defun fixture-many-subscribers-to-many-topics (body)
  (let* ((result nil)
         (result2 nil)
         (callback (lambda (notice)
                     (setq result notice)))
         (callback2 (lambda (notice)
                      (setq result2 notice))))
    (pubsub-subscribe fixture-topic-1
                      fixture-subscriber-name
                      callback)
    (pubsub-subscribe fixture-topic-2
                      fixture-subscriber-name-2
                      callback2)
    (funcall body)))

;;
;; Tests
;;

(ert-deftest publish-test ()

  ;; publishing should notify subscriber
  (with-fixture fixture-empty-board
    (with-fixture fixture-single-subscriber
      (pubsub-publish fixture-topic-1 "hi")
      (should (equal "hi" result))))

  ;; publishing should notify all subscribers
  (with-fixture fixture-empty-board
    (with-fixture fixture-many-subscribers-to-one-topic
      (pubsub-publish fixture-topic-1 "hi")
      (should (equal "hi" result))
      (should (equal "hi" result2))))

  ;; publishing should not notify subscribers to other topics
  (with-fixture fixture-empty-board
    (with-fixture fixture-many-subscribers-to-many-topics
      (pubsub-publish fixture-topic-2 "hi")
      (should-not (equal "hi" result))))

  (with-fixture fixture-empty-board
    (with-fixture fixture-single-subscriber-with-error
      (pubsub-publish fixture-topic-1 "hi")
      (should-not (equal "hi" result))
      (should-not (member fixture-subscriber-with-error-name
                          (gethash fixture-topic-1 pubsub-board))))))

(ert-deftest subscribe-test ()

  ;; subscribing creates a directory entry
  (with-fixture fixture-empty-board
    (pubsub-subscribe fixture-topic-1
                      fixture-subscriber-name
                      #'fixture-subscriber-callback)
    (should (gethash fixture-subscriber-name pubsub-subscriber-directory)))

  ;; subscribing adds subscriber to topic by name
  (with-fixture fixture-empty-board
    (pubsub-subscribe fixture-topic-1
                      fixture-subscriber-name
                      #'fixture-subscriber-callback)
    (should (member fixture-subscriber-name
                    (gethash fixture-topic-1 pubsub-board))))

  ;; subscriber isn't subscribed more than once
  (with-fixture fixture-empty-board
                (pubsub-subscribe fixture-topic-1
                                  fixture-subscriber-name
                                  #'fixture-subscriber-callback)
                (pubsub-subscribe fixture-topic-1
                                  fixture-subscriber-name
                                  #'fixture-subscriber-callback)
                (puthash fixture-topic-1
                         (cl-remove fixture-subscriber-name
                                    (gethash fixture-topic-1 pubsub-board)
                                    :count 1)
                         pubsub-board)
                (should-not (member fixture-subscriber-name
                                    (gethash fixture-topic-1 pubsub-board)))))

(ert-deftest unsubscribe-test ()

  ;; unsubscribe removes subscriber name from topic
  (with-fixture fixture-empty-board
    (with-fixture fixture-single-subscriber
      (pubsub-unsubscribe fixture-topic-1 fixture-subscriber-name)
      (should-not (member fixture-subscriber-name
                          (gethash fixture-topic-1 pubsub-board))))))

(ert-deftest subscribers-test ()

  (with-fixture fixture-empty-board
    (with-fixture fixture-many-subscribers-to-one-topic
      (should (member "vjs" (pubsub-subscribers fixture-topic-1)))
      (should (member "hks" (pubsub-subscribers fixture-topic-1))))))

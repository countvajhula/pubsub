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
  (let ((pubsub-board (make-hash-table)))
    (unwind-protect
        (funcall body)
      ;; perhaps aid garbage collection
      (setq pubsub-board nil))))

(defvar fixture-topic-1 "topic-1")

(defvar fixture-topic-2 "topic-2")

(defun fixture-single-subscriber (body)
  (let* ((result nil)
         (subscriber (lambda (notice)
                       (setq result notice))))
    (pubsub-subscribe fixture-topic-1 subscriber)
    (funcall body)))

(defun fixture-many-subscribers-to-one-topic (body)
  (let* ((result nil)
         (result2 nil)
         (subscriber (lambda (notice)
                       (setq result notice)))
         (subscriber2 (lambda (notice)
                        (setq result2 notice))))
    (pubsub-subscribe fixture-topic-1 subscriber)
    (pubsub-subscribe fixture-topic-1 subscriber2)
    (funcall body)))

(defun fixture-many-subscribers-to-many-topics (body)
  (let* ((result nil)
         (result2 nil)
         (subscriber (lambda (notice)
                       (setq result notice)))
         (subscriber2 (lambda (notice)
                        (setq result2 notice))))
    (pubsub-subscribe fixture-topic-1 subscriber)
    (pubsub-subscribe fixture-topic-2 subscriber2)
    (funcall body)))

(defun fixture-subscriber (notice)
  notice)

(defun fixture-named-subscriber (body)
  (pubsub-subscribe fixture-topic-1 #'fixture-subscriber)
  (funcall body))

;;
;; Tests
;;

(ert-deftest pubsub-test ()

  (with-fixture fixture-empty-board
    (with-fixture fixture-single-subscriber
      (pubsub-publish fixture-topic-1 "hi")
      (should (equal "hi" result))))

  (with-fixture fixture-empty-board
    (with-fixture fixture-many-subscribers-to-one-topic
      (pubsub-publish fixture-topic-1 "hi")
      (should (equal "hi" result))
      (should (equal "hi" result2))))

  (with-fixture fixture-empty-board
    (with-fixture fixture-many-subscribers-to-many-topics
      (pubsub-publish fixture-topic-1 "hi")
      (should (equal "hi" result))
      (should-not (equal "hi" result2))))

  (with-fixture fixture-empty-board
    (with-fixture fixture-many-subscribers-to-many-topics
      (pubsub-publish fixture-topic-2 "hi")
      (should-not (equal "hi" result))
      (should (equal "hi" result2))))

  (with-fixture fixture-empty-board
    (with-fixture fixture-named-subscriber
      (should (member #'fixture-subscriber
                      (gethash fixture-topic-1 pubsub-board))))))

(ert-deftest unsubscribe-test ()

  (with-fixture fixture-empty-board
    (with-fixture fixture-named-subscriber
      (pubsub-unsubscribe fixture-topic-1 #'fixture-subscriber)
      (should-not (member #'fixture-subscriber
                          (gethash fixture-topic-1 pubsub-board))))))


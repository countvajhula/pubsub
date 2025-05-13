;; Load this file before running tests and it will generate coverage
;; data and also upload it to your coverage provider if one is
;; configured.
(when (require 'undercover nil t)
  (undercover))

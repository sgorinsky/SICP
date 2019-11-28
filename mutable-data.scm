#lang scheme

;; a package that imports mutable primitives
;; mcons, mcar, mcdr, set-mcar!, set-mcdr!, mlist
(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(define a (mcons 1 2))
a

(set-cdr! a 1)
a
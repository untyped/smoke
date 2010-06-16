#lang setup/infotab

(define name "smoke")

(define blurb 
  '("Object oriented web application UIs."))

(define release-notes
  '((p "Changes and additions:")
    (ul (li "Initial release."))))

(define primary-file
  "smoke.ss")

(define url "http://svn.untyped.com/smoke/")

;(define scribblings '(("scribblings/smoke.scrbl" (multi-page))))

(define categories '(devtools net ui))

(define required-core-version "4.2")

(define repositories '("4.x"))

(define compile-omit-files
  '("autoplanet.ss"
    "build.ss"
    "planet"
    "planetdev"))

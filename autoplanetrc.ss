#lang scheme

; ------------------------------------------------
;
; Smoke autoplanet script.
;
; Usage: mzscheme autoplanetrc.ss
;
; This module sets PLaneT development links so you can run the Smoke test code.
;
; First make sure you've removed any existing development links for Untyped packages,
; either by using the planet command line tool or by uncommenting the line below.
;
; Dependent packages will be checked out of the Untyped SVN to .autoplanet/svn/blah
;
; Development links will be added for Smoke and its dependencies.
;
; ------------------------------------------------

(require scheme/runtime-path
         (planet untyped/autoplanet:1))

; Remove ALL existing development links:
; (remove-hard-links)

; The SVN server to fetch packages from:
(define OPEN-URL "http://svn.untyped.com")

; Temporarily check packages out of SVN to here:
(autoplanet-root (build-path (current-directory) ".autoplanet"))

; The packages to fetch and some fake version numbers
; to ensure we prefer them over other minor revisions:
(define packages
  '(("untyped" "unlib"        3 99 "trunk")
    ("untyped" "mirrors"      2 99 "trunk")
    ("untyped" "delirium"     3 99 "trunk")
    ("untyped" "dispatch"     3 99 "branches/bridge")))

; Install PLaneT development links for these packages:
(for ([package (in-list packages)])
  (match package
    [(list owner package major minor branch)
     (install-svn owner
                  (format "~a.plt" package)
                  major
                  minor
                  (format "~a/~a/~a/src" OPEN-URL package branch)
                  'head)]))

; Install a PLaneT development link for Smoke:
(install-local "untyped" "smoke.plt" 1 0 (current-directory))

; After running this file,inspect your PLaneT packages with the command line tool:
;
;   planet show

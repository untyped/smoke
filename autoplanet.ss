#lang scheme

(require scheme/runtime-path
         (planet untyped/autoplanet:1))

(define-runtime-path dev-path
  "planetdev")

(remove-hard-links)
(install-local "untyped" "unlib.plt"        3 99 (build-path dev-path "unlib"))
(install-local "untyped" "mirrors.plt"      2 99 (build-path dev-path "mirrors"))
(install-local "untyped" "delirium.plt"     3 99 (build-path dev-path "delirium"))
(install-local "untyped" "dispatch.plt"     3 99 (build-path dev-path "dispatch"))

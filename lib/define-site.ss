#lang scheme

(require "../lib-base.ss")

(require (for-syntax scheme/provide-transform
                     syntax/parse
                     (planet untyped/unlib:3/syntax)
                     "site-info.ss")
         web-server/dispatchers/dispatch
         "dispatch/core.ss"
         "page.ss"
         "site.ss")

(define-syntax (define-site complete-stx)
  (syntax-parse complete-stx
    [(_ (~describe "id"
          (~and site-id:id
                (~bind [site-private-id (datum->syntax #f (syntax->datum #'id))])))
        (~describe "class" class%)
        ((~describe "dispatch rule" 
           [(~describe "url pattern" (part ...))
            (~describe "page id"
              (~and page-id:id
                    (~bind [page-private-id (make-id #f #'page-id '-private)])
                    (~bind [page-box-id     (make-id #f #'page-id '-box)])))])
         ...))
     
     (syntax/loc complete-stx
       (begin
         
         (define page-private-id (new undefined-page% [id 'page-id]))
         ...
         
         (define page-box-id
           (box page-private-id))
         ...
         
         (define-syntax (page-id stx)
           (let ([certify (syntax-local-certifier #t)])
             (page-info-add!
              (make-page-info
               (certify #'page-id)
               (certify #'page-box-id)))))
         ...
         
         (define site-private-id
           (new class% 
                [component-id 'site-id]
                [rules      (list (make-rule (create-pattern part ...) page-box-id) ...)]
                [page-boxes (list page-box-id ...)]))
         
         (define-syntax site-id
           (let ([certify (syntax-local-certifier #t)])
             (site-info-add!
              (make-site-info
               (certify #'site-id)
               (certify #'site-private-id)
               (list (certify #'page-id) ...)))))))]))

; (_ id)
(define-syntax site-out
  (make-provide-transformer
   (lambda (stx modes)
     ; syntax -> export
     (define (create-export id-stx)
       (make-export id-stx (syntax->datum id-stx) 0 #f id-stx))
     ; (listof export)
     (syntax-case stx ()
       [(_ id)
        (let ([info (site-info-ref #'id)])
          (map create-export (list* (site-info-id info)
                                    (site-info-page-ids info))))]))))

; Provide statements -----------------------------

(provide define-site
         site-out)
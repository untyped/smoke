#lang scheme

(require "../lib-base.ss")

(require (for-syntax scheme/list
                     scheme/provide-transform
                     syntax/parse
                     (planet cce/scheme:6/syntax)
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
            (~describe "page id" rule-page-id:id)])
         ...))
     (with-syntax* ([([page-id page-private-id page-box-id] ...)
                     (for/list ([page-id-stx (in-list (remove-duplicates (syntax->list #'(rule-page-id ...))
                                                                         symbolic-identifier=?))])
                       (list page-id-stx
                             (make-id #f page-id-stx '-private)
                             (make-id #f page-id-stx '-box)))]
                    [(rule-page-box-id ...)
                     (for/list ([rule-page-id-stx (in-list (syntax->list #'(rule-page-id ...)))])
                       (or (for/or ([page-id-stx      (in-list (syntax->list #'(page-id ...)))]
                                [page-box-id-stx  (in-list (syntax->list #'(page-box-id ...)))])
                             (and (symbolic-identifier=? rule-page-id-stx page-id-stx)
                                  page-box-id-stx))
                           (raise-syntax-error #f "internal badness" complete-stx #'page-id-stx)))])
       (syntax/loc complete-stx
         (begin
           
           (define page-private-id (new undefined-page% [component-id 'page-id]))
           ...
           
           (define page-box-id
             (box page-private-id))
           ...
           
           (define-syntax page-id
             (let ([certify (syntax-local-certifier #t)])
               (page-info-add!
                (make-page-info
                 (certify #'page-id)
                 (certify #'page-box-id)))))
           ...
           
           (define site-private-id
             (new class% 
                  [component-id 'site-id]
                  [rules      (list (make-rule (create-pattern part ...) rule-page-box-id) ...)]
                  [page-boxes (list page-box-id ...)]))
           
           (define-syntax site-id
             (let ([certify (syntax-local-certifier #t)])
               (site-info-add!
                (make-site-info
                 (certify #'site-id)
                 (certify #'site-private-id)
                 (list (certify #'page-id) ...))))))))]))

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
#lang scheme

(require "../lib-base.ss")

(require (for-syntax scheme/list
                     scheme/provide-transform
                     syntax/parse
                     (planet cce/scheme:6/syntax)
                     (planet untyped/unlib:3/debug)
                     (planet untyped/unlib:3/syntax)
                     "site-info.ss")
         web-server/dispatchers/dispatch
         "dispatch/core.ss"
         "page.ss"
         "site.ss")

(define-syntax (define-site complete-stx)
  (syntax-parse complete-stx
    [(_ (~describe "id" site-id:id)
        (~describe "class" class%)
        (~describe "dispatch rules"
                   ((~describe "dispatch rule"
                               [(~describe "url pattern" (part ...))
                                (~describe "page id" rule-page-id:id)]) ...))
        (~or (~optional (~seq #:other-pages (other-page-id:id ...)) #:name "#:other-pages keyword"))
        ...)
     (with-syntax* ([site-private-id     (datum->syntax #f (syntax->datum #'id))]
                    [all-page-ids        (remove-duplicates
                                          (append (syntax->list #'(rule-page-id ...))
                                                  (syntax->list (or (attribute other-page-id) #'())))
                                          symbolic-identifier=?)]
                    [([page-id page-private-id page-box-id] ...)
                     `(,@(for/list ([page-id-stx (in-list (syntax->list #'all-page-ids))])
                           (list page-id-stx
                                 (make-id #f page-id-stx '-private)
                                 (make-id #f page-id-stx '-box))))]
                    [(rule-page-box-id ...)
                     (for/list ([rule-page-id-stx (in-list (syntax->list #'(rule-page-id ...)))])
                       (or (for/or ([page-id-stx      (in-list (syntax->list #'(page-id ...)))]
                                    [page-box-id-stx  (in-list (syntax->list #'(page-box-id ...)))])
                             (and (symbolic-identifier=? rule-page-id-stx page-id-stx)
                                  page-box-id-stx))
                           (raise-syntax-error #f "internal badness" complete-stx #'page-id-stx)))])
       (syntax/loc complete-stx
         (begin
           
           (define page-private-id
             (new undefined-page% [component-id 'page-id]))
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
             (let ([ans (new class% 
                             [component-id 'site-id]
                             [rules      (list (make-rule (create-pattern part ...) rule-page-box-id) ...)]
                             [page-boxes (list page-box-id ...)])])
               (send page-private-id set-site! ans)
               ...
               ans))
           
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
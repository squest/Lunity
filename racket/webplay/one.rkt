#lang racket

(require web-server/servlet
         web-server/servlet-env
         web-server/templates
         web-server/web-server
         web-server/dispatch)

(define-syntax-rule (fn binding body)
  (lambda binding body))

(define-syntax-rule (def fname body)
  (define fname body))

(define (app req)
  (response/full
   200 #"OK" (current-seconds) TEXT/HTML-MIME-TYPE
   (list (make-header #"Location"
                      #"http://localhost:3000"))
   (list
          (let [(name "John Paul")
                (age "Cukup uzur")
                (x (fn () "x"))]
            (include-template "resources/main-pages/base.html")))))

(serve/servlet app
               #:launch-browser? true
               #:port 3000
               #:servlet-path "/one.rkt"
               #:extra-files-paths 
               (list (build-path "resources/")))



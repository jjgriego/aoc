#lang racket

(struct fs-ent (name size) #:transparent #:mutable)
(struct fs-dir fs-ent (contents) #:transparent #:mutable)
(struct fs-file fs-ent () #:transparent #:mutable)

(define (fs-get-or-create-entry dir name create-thunk)
  (match dir
    [(fs-dir name size contents)
     (or
      (findf (lambda (ent)
               (equal? name (fs-ent-name ent)))
             contents)
      (let ([e (create-thunk)])
        (set-fs-dir-contents! dir (cons e contents))
        e))]))

(define (fs-get-or-create-child-dir d name)
  (fs-get-or-create-entry d name (lambda () (fs-dir name #f '()))))
(define (fs-get-or-create-file d name size)
  (fs-get-or-create-entry d name (lambda () (fs-file name size))))

(define (fs-calculate-size ent)
  (match ent
    [(fs-dir _ #f contents)
     (define result (for/sum ([ent contents])
                      (fs-calculate-size ent)))
     (set-fs-ent-size! ent result)
     result]
    [(fs-file _ size)
     size]
    [(? fs-ent?)
     (fs-ent-size ent)]))

(define (fs-fold ent file->result dir->result)
  (cond
    [(fs-file? ent)
     (file->result ent)]
    [(fs-dir? ent)
     (dir->result ent (map (lambda (e) (fs-fold e file->result dir->result)) (fs-dir-contents ent)))]))

(define filesystem
  (parameterize ([current-input-port (open-input-file "../inputs/07")])
    (define root (fs-dir "/" #f '()))
    (let loop ([current-dirs (list)])
      (match (read-line)
        [(? eof-object?) (void)]
        ;; special case for root dir
        ["$ cd /" (loop (list root))]
        ["$ ls"
         (let listing-loop ()
           (define c (peek-char))
           (unless (or (equal? #\$ c) (eof-object? c))
             (match (string-split (read-line))
               [(list "dir" name)
                (fs-get-or-create-child-dir (car current-dirs) name)]
               [(list size-str name)
                (fs-get-or-create-file (car current-dirs) name (string->number size-str))])
             (listing-loop)))
         (loop current-dirs)]
        ["$ cd .."
         (loop (cdr current-dirs))]
        [(regexp #rx"\\$ cd (.*)$" (list _ name))
         (loop (cons (fs-get-or-create-child-dir (car current-dirs)
                                                 name)
                     current-dirs))]))
    root))

(fs-calculate-size filesystem)

(define disk-size 70000000)
(define needed-space 30000000)
(define used-space (fs-ent-size filesystem))
(define minimum-candidate-size (- needed-space (- disk-size used-space)))

#;(fs-fold filesystem
         (lambda (f) 0)
         (lambda (d entries)
           (define dir-weight (if (<= (fs-ent-size d) 100000)
                                  (fs-ent-size d)
                                  0))
           (apply + dir-weight entries)))

(define all-dirs (fs-fold filesystem
                          (lambda (f) '())
                          (lambda (d entries)
                            (cons d (append* entries)))))

(fs-ent-size (argmin fs-ent-size
                     (filter (lambda (d)
                               (>= (fs-ent-size d) minimum-candidate-size))
                             all-dirs)))

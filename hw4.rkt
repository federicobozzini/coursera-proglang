
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence l h s)
  (if (> l h)
      null
      (cons l (sequence(+ l s) h s))))

(define (string-append-map xs sfx)
  (map (lambda (x) (string-append x sfx)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        ;;[(null? xs) (error (number->string n))]
        [(= n 0) (car xs)]
        [(>= n (length xs)) (list-nth-mod xs (remainder n (length xs)))]
        [#t (list-nth-mod (cdr xs) (- n 1))]))
  

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define funny-number-stream 
  (letrec(
      [check (lambda(x) (if (= (remainder x 5) 0) (- x) x))]
      [f (lambda (x) (cons (check x) (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define dan-then-dog 
  (letrec(
      [check (lambda(x) (if (= (remainder x 2) 1) "dan.jpg" "dog.jpg"))]
      [f (lambda (x) (cons (check x) (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define (stream-add-zero s) 
  (lambda ()
    (cons (cons 0 (car (s))) (stream-add-zero (cdr (s))))))

(define (cycle-lists xs ys) 
  (letrec(
      [f (lambda (x) 
           (cons 
            (cons(list-nth-mod xs x) (list-nth-mod ys x)) 
            (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([l (vector-length vec)]
           [check (lambda (i) 
                    (if (or (= l 0 ) (>= i l))
                        #f
                        (let ([el (vector-ref vec i)])
                          (if (and (pair? el) (equal? v (car el)))
                              el
                              (check (+ i 1))))))])
    (check 0)))
  
(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)])
    (lambda(v) 
      (let* ([i 0]
            [read-cache (lambda (val) (vector-assoc val memo))]
            [write-cache (lambda (val) 
                           (begin
                             (vector-set! memo i val)
                             (set! i (remainder (+ i 1) (vector-length memo)))))]
            [el (read-cache v)])                    
        (if el 
            el
            (let* ([new-el (assoc v xs)])
              (begin
                (write-cache new-el)
                new-el)))))))

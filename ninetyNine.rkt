(define my-last
  (lambda (n)
    (cond ((null? (cdr n)) (car n))
          (else (my-last (cdr n))))))

(define my-but-last
  (lambda (n)
    (cond ((null? (cdr (cdr n)))  n)
          (else (my-but-last (cdr n))))))


(define element-at
  (lambda (n m)
    (cond  ((null? n) null )
           ((zero? (- m 1)) (car n))
           (else(element-at (cdr n) (- m 1))))))



(define posición
  (lambda (n  m)
    (cond( (equal? m (car n))  1)
         (else (+(posición (cdr n) m) 1)))))



(define reverse-aux
  (lambda (n m)
    (cond ((null? n) m)
          (else(reverse-aux (cdr n) (cons (car n) m))))))


(define reverse
  (lambda (l)
    (reverse-aux l '())))


(define palindromo?
  (lambda (l)
    (equal? l (reverse l))))



(define lista-plana
  (lambda (l)
    (cond  ((null? l) '())
           ((list? (car l)) (append (lista-plana (car l)) (lista-plana (cdr l))))
           (else ( cons (car l) (lista-plana (cdr l)))))))


(define compress
  (lambda (l)
    (cond ((null? (cdr l)) l)
          ((equal? (car l) (car(cdr l))) (compress (cdr l)))  
          (else (cons (car l) (compress (cdr l)))))))


(define pack-aux
  (lambda (l m)
    (cond ((null? (cdr l)) (cons(cons (car l) m ) '()))
          ((equal? (car l) (car (cdr l))) (pack-aux (cdr l) (cons (car l) m))) 
          (else ( cons (cons (car l) m) (pack-aux (cdr l) '()))))))

(define pack
  (lambda (n)
    (pack-aux n '()))) 


(define longitud
  (lambda (l)
    (cond ((null? l) 0)
          (else (+ 1 (longitud (cdr l)))))))

(define encode-aux
  (lambda (l n)
    (cond ((null? l) '())
          (else( cons (cons (longitud (car l)) (cons (car(car l)) n)) (encode-aux (cdr  l) '()))))))

  
          
(define encode
  (lambda (l)
    (encode-aux(pack l)'()))) 

(define encode-aux-mod
  (lambda (l n)
    (cond ((null? l) '())
          (else(cond((equal? (longitud (car l)) 1)   (cons (car(car l)) (encode-aux-mod (cdr  l) '()))    )
                    (else (cons (cons (longitud (car l)) (cons (car(car l)) n)) (encode-aux-mod (cdr  l) '()))))))))


(define encode-mod
  (lambda (l)
    (encode-aux-mod(pack l)'()))) 
          
(define descompresor-aux
   (lambda (l)
     (cond ((null? l) '())
           ((zero? (car(car l)))  (descompresor-aux (cdr l) )) 
           (else( append (cons (car(cdr(car l))) '()) (descompresor-aux (cons (cons(- (car(car l)) 1) (cdr (car l))) '()) ))))))


(define descompresor
   (lambda (l)
     (cond((null? l) l)
      (else(append (descompresor-aux l) (descompresor (cdr l)))))))
     

(define dupli-aux
  (lambda (l n)
    (cond ((null? (cdr l))(cons (car l) l))
          (else ( append (cons n (cons n '()))  (dupli-aux (cdr l) (car (cdr l))) ))))) 
 
(define dupli
  (lambda (l)
    (dupli-aux l (car l))))

(define repli-aux
  (lambda (l  m n)
    (cond ((null? l) '())
           ((zero? n)  (repli-aux (cdr l) m m)) 
           (else( cons (car l) (repli-aux l m (- n 1)))))))

(define repli
  (lambda (l n)
    (repli-aux l n n)))


(define drop-aux
  (lambda (l n m)
    (cond ((null? l) l)
          ((equal? n 1) (cons (cadr l) (drop-aux (cddr l) (- m 1) m)))
          (else (cons (car l) (drop-aux (cdr l) (- n 1) m))))))

(define drop
  (lambda (l n)
    (drop-aux l n n)))


(define split
  (lambda (l n)
     (cond ((null? l) (cons '()(cons  l '())) )
          ((zero? n) (cons '()(cons  l '())) )
          (else ( cons (cons (car l) (car (split (cdr l) (- n 1)))) (cdr(split (cdr l) (- n 1))))))))


(define split-max
  (lambda (l n m)
     (cond ((null? l) l)
          ((equal? m 1)  (cons(car l) '()) )
          ((and (equal? n 1) (not (zero? m)))  (cons (car l) (split-max (cdr l) n (- m 1)))  )
          (else ( split-max(cdr l) (- n 1)(- m 1))))))


(define rotate
  (lambda (l n)
    (cond ((null? l) l)
          (else( append (cdr(split l n)) (car(split l n)))))))


(define remove-at
  (lambda (l n)
    (cond ((null? l) l)
          ((equal? n 1) (cdr l) )
          (else (cons (car l) (remove-at (cdr l) (- n 1)))))))


(define insert-at
  (lambda (p l n)
     (cond ((null? l) l)
          (else( append (car(split l n)) (cons p (cdr(split l n))))))))

(define range
  (lambda (n m)
    (cond ((>= n  m) (cons n'()))
          (else(cons n (range (+ n 1) m))))))


(define rnd-select
  (lambda (l n)
    (rnd-select-aux l n (+ (random  (longitud l)) 1))))

(define rnd-select-aux
  (lambda (l n m)
    (cond((null? l) '())
         ((equal? n 1) (list(element-at l (+(random  (longitud l) ) 1))))
         (else(cons (element-at l m) (rnd-select-aux(remove-at l m) (- n 1) (+ (random (- (longitud l) 1)) 1)))))))

(define lotto-select
  (lambda (n m)
    (cond((= n 1) (list (random m)))
         (else ( cons (random m) (lotto-select (- n 1) m))))))


(define rnd-permu
  (lambda (l)
    (rnd-select l (longitud l))))



(define combination
  (lambda (n l)
    (combination-aux n l l l)))

(define combination-aux
  (lambda (n l a p)
    (cond ((and (< (longitud p) n))'())
          ((< (longitud a) n)(combination-aux n (cdr p) (cdr p) (cdr p) ))
          ((< (longitud l) n) (combination-aux n (remove-at  a 2)  (remove-at  a 2) p ))
          (else ( cons (car(split l n)) (combination-aux n (remove-at l n) a p))))))         



      
(define group3-aux
  (lambda (l)
   (append (combination 2 l) (combination 3 l) (combination 4 l))))



(define lsort
  (lambda (l)
    (lsort-aux l '())))

(define lsort-aux
  (lambda (l m)
    (cond ((null? l) m)
          ((null? m) (lsort-aux (cdr l) (cons (car l) m)))
          ( (<= (length (car l)) (length (car m))) (lsort-aux (cdr l) (append (list(car l)) m)))
          (else (lsort-aux (cdr l) (append m (list(car l))  ))))))

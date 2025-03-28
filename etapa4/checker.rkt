#lang racket

(require "etapa4.rkt")

; ignorați următoarele linii de cod...
(define show-defaults 999) ; câte exerciții la care s-au întors rezultate default să fie arătate detaliat
(define prepend #t) (define nopoints #f) (define name-ex '(testul testele trecut capitolul))
(define default-results `(#f 0 () your-code-here)) (define (default-result r) (set! default-results (cons r default-results))) (define : 'separator) (define punct 'string) (define puncte 'string) (define BONUS 'string) (define exerciții 'string)
(define total 0) (define all '()) (define n-ex 0) (define p-ex 0) (define n-exercs -1) (define default-returns '()) (define (ex n sep p . s) (set! n-ex n) (set! p-ex p) (set! all (cons (list n p) all))) (define exercițiul ex) (define (sunt n s) (set! n-exercs n)) (define s-a string-append)
(define (p . L) (map (λ (e) (display e) (when (> (string-length (format "~a" e)) 0) (display " "))) L) (newline)) (define (p-n-ex) (format "[~a]" (if nopoints (string-join (list (symbol->string (cadddr name-ex)) (number->string n-ex) "/" (number->string n-exercs))) n-ex)))
(define (epart ep% pfix full) (if (< (caddr ep%) 1) (s-a pfix (if full "" (s-a (symbol->string (car name-ex)) " ")) (if (and nopoints (not full)) "" (number->string n-ex)) (symbol->string (cadr ep%))) (if (and nopoints (not full)) "" (s-a pfix (if full "" (s-a (symbol->string (car name-ex)) " ")) (number->string n-ex)))))
(define (whengood ep%) (let [(pts (* p-ex (caddr ep%)))] (and (if prepend (printf "+~v: " pts) (printf "~a[OK] " (p-n-ex))) (if nopoints (p (epart ep% "" #f) "rezolvat") (p (epart ep% "" #f) "rezolvat: +" pts (if (= pts 1) 'punct 'puncte))) (set! total (+ total pts)))))
(define (whenbad ep% gvn expcd msg) (and (when (member gvn default-results) (set! default-returns (cons (epart ep% "" #t) default-returns))) (when (or (not (member gvn default-results)) (<= (length default-returns) show-defaults)) (bad-res ep% gvn expcd msg))))
(define (bad-res ep% gvn expcd msg) (p (if prepend "+0.0:" (format "~a[--]" (p-n-ex))) (epart ep% "la " #f) 'rezultatul gvn msg expcd))
(define (check-conds e gvn conds) (or (null? conds) (let ([r ((car conds) gvn)]) (if (eq? r #t) (check-conds e gvn (cdr conds)) (whenbad e gvn "" (or r "nu îndeplinește condițiile (completitudine, stabilitate)"))))))
(define (check-part part per given main-test expected . conds) (let* ([e (list n-ex part per)] [p? (pair? (cdr main-test))] [p (if p? (car main-test) identity)] [t ((if p? cadr car) main-test)] [m ((if p? cddr cdr) main-test)]) (when (eq? #t (check-conds e given conds)) (if (t (p given) expected) (whengood e) (whenbad e (p given) expected m)))))
(define (check given main-test expected . conds) (apply check-part '- 1 given main-test expected conds))
(define the cons) (define is (cons equal? "diferă de cel așteptat")) (define in (cons member "nu se află printre variantele așteptate"))
(define same-set-as (cons (λ (x y) (apply equal? (map list->seteqv (list x y)))) "nu este aceeași mulțime cu"))
(define same-unique (cons (λ (x y) (and (apply = (map length (list x y))) ((car same-set-as) x y))) "nu sunt aceleași rezultate cu"))
(define (sumar) (when (and (not (null? default-returns)) (< show-defaults (length default-returns))) (p "... rezultatul implicit dat la" (cadr name-ex) (reverse default-returns))) (when (not nopoints) (p 'total: (/ (floor (* total 100.)) 100) 'puncte)))
(define (mark-helper) (printf "---~nEx  puncte    Total până aici~n") (foldr (λ (e-p t) (p (car e-p) ': (cadr e-p) "puncte. total 1 -" (car e-p) ': (+ t (cadr e-p))) (+ t (cadr e-p))) 0 all) (newline))


; Definiții ajutătoare pentru checker.

; Stream infinit.
(define inf-stream
  (stream-cons '(a . 7)
               (stream-cons '(b . 8)
                            (stream-cons '(b . 9) inf-stream))))

; Test care ignoră comparația cu un rezultat exact, pentru a testa doar condiții.
(define just-conds (cons (lambda (_ __) #t) "this shouldn't happen?..."))

; Condiție care verifică dacă un stream răspuns are elementele cerute într-o referință.
; Față de o verificare directă, are avantajul că nu dă eroare dacă răspunsul nu este stream.
; stream-equals? : [T] -> (X -> True | Str)
(define (stream-equals? ref)
  (lambda (candidate)
    (if (not (stream? candidate))
        "nu e un stream, așa cum e cerut"
        (let ([candidate-list (stream->list candidate)])
          (or (equal? candidate-list ref)
              (format "are alte elemente decât cele cerute: ~s ~s" candidate-list ref))))))

; Verifică dacă răspunsurile exercițiilor 2 și 3 sunt corecte.
; Concret, verifică dacă un stream de liste are aceleași liste ca răspunsul corect.
; Din moment ce ordinea elementelor în liste nu contează, listele sunt tratate ca seturi.
; same-seq-of-sets: [[T]] -> (Stream<[T]> -> True | Str)
(define (same-seq-of-sets ref)
  (lambda (candidate)
    (if (not (stream? candidate))
        "nu e un stream, așa cum e cerut"
        (let* ([candidate-list (stream->list candidate)]
               [len-candidate  (length candidate-list)]
               [len-ref        (length ref)])
          (if (not (= len-candidate len-ref))
              (format "nu are numărul corect de elemente: are ~s, dar așteptam ~s" len-candidate len-ref)
              (let iter ([cc candidate-list] [rr ref] [i 1])
                (cond
                  [(null? cc) true]
                  [(equal? (list->set (car cc)) (list->set (car rr)))
                   (iter (cdr cc) (cdr rr) (add1 i))]
                  [else (format "are elemente greșite la al ~s-lea pas: am primit ~s dar așteptam ~s" i (car cc) (car rr))])))))))


; Testele încep de aici.
(sunt 3 exerciții)

(exercițiul 1 : 45 puncte)
(check-part 'a (/ 1 15) (add-rating '(a 0 (7) (7))  7)   is '(a 1 (7 (7)) (7)))
(check-part 'b (/ 1 15) (add-rating '(a 1 (4) ()) 7)   is '(a 0 (4) (7)))
(check-part 'c (/ 1 15) (add-rating '(a 1 (7) ()) 4)   is '(a 0 (4) (7)))
(check-part 'd (/ 1 15) (add-rating '(a 0 (4) (7)) 5)  is '(a 1 (5 (4)) (7)))
(check-part 'e (/ 1 15) (add-rating '(a 0 (4) (7)) 10) is '(a 1 (7 (4)) (10)))
(check-part 'f (/ 1 15) (add-rating '(a 0 (4) (7)) 1)  is '(a 1 (4 (1)) (7)))
(check-part 'g (/ 1 15) (add-rating '(dontgiveup 1 (2 (1)) (8)) 1) is '(dontgiveup 0 (1 (1)) (2 (8))))
(check-part 'h (/ 1 15) (add-rating '(boss 1 (7 (7) (2)) (8 (10))) 5) is '(boss 0 (7 (2) (5)) (7 (8 (10)))))
(check-part 'i (/ 1 15) (add-rating '(youcandoit 1 (3 (1) (2)) (4 (7))) 5) is '(youcandoit 0 (3 (1) (2)) (4 (5) (7))))
(check-part 'j (/ 1 15) (add-rating '(dontgiveup 1 (7 (4) (6 (2))) (10 (10 (10)))) 9) is '(dontgiveup 0 (7 (4) (6 (2))) (9 (10 (10 (10))))))
(check-part 'k (/ 1 15) (add-rating '(omg 0 (6 (2) (5 (2))) (8 (8 (10) (9)))) 3) is '(omg 1 (6 (3) (2) (5 (2))) (8 (8 (10) (9)))))
(check-part 'l (/ 1 15) (add-rating '(youcandoit 0 (4 (2) (4) (1)) (4 (5 (7) (10)))) 4) is '(youcandoit 1 (4 (4) (2) (4) (1)) (4 (5 (7) (10)))))
(check-part 'm (/ 1 15) (add-rating '(omg 0 (5 (2) (2) (3)) (5 (6) (6 (6)))) 1) is '(omg 1 (5 (1) (2) (2) (3)) (5 (6) (6 (6)))))
(check-part 'n (/ 1 15) (add-rating '(boss 1 (5 (5) (1) (2) (4)) (8 (9) (9) (10))) 6) is '(boss 0 (5 (5) (1) (2) (4)) (6 (8 (9) (9) (10)))))
(check-part 'o (/ 1 15) (add-rating '(dontgiveup 0 (5 (3 (1) (2) (2))) (5 (9) (9) (5 (7)))) 5) is '(dontgiveup 1 (5 (5) (3 (1) (2) (2))) (5 (9) (9) (5 (7)))))

(exercițiul 2 : 45 puncte)
(check-part 'a (/ 1 9) (reviews->quads empty-stream) is empty-stream)
(check-part 'b (/ 1 9) (stream-take (reviews->quads inf-stream) 5)
            just-conds 'nil (same-seq-of-sets '([(a 1 (7) ())]
                                                [(b 1 (8) ()) (a 1 (7) ())]
                                                [(b 0 (8) (9)) (a 1 (7) ())]
                                                [(b 0 (8) (9)) (a 0 (7) (7))]
                                                [(b 1 (8 (8)) (9)) (a 0 (7) (7))])))
(check-part 'c (/ 1 9) (reviews->quads (stream '(a . 4) '(a . 1))) just-conds 'nil (stream-equals? '([(a 1 (4) ())]
                                                                                                     [(a 0 (1) (4))])))
(check-part 'd (/ 1 9) (reviews->quads (stream '(a . 4) '(b . 1)))
            just-conds 'nil (same-seq-of-sets '([(a 1 (4) ())]
                                                [(a 1 (4) ()) (b 1 (1) ())]))) ; Ordinea din interiorul unui pas nu contează.
(check-part 'e (/ 1 9) (reviews->quads (stream '(a . 4) '(b . 1) '(a . 2)))
            just-conds 'nil (same-seq-of-sets '([(a 1 (4) ())]
                                                [(a 1 (4) ()) (b 1 (1) ())]
                                                [(a 0 (2) (4)) (b 1 (1) ())])))
(check-part 'f (/ 1 9) (reviews->quads (stream '(a . 4) '(b . 1) '(a . 2) '(c . 7) '(b . 3) '(c . 9)))
            just-conds 'nil (same-seq-of-sets '([(a 1 (4) ())]
                                                [(a 1 (4) ()) (b 1 (1) ())]
                                                [(a 0 (2) (4)) (b 1 (1) ())]
                                                [(a 0 (2) (4)) (b 1 (1) ()) (c 1 (7) ())]
                                                [(a 0 (2) (4)) (b 0 (1) (3)) (c 1 (7) ())]
                                                [(a 0 (2) (4)) (b 0 (1) (3)) (c 0 (7) (9))])))
(check-part 'g (/ 1 9) (reviews->quads (stream '(c . 6) '(c . 1) '(a . 3) '(b . 2) '(a . 1) '(a . 1) '(a . 2)))
            just-conds 'nil (same-seq-of-sets '([(c 1 (6) ())]
                                                [(c 0 (1) (6))]
                                                [(c 0 (1) (6)) (a 1 (3) ())]
                                                [(c 0 (1) (6)) (a 1 (3) ()) (b 1 (2) ())]
                                                [(c 0 (1) (6)) (a 0 (1) (3)) (b 1 (2) ())]
                                                [(c 0 (1) (6)) (a 1 (1 (1)) (3)) (b 1 (2) ())]
                                                [(c 0 (1) (6)) (a 0 (1 (1)) (2 (3))) (b 1 (2) ())])))
(check-part 'h (/ 1 9) (reviews->quads (stream '(d . 2) '(c . 9) '(d . 7) '(f . 4) '(b . 4) '(c . 6) '(d . 3) '(e . 2) '(e . 6) '(a . 3) '(f . 1)))
            just-conds 'nil (same-seq-of-sets '([(d 1 (2) ())]
                                                [(d 1 (2) ()) (c 1 (9) ())]
                                                [(d 0 (2) (7)) (c 1 (9) ())]
                                                [(d 0 (2) (7)) (c 1 (9) ()) (f 1 (4) ())]
                                                [(d 0 (2) (7)) (c 1 (9) ()) (f 1 (4) ()) (b 1 (4) ())]
                                                [(d 0 (2) (7)) (c 0 (6) (9)) (f 1 (4) ()) (b 1 (4) ())]
                                                [(d 1 (3 (2)) (7)) (c 0 (6) (9)) (f 1 (4) ()) (b 1 (4) ())]
                                                [(d 1 (3 (2)) (7)) (c 0 (6) (9)) (f 1 (4) ()) (b 1 (4) ()) (e 1 (2) ())]
                                                [(d 1 (3 (2)) (7)) (c 0 (6) (9)) (f 1 (4) ()) (b 1 (4) ()) (e 0 (2) (6))]
                                                [(d 1 (3 (2)) (7)) (c 0 (6) (9)) (f 1 (4) ()) (b 1 (4) ()) (e 0 (2) (6)) (a 1 (3) ())]
                                                [(d 1 (3 (2)) (7)) (c 0 (6) (9)) (f 0 (1) (4)) (b 1 (4) ()) (e 0 (2) (6)) (a 1 (3) ())])))
(check-part 'i (/ 1 9) (reviews->quads (stream '(b . 8) '(g . 4) '(h . 8) '(g . 4) '(d . 7) '(a . 4) '(g . 8) '(g . 6) '(c . 6) '(f . 3) '(d . 2) '(e . 5) '(a . 6) '(b . 4) '(f . 6) '(b . 3) '(d . 5)))
            just-conds 'nil (same-seq-of-sets '([(b 1 (8) ())]
                                                [(b 1 (8) ()) (g 1 (4) ())]
                                                [(b 1 (8) ()) (g 1 (4) ()) (h 1 (8) ())]
                                                [(b 1 (8) ()) (g 0 (4) (4)) (h 1 (8) ())]
                                                [(b 1 (8) ()) (g 0 (4) (4)) (h 1 (8) ()) (d 1 (7) ())]
                                                [(b 1 (8) ()) (g 0 (4) (4)) (h 1 (8) ()) (d 1 (7) ()) (a 1 (4) ())]
                                                [(b 1 (8) ()) (g 1 (4 (4)) (8)) (h 1 (8) ()) (d 1 (7) ()) (a 1 (4) ())]
                                                [(b 1 (8) ()) (g 0 (4 (4)) (6 (8))) (h 1 (8) ()) (d 1 (7) ()) (a 1 (4) ())]
                                                [(b 1 (8) ()) (g 0 (4 (4)) (6 (8))) (h 1 (8) ()) (d 1 (7) ()) (a 1 (4) ()) (c 1 (6) ())]
                                                [(b 1 (8) ()) (g 0 (4 (4)) (6 (8))) (h 1 (8) ()) (d 1 (7) ()) (a 1 (4) ()) (c 1 (6) ()) (f 1 (3) ())]
                                                [(b 1 (8) ()) (g 0 (4 (4)) (6 (8))) (h 1 (8) ()) (d 0 (2) (7)) (a 1 (4) ()) (c 1 (6) ()) (f 1 (3) ())]
                                                [(b 1 (8) ()) (g 0 (4 (4)) (6 (8))) (h 1 (8) ()) (d 0 (2) (7)) (a 1 (4) ()) (c 1 (6) ()) (f 1 (3) ()) (e 1 (5) ())]
                                                [(b 1 (8) ()) (g 0 (4 (4)) (6 (8))) (h 1 (8) ()) (d 0 (2) (7)) (a 0 (4) (6)) (c 1 (6) ()) (f 1 (3) ()) (e 1 (5) ())]
                                                [(b 0 (4) (8)) (g 0 (4 (4)) (6 (8))) (h 1 (8) ()) (d 0 (2) (7)) (a 0 (4) (6)) (c 1 (6) ()) (f 1 (3) ()) (e 1 (5) ())]
                                                [(b 0 (4) (8)) (g 0 (4 (4)) (6 (8))) (h 1 (8) ()) (d 0 (2) (7)) (a 0 (4) (6)) (c 1 (6) ()) (f 0 (3) (6)) (e 1 (5) ())]
                                                [(b 1 (4 (3)) (8)) (g 0 (4 (4)) (6 (8))) (h 1 (8) ()) (d 0 (2) (7)) (a 0 (4) (6)) (c 1 (6) ()) (f 0 (3) (6)) (e 1 (5) ())]
                                                [(b 1 (4 (3)) (8)) (g 0 (4 (4)) (6 (8))) (h 1 (8) ()) (d 1 (5 (2)) (7)) (a 0 (4) (6)) (c 1 (6) ()) (f 0 (3) (6)) (e 1 (5) ())])))


(exercițiul 3 : 30 puncte)
(check-part 'a (/ 1 5) (quads->medians empty-stream) is empty-stream)
(check-part 'b (/ 1 5) (quads->medians (stream '[(a 1 (4) ())])) just-conds 'nil (stream-equals? '([(a . 4)])))
(check-part 'c (/ 1 5) (quads->medians (stream '[(a 1 (4) ())]
                                               '[(a 1 (4) ()) (b 1 (1) ())]
                                               '[(a 0 (2) (4)) (b 1 (1) ())]))
            just-conds 'nil (same-seq-of-sets '([(a . 4)]
                                                [(a . 4) (b . 1)]
                                                [(a . 3) (b . 1)])))
(check-part 'd (/ 1 5) (quads->medians (stream '[(d 1 (3 (2)) (7)) (c 0 (6) (9)) (f 1 (4) ()) (b 1 (4) ()) (e 0 (2) (6)) (a 1 (3) ())]
                                               '[(d 1 (3 (2)) (7)) (c 0 (6) (9)) (f 0 (1) (4)) (b 1 (4) ()) (e 0 (2) (6)) (a 1 (3) ())]))
            just-conds 'nil (same-seq-of-sets '([(d . 3) (c . 15/2) (f . 4)   (b . 4) (e . 4) (a . 3)]
                                                [(d . 3) (c . 15/2) (f . 5/2) (b . 4) (e . 4) (a . 3)])))
(check-part 'e (/ 1 5) (quads->medians (stream '[(b 0 (4) (8)) (g 0 (4 (4)) (6 (8))) (h 1 (8) ()) (d 0 (2) (7)) (a 0 (4) (6)) (c 1 (6) ()) (f 0 (3) (6)) (e 1 (5) ())]
                                               '[(b 1 (4 (3)) (8)) (g 0 (4 (4)) (6 (8))) (h 1 (8) ()) (d 0 (2) (7)) (a 0 (4) (6)) (c 1 (6) ()) (f 0 (3) (6)) (e 1 (5) ())]
                                               '[(b 1 (4 (3)) (8)) (g 0 (4 (4)) (6 (8))) (h 1 (8) ()) (d 1 (5 (2)) (7)) (a 0 (4) (6)) (c 1 (6) ()) (f 0 (3) (6)) (e 1 (5) ())]))
            just-conds 'nil (same-seq-of-sets '([(b . 6) (g . 5) (h . 8) (d . 9/2) (a . 5) (c . 6) (f . 9/2) (e . 5)]
                                                [(b . 4) (g . 5) (h . 8) (d . 9/2) (a . 5) (c . 6) (f . 9/2) (e . 5)]
                                                [(b . 4) (g . 5) (h . 8) (d . 5) (a . 5) (c . 6) (f . 9/2) (e . 5)])))

(sumar)

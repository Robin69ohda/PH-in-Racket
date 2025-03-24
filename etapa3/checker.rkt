#lang racket

(require "etapa2.rkt")
(require "etapa3.rkt")

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
; Structura movie are 5 câmpuri: name, rating, genre, duration și others.
; Convenție de nume: "ms" înseamnă că filmul a fost văzut; numărul reprezintă rating-ul.
(define m10  (make-movie 'hundreds-of-beavers  10 'comedy    '(1 48) '(slapstick action)))
(define m9   (make-movie '12-angry-men         9  'drama     '(1 36) '(legal)))
(define m8   (make-movie 'manchurian-candidate 8  'thriller  '(2 06) '(spy tragedy)))
(define m7   (make-movie 'm*a*s*h              7  'comedy    '(1 56) '(satire drama)))
(define m6   (make-movie 'pulse-kairo          6  'horror    '(1 59) '(supernatural mystery)))
(define m5   (make-movie 'trap                 5  'horror    '(1 45) '(crime thriller)))
(define ms10 (make-movie 'rear-window          10 'thriller  '(1 52) '(seen drama suspense)))
(define ms9  (make-movie 'm                    9  'thriller  '(1 39) '(seen crime mystery)))
(define ms8  (make-movie 'menilmontant         8  'drama     '(0 38) '(seen tragedy short)))
(define ms7  (make-movie 'neo-tokyo            7  'animation '(0 50) '(seen scifi fantasy)))
(define ms3  (make-movie 'maniac               3  'horror    '(0 51) '(seen b-horror)))

; Test care ignoră comparația cu un rezultat exact, pentru a testa doar condiții.
(define just-conds (cons (lambda (_ __) #t) "this shouldn't happen?..."))

; Liniarizează un PH, strângând toate valorile lui într-o listă fără imbricări.
; ph->list : PH -> [T]
(define (ph->list ph)
  (if (ph-empty? ph) '()
      (apply append (list (ph-root ph)) (map ph->list (ph-subtrees ph)))))


; Testele încep de aici.
(sunt 3 exerciții)

(exercițiul 1 : 40 puncte)
(when (andmap procedure? (list best-k-rating best-k-duration))
; best-k-rating
(check-part 'a (/ 1 10) (best-k-rating '() 0) is '())
(check-part 'b (/ 1 10) (best-k-rating (list m8 ms9 ms3 m7) 4) is (list ms9 m8 m7 ms3))
(check-part 'c (/ 1 10) (best-k-rating (list m6 m5 m7) 10) is (list m7 m6 m5))
(check-part 'd (/ 1 10) (best-k-rating (list m6 m10 m5 m7 m9 ms3) 3) is (list m10 m9 m7))
(check-part 'e (/ 1 10) (best-k-rating (list m8 m7 ms8) 3) in (list (list m8 ms8 m7) (list ms8 m8 m7)))
; best-k-duration
(check-part 'f (/ 1 10) (best-k-duration (list ms7) 0) is '())
(check-part 'g (/ 1 10) (best-k-duration (list m8 ms9 ms3 m7) 4) is (list ms3 ms9 m7 m8))
(check-part 'h (/ 1 10) (best-k-duration (list m6 m5 m7) 10) is (list m5 m7 m6))
(check-part 'i (/ 1 10) (best-k-duration (list m6 m10 m5 m7 m9 ms3) 3) is (list ms3 m9 m5))
(check-part 'f (/ 1 10) (best-k-duration (list m8 ms10 m8) 3) is (list ms10 m8 m8)))

(exercițiul 2 : 30 puncte)
(check-part 'a (/ 1 5)
            (update-pairs (lambda (_) true)
                          '((a 10 (9) (10 (8) (9 (7) (9 (8)))))
                            (b 10 (9) (6) (8) (9) (8) (7 (7)))
                            (c 10 (8) (10 (8) (7) (9) (8) (6)))))
            is '((a 10 (9) (8) (9 (7) (9 (8)))) ; Și-a pierdut rădăcina.
                 (b 10 (9) (6) (8) (9) (8) (7 (7)))
                 (c 10 (8) (10 (8) (7) (9) (8) (6)))))
(check-part 'b (/ 1 5)
            (update-pairs (lambda (p) (equal? (ph-root (cdr p)) 10))
                          '((a 9.5 (9) (9.5 (8) (9 (7) (9 (8)))))
                            (b 9.5 (9) (6) (8) (9) (8) (7 (7)))
                            (c)
                            (d 10 (8) (9.5 (8) (7) (9) (8) (6)))))
            is '((a 9.5 (9) (9.5 (8) (9 (7) (9 (8)))))
                 (b 9.5 (9) (6) (8) (9) (8) (7 (7)))
                 (c)
                 (d 9.5 (8) (8) (7) (9) (8) (6))))  ; Și-a pierdut rădăcina.
(check-part 'c (/ 1 5)
            (update-pairs (lambda (p) (equal? 'b (car p)))
                          '((a 10 (9) (10 (8) (9 (7) (9 (8)))))
                            (b 10 (9) (6) (8) (9) (8) (7 (7)))
                            (c 10 (8) (10 (8) (7) (9) (8) (6)))))
            is '((a 10 (9) (10 (8) (9 (7) (9 (8)))))
                 (b 9 (8 (7 (7))) (9 (8)) (6)) ; Și-a pierdut rădăcina.
                 (c 10 (8) (10 (8) (7) (9) (8) (6)))))
(let ([pairs '((a 3 (2) (1))
               (b 10 (7 (3)) 5)
               (c)
               (d 2)
               (e))])
  ; Dacă nicio pereche nu satisface predicatul, se întoarce lista nemodificată.
  (check-part 'd (/ 1 5) (update-pairs (lambda (_) false) pairs) is pairs)
  ; Dacă PH-ul perechii este vid, se întoarce lista nemodificată.
  (check-part 'e (/ 1 5) (update-pairs (lambda (p) (ph-empty? (cdr p))) pairs) is pairs))

(exercițiul 3 : 50 puncte)

; Verifică dacă valoarea returnată este o listă de perechi, așa cum e cerut.
; has-right-type: T -> True | Str
(define (has-right-type? res)
  (define (elem-has-right-type? x) (and (pair? x) (symbol? (car x)) (number? (cdr x))))
  (if (and (list? res) (andmap elem-has-right-type? res))
      true
      "nu are tipul cerut (listă de perechi Symbol x Number)"))

; Verifică dacă perechile returnate se găsesc printre cele inițiale.
; valid-counts: [(Symbol, PH)] -> ([(Symbol, Number)] -> True | Str)
(define (valid-counts? ref-pairs)
  ; make-multiset: [T] -> HashTable
  (define (make-multiset l)
    (foldl (lambda (x mset) (hash-set mset x (add1 (hash-ref mset x 0))))
           (make-immutable-hash)
           l))
  ; superset?: HashTable x HashTable -> Bool
  (define (superset? superms ms)
    (andmap identity (hash-map ms (lambda (k v) (<= v (hash-ref superms k 0))))))

  ; tagged-pairs: (Symbol, PH) -> [(Symbol, Number)]
  (define (tagged-pairs symbol-ph)
    (match symbol-ph
      [(cons symbol ph) (map (lambda (i) (cons symbol i)) (ph->list ph))]))
  ; ref-counts: HashTable
  (define ref-counts (make-multiset (apply append (map tagged-pairs ref-pairs))))

  ; : [(Symbol, Number)] -> True | Str
  (lambda (candidate-pairs)
    (if (superset? ref-counts (make-multiset candidate-pairs))
        true
        "nu e un subset al elementelor inițiale; poate returnezi elemente duplicate sau nume greșite?")))

; Verifică dacă sunt returnate cele mai mari K rating-uri, în ordine.
; valid-order: [(Symbol, PH)] x Number -> ([(Symbol, Number)] -> True | Str)
(define (valid-order? ref-pairs k)
  (define all-ratings      (apply append (map (lambda (p) (ph->list (cdr p))) ref-pairs)))
  (define sorted-ratings   (sort all-ratings >))
  (define expected-ratings (take sorted-ratings (min k (length sorted-ratings))))

  ; : [(Symbol, Number)] -> True | Str
  (lambda (candidate-pairs)
    (if (equal? expected-ratings (map cdr candidate-pairs))
        true
        (format "pare să aibă rating-uri greșite (ne așteptam la ~s în ordinea asta)" expected-ratings))))


(let ([pairs '() ]) ; Input gol.
  (check-part 'a (/ 1 10) (best-k-ratings-overall pairs 7) just-conds 'nil has-right-type? (valid-counts? pairs) (valid-order? pairs 7)))
(let ([pairs '((A 9 (7) (8))
               (B 10))])
  (check-part 'b (/ 1 10) (best-k-ratings-overall pairs 1) just-conds 'nil has-right-type? (valid-counts? pairs) (valid-order? pairs 1))
  (check-part 'c (/ 1 10) (best-k-ratings-overall pairs 3) just-conds 'nil has-right-type? (valid-counts? pairs) (valid-order? pairs 3))
  (check-part 'd (/ 1 10) (best-k-ratings-overall pairs 4) just-conds 'nil has-right-type? (valid-counts? pairs) (valid-order? pairs 4)))
(let ([pairs '((a 10 (9) (10 (8) (9 (7) (9 (8)))))
               (b 10 (9) (6) (8) (9) (8) (7 (7)))
               (c 10 (8) (10 (8) (7) (9) (8) (6))))])
  (check-part 'e (/ 1 10) (best-k-ratings-overall pairs 3) just-conds 'nil has-right-type? (valid-counts? pairs) (valid-order? pairs 3))
  (check-part 'f (/ 1 10) (best-k-ratings-overall pairs 7) just-conds 'nil has-right-type? (valid-counts? pairs) (valid-order? pairs 7))
  (check-part 'g (/ 1 10) (best-k-ratings-overall pairs 12) just-conds 'nil has-right-type? (valid-counts? pairs) (valid-order? pairs 12))
  (check-part 'h (/ 1 10) (best-k-ratings-overall pairs 24) just-conds 'nil has-right-type? (valid-counts? pairs) (valid-order? pairs 24))
  (check-part 'i (/ 1 10) (best-k-ratings-overall pairs 0) just-conds 'nil has-right-type? (valid-counts? pairs) (valid-order? pairs 0))
  (check-part 'j (/ 1 10) (best-k-ratings-overall pairs 300) just-conds 'nil has-right-type? (valid-counts? pairs) (valid-order? pairs 300)))

(sumar)

#lang racket
(require racket/match)
(require "etapa2.rkt")
(provide (all-defined-out))

;; Această etapă continuă seria aplicațiilor heap-urilor  
;; de împerechere, pe care le vom folosi pentru a calcula
;; în mod dinamic mediana recenziilor unui film, simulând
;; condițiile din realitate - în care apar în permanență
;; noi recenzii pentru diverse filme.
;;    
;; Pentru a modela această dinamică folosim un flux de
;; perechi (nume-film . rating), pe baza căruia calculăm
;; un flux de stadii evolutive astfel:
;;  - fiecare stadiu este reprezentat ca listă de perechi
;;    * o pereche pentru fiecare film cu minim o recenzie
;;    * fiecare pereche este de forma
;;      (nume-film . mediană-rating-uri-primite-până-acum)
;;  - fiecare nouă recenzie determină actualizarea unei
;;    mediane, adică trecerea într-un alt stadiu,
;;    generând un nou element în fluxul rezultat
;;
;; Algoritmul folosit este următorul:
;;  Fluxul de perechi este transformat într-un flux de
;;  liste de cvartete (nume-film delta max-ph min-ph)
;;   - fiecare element din flux conține câte un cvartet 
;;     pentru fiecare film care are minim o recenzie
;;   - dacă filmul are un număr par de recenzii:
;;     - max-ph și min-ph au aceeași dimensiune
;;     - delta = size(max-ph) - size(min-ph) = 0
;;     - max-ph = max-PH cu cele mai mici rating-uri
;;     - min-ph = min-PH cu cele mai mari rating-uri
;;     - mediana este media rădăcinilor celor 2 PH-uri
;;   - dacă filmul are un număr impar de recenzii:
;;     - max-ph are un element în plus față de min-ph
;;     - delta = size(max-ph) - size(min-ph) = 1
;;     - max-ph = max-PH cu cele mai mici rating-uri
;;     - min-ph = min-PH cu cele mai mari rating-uri
;;     - mediana este rădăcina lui max-ph
;;
;; Pentru completarea cu succes a etapei este necesar să
;; calculați medianele cu algoritmul descris în enunț.
;; În caz contrar, punctajul acordat de checker va fi retras.


; TODO 1 (45p)
; add-rating : (Symbol, Int, PH, PH) x Number
;              -> (Symbol, Int, PH, PH)
; in: cvartet (nume delta max-ph min-ph),
;     rating de adăugat
; out: cvartet actualizat prin adăugarea 
;      rating-ului, astfel:
;  - dacă rating <= root(max-ph)
;    inserează rating în max-ph, actualizând delta
;  - altfel
;    inserează rating în min-ph, actualizând delta
;  - dacă delta > 1
;    mută root(max-ph) în min-ph
;  - dacă delta < 0
;    mută root(min-ph) în max-ph
(define (add-rating quad rating)
  (let ((name (car quad))
       (delta (cadr quad))
       (max-ph (caddr quad))
       (min-ph (cadddr quad)))
    (let* ((max-root (if (null? max-ph) #f (ph-root max-ph)))
           (min-root (if (null? min-ph) #f (ph-root min-ph))))
      
      ; Determine which heap to insert the rating into
      (if (<= rating max-root)
          ; Insert into max-ph (lower half)
          (let* ((new-max-ph (ph-insert merge-max rating max-ph))
                 (new-delta (+ delta 1))
                 (new-quad (list name new-delta new-max-ph min-ph)))
            ; Rebalance if needed (delta > 1)
            (if (> new-delta 1)
                (let* ((max-root (ph-root new-max-ph))
                       (updated-max-ph (ph-del-root merge-max new-max-ph))
                       (updated-min-ph (ph-insert merge-min max-root min-ph))
                       (updated-delta (- new-delta 2)))
                  (list name updated-delta updated-max-ph updated-min-ph))
                new-quad))
          
          ; Insert into min-ph (upper half)
          (let* ((new-min-ph (ph-insert merge-min rating min-ph))
                 (new-delta (- delta 1))
                 (new-quad (list name new-delta max-ph new-min-ph)))
            ; Rebalance if needed (delta < 0)
            (if (< new-delta 0)
                (let* ((min-root (ph-root new-min-ph))
                       (updated-min-ph (ph-del-root merge-min new-min-ph))
                       (updated-max-ph (ph-insert merge-max min-root max-ph))
                       (updated-delta (+ new-delta 2)))
                  (list name updated-delta updated-max-ph updated-min-ph))
                new-quad))))))


; TODO 2 (45p)
; reviews->quads : Stream<(Symbol, Number)> ->
;                  Stream<[(Symbol, Int, PH, PH)]>
; in: stream de perechi (nume . rating)
; out: stream de liste de cvartete
;      (nume delta max-ph min-ph)
;  - elementul k din rezultat corespunde primelor
;    k recenzii din input (ex: dacă primele 10
;    recenzii sunt pentru 3 filme distincte, al
;    10-lea element din fluxul rezultat conține o
;    listă de 3 cvartete - unul pentru fiecare film)
; RESTRICȚII (20p):
;  - Lucrați cu operatorii pe fluxuri, fără a
;    converti liste în fluxuri sau fluxuri în liste.
(define (process-review review quads)
    (let* ((name (car review))
           (rating (cdr review))
           (existing-quad (findf (λ (q) (equal? (car q) name)) quads)))
      (if existing-quad
          (let* ((updated-quad (add-rating existing-quad rating))
                 (other-quads (filter (λ (q) (not (equal? (car q) name))) quads)))
            (cons updated-quad other-quads))
          (cons (list name 1 (list rating) '()) quads))))

(define (reviews->quads reviews)
  (let loop ((revs reviews) (current-quads '()) (result-stream empty-stream))
    (if (stream-empty? revs)
        result-stream
        (let* ((review (stream-first revs))
               (new-quads (process-review review current-quads)))
          (stream-cons 
            new-quads
            (loop (stream-rest revs) new-quads result-stream))))))


; TODO 3 (30p)
; quads->medians : Stream<[(Symbol, Int, PH, PH)]> ->
;                  Stream<[(Symbol, Number)]>  
; in: stream de liste de cvartete (ca mai sus)
; out: stream de liste de perechi (nume-film . mediană)
;  - mediana se calculează pe baza PH-urilor din
;    fiecare cvartet, conform algoritmului de mai sus
; RESTRICȚII (20p):
;  - Nu folosiți recursivitate explicită. Folosiți cel
;    puțin o funcțională pe fluxuri.
(define (quads->medians quads)
  (if (stream-empty? quads)
      empty-stream
      (stream-map 
       (λ (quad-list)
         (map
          (λ (quad)
            (let* ((name (car quad))
                   (delta (cadr quad))
                   (max-ph (caddr quad))
                   (min-ph (cadddr quad))
                   (max-root (if (null? max-ph) #f (ph-root max-ph)))
                   (min-root (if (null? min-ph) #f (ph-root min-ph))))
              (cond
                ((and max-root (not min-root)) (cons name max-root))
                ((and (not max-root) min-root) (cons name min-root))
                ((= delta 1) (cons name max-root))
                ((= delta 0) (cons name (/ (+ max-root min-root) 2))))))
          quad-list))
       quads)))
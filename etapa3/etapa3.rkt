#lang racket
(require "etapa2.rkt")
(require racket/match)
(provide (all-defined-out))

;; Această etapă este dedicată aplicațiilor heap-urilor de 
;; împerechere, pe care le vom folosi pentru:
;;  - a extrage cele mai bune filme dintr-o listă, conform
;;    cu un anumit criteriu
;;  - a extrage cele mai bune recenzii dintr-o colecție
;;    de recenzii pentru diverse filme (o recenzie bună
;;    corespunde unei note bune acordate filmului respectiv)
;;
;; Pentru completarea cu succes a etapei este necesar să
;; rezolvați sarcinile cu algoritmii dedicați PH-urilor 
;; (descriși în enunț). Punctajul acordat de checker va fi 
;; retras dacă sarcinile sunt rezolvate cu alți algoritmi.


; TODO 1 (40p)
; Definiți funcția best-k într-o formă care
; facilitează derivarea ulterioară a funcțiilor
; best-k-rating și best-k-duration.
; in: criteriu de comparație op (care compară 2 filme),
;     listă de filme movies, număr k
; out: lista sortată a celor mai "bune" filme
;      conform criteriului (cel mai "bun" primul)
; Algoritm:
;  1. construiește un PH de filme pe baza listei movies 
;     și a criteriului op
;  2. extrage în mod repetat root-ul acestui PH până
;     când rezultatul conține k filme (sau PH-ul devine vid)
; RESTRICȚII (20p):
;  - Folosiți named let pentru a efectua pasul 2 al
;    algoritmului.
(define best-k
  'your-code-here)

; best-k-rating : [Movie] x Int -> [Movie]
; in: listă de filme movies, număr k
; out: cele mai bune k filme din movies (ca rating)
; RESTRICȚII (5p):
;  - Obțineți best-k-rating ca aplicație a lui best-k.
(define best-k-rating
  'your-code-here)

; best-k-duration : [Movie] x Int -> [Movie]
; in: listă de filme movies, număr k
; out: cele mai scurte k filme din movies 
; RESTRICȚII (5p):
;  - Obțineți best-k-duration ca aplicație a lui best-k.
(define best-k-duration
  'your-code-here)


; TODO 2 (30p)
; update-pairs : ((Symbol, PH) -> Bool) x [(Symbol, PH)]
;                -> [(Symbol, PH)]
; in: predicat p, listă de perechi (nume-film . PH)
;     (PH este un max-PH care conține notele acordate
;      filmului în diverse recenzii - așadar un PH
;      de numere)
; out: lista pairs actualizată astfel:
;      - pentru prima pereche care satisface predicatul 
;        p, PH-ului perechii i se șterge rădăcina
;      - dacă PH-ul perechii este vid sau dacă nicio pereche
;        nu satisface p, se întoarce lista pairs nemodificată
; RESTRICȚII (20p):
;  - Folosiți named let pentru a itera prin perechi.
(define (update-pairs p pairs)
  'your-code-here)
                  

; TODO 3 (50p)
; best-k-ratings-overall : [(Symbol, PH)] x Int
;                          -> [(Symbol, Number)]
; in: listă de perechi (nume-film . PH)
;     (ca mai sus, PH este un max-PH de rating-uri)
;     număr k 
; out: lista sortată a celor mai bune k perechi
;      (nume-film . rating), corespunzând celor mai
;      bune rating-uri din toate PH-urile
; Algoritm:
;  1. Se inițializează un PH de perechi (nume . rating), 
;     corespunzând celui mai bun rating al fiecărui film
;     (adică se extrage rădăcina fiecărui PH de ratinguri,
;      în pereche cu numele filmului aferent)
;  2. Repetă de k ori:
;     - extrage rădăcina PH-ului de rădăcini
;       (name-root . rating-root)
;       (adică extrage cea mai bună pereche per total)
;     - adu în PH-ul de rădăcini următorul cel mai bun 
;       rating al filmului name-root (dacă există)
; RESTRICȚII (20p):
;  - Folosiți named let pentru a efectua pasul 2 al
;    algoritmului.
(define (best-k-ratings-overall pairs k)
  'your-code-here)


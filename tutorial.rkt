#lang racket

; ----- Structuri -----

; Definirea unei structuri movie, cu 5 câmpuri:
; - name, rating, genre, duration și others.
; Transparent permite afișarea valorilor câmpurilor.
(define-struct movie (name rating genre duration others) #:transparent)

; Definirea unui obiect structură.
"Definire"
(define m1 (make-movie '12-angry-men 9 'drama '(1 36) '(legal)))
m1

; Accesul la câmpuri se face prin funcțiile <structură>-<câmp>.
"Acces"
(movie-name m1)
(movie-rating m1)
(movie-genre m1)
(movie-duration m1)
(movie-others m1)

; „Modificarea” câmpurilor rating și others.
; Câmpurile nemenționate explicit își mențin valoarea.
; Atragem atenția că nu există nicio modificare propriu-zisă
; a obiectului existent, ci se creează un nou obiect 
; din noile valori ale câmpurilor.
"Modificare"
(define m2 (struct-copy movie m1 [rating 9.0] [others '(legal psychological)]))
m1
m2

; Construcția match permite pattern matching
; (nu doar pentru structuri).
"Match care întoarce duration"
(match m1 [(movie nam rat gen dur oth) dur])

; Underscore permite ignorarea câmpurilor pe care nu le folosim.
"Match care întoarce others"
(match m2 [(movie _ _ _ _ oth) oth])

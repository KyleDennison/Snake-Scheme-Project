;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Snekproject) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)
(define scene (empty-scene 500 500))
(define snekalarm (text "Snek hit the wall" 24 "olive"))
(define foodShape (circle 10 "solid" "blue"))
(define snekHead (circle 5 "solid" "red"))
(define-struct snek (head tail dir))
(define mysnek (make-snek (make-posn 250 250) '() "up"))
(define food (make-posn 100 100))
(define-struct game (snek food))
(define mygame (make-game mysnek (make-posn 100 100) ))
;Check to see if game is over
(define (hitwall? mysnek)
  (if (or (>= (posn-x (snek-head mysnek)) 500)(<= (posn-x (snek-head mysnek)) -1)(>= (posn-y (snek-head mysnek)) 500)(<= (posn-y (snek-head mysnek)) -1)) #true #false))

(define (hitself? mysnek snektail )
  (cond
    [(empty? snektail)#false]
    [(member? (snek-head mysnek) snektail) #true]
    [else #false]))
  

;Drawing the game 
(define (placeHead snekhead snektail)
  (place-image snekHead (posn-x snekhead) (posn-y snekhead) snektail))



(define (placeTail snektail scene)
  (cond
    [(empty? snektail) scene]
    [else (place-image (circle 5 "solid" "red") (posn-x (first snektail)) (posn-y (first snektail)) (placeTail (rest snektail) scene))]))

(define (drawSnek mysnek)
  (cond
    [(hitwall? mysnek) (place-image (text "Snek hit the wall" 24 "olive") 200 400 scene)]
    [(hitself? mysnek (snek-tail mysnek)) (place-image (text "snek hit itself" 24 "olive") 200 400 scene)]
    [else (placeHead (snek-head mysnek) (placeTail (snek-tail mysnek) scene))]))

(define (drawFood mygame scene)
  (place-image foodShape (posn-x (game-food mygame)) (posn-y (game-food mygame)) scene))

(define (drawGame mygame)
     (drawFood mygame (drawSnek (game-snek mygame))))

;Moving the game 

(define (tick-tail tail head)
  (cond [(empty? tail) '()]   
        [else (cons head (reverse (rest (reverse tail))))]))



(define (moveSnek mysnek)
  (cond
    [(equal? (snek-dir mysnek) "up") (make-snek (make-posn (posn-x (snek-head mysnek)) (-(posn-y (snek-head mysnek))10)) (tick-tail (snek-tail mysnek) (snek-head mysnek))  "up" )]
    [(equal? (snek-dir mysnek) "down") (make-snek (make-posn (posn-x (snek-head mysnek)) (+(posn-y (snek-head mysnek))10)) (tick-tail (snek-tail mysnek) (snek-head mysnek))  "down" )]
    [(equal? (snek-dir mysnek) "left") (make-snek (make-posn (-(posn-x (snek-head mysnek))10) (posn-y (snek-head mysnek))) (tick-tail (snek-tail mysnek) (snek-head mysnek))  "left" )]
    [(equal? (snek-dir mysnek) "right") (make-snek (make-posn (+(posn-x (snek-head mysnek))10) (posn-y (snek-head mysnek))) (tick-tail (snek-tail mysnek) (snek-head mysnek))  "right" )]))

(define (eating? mygame snekhead)
  (if (and (equal? (posn-x (game-food mygame)) (posn-x snekhead)) (equal? (posn-y (game-food mygame)) (posn-y snekhead))) #true #false))
    

(define (newHead mysnek)
  (cond
    [(string=? (snek-dir mysnek) "up") (make-posn (posn-x (snek-head mysnek)) (-(posn-y (snek-head mysnek)) 10))]
    [(string=? (snek-dir mysnek) "down") (make-posn (posn-x (snek-head mysnek)) (+(posn-y (snek-head mysnek)) 10))]
    [(string=? (snek-dir mysnek) "left") (make-posn (-(posn-x (snek-head mysnek))10) (posn-y (snek-head mysnek)))]
    [(string=? (snek-dir mysnek) "right") (make-posn (+(posn-x (snek-head mysnek))10) (posn-y (snek-head mysnek)))]))

(define (newTail snektail snekhead)
  (cons snekhead snektail))

(define (grow mygame)
  (make-game (make-snek (newHead (game-snek mygame)) (newTail (snek-tail (game-snek mygame)) (snek-head (game-snek mygame))) (snek-dir (game-snek mygame)))
             (make-posn (*(random  50)10) (*(random  50)10) )))
  
(define (moveGame mygame)
  (cond
    [(eating? mygame (snek-head (game-snek mygame))) (grow mygame)]
    [else (make-game (moveSnek (game-snek mygame))(game-food mygame))]))

;Key presses for game

(define (changeDir mysnek key)
  (cond
    [(and (equal? key "up") (equal? (snek-dir mysnek) "down")) (make-snek (make-posn(posn-x (snek-head mysnek)) (posn-y (snek-head mysnek))) (tick-tail (snek-tail mysnek) (snek-head mysnek)) "down" )]
    [(and (equal? key "down") (equal? (snek-dir mysnek) "up"))(make-snek (make-posn(posn-x (snek-head mysnek)) (posn-y (snek-head mysnek))) (tick-tail (snek-tail mysnek) (snek-head mysnek)) "up" )]
    [(and (equal? key "left") (equal? (snek-dir mysnek) "right")) (make-snek (make-posn(posn-x (snek-head mysnek)) (posn-y (snek-head mysnek))) (tick-tail (snek-tail mysnek) (snek-head mysnek)) "right" )]
    [(and (equal? key "right") (equal? (snek-dir mysnek) "left")) (make-snek (make-posn(posn-x (snek-head mysnek)) (posn-y (snek-head mysnek))) (tick-tail (snek-tail mysnek) (snek-head mysnek)) "left" )]
    [(equal? key "up") (make-snek (make-posn(posn-x (snek-head mysnek)) (posn-y (snek-head mysnek))) (snek-tail mysnek) "up" )]
    [(equal? key "down") (make-snek (make-posn(posn-x (snek-head mysnek)) (posn-y (snek-head mysnek))) (snek-tail mysnek) "down" )]
    [(equal? key "left") (make-snek (make-posn(posn-x (snek-head mysnek)) (posn-y (snek-head mysnek))) (snek-tail mysnek) "left" )]
    [(equal? key "right") (make-snek (make-posn(posn-x (snek-head mysnek)) (posn-y (snek-head mysnek))) (snek-tail mysnek) "right" )]))

(define (keypressGame mygame key)
  (make-game (changeDir (game-snek mygame) key) (game-food mygame)))

;Stopping the snek

(define (stopSnek mygame)
  (cond
    [(if (or (>= (posn-x (snek-head (game-snek mygame))) 510)(<= (posn-x (snek-head (game-snek mygame))) -10)(>= (posn-y (snek-head (game-snek mygame))) 510)(<= (posn-y (snek-head (game-snek mygame))) -10)) #true #false) #true]
    [(hitself? (game-snek mygame) (snek-tail (game-snek mygame))) #true]
    [else #false]))
  
(define (snekAlarm mygame)
  (cond
    [(hitwall? (game-snek mygame)) (place-image (text "Snek hit the wall" 24 "olive") 200 400 scene)]
    [(hitself? (game-snek mygame) (snek-tail (game-snek mygame))) (place-image (text "snek hit itself" 24 "olive") 200 400 scene)]))


(big-bang mygame
  [to-draw drawGame]
  [on-tick moveGame 0.05]
  [on-key keypressGame]
  [stop-when stopSnek snekAlarm]
  )





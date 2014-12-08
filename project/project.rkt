
#lang racket/gui


(require 2htdp/universe)
(require 2htdp/image)
(require "sound.ss")

;;-------------------------------BASIC GRAPHICS ELEMENTS-------------------------------
(define board (bitmap/file "b.jpg"))
(define w (circle 20 "solid" "white"))
(define b (circle 20 "solid" "violet"))
(define a (rectangle 200 50 "solid" "red"))
(define exit (bitmap/file "exit-b2.png"))
(define roll-dice (bitmap/file "roll-dice.jpg"))
(define reset (bitmap/file "reset.jpg"))
(define background1
  (underlay/xy (rectangle 1050 600 "solid" "red") 0 0 board))
(define (dice-s) (background-play-sound-file "dice-s.wav"))
(define (button-s) (background-play-sound-file "button-s.wav"))
(define (checker-s) (background-play-sound-file "checker-s.wav"))
(define background
  (let* ((i2 (place-image roll-dice 200 550 background1))
         (i3 (place-image exit 800 550 i2))
         (i4 (place-image (text "EXIT" 20 "red") 830 540 i3))
         (i5 (place-image reset 500 550 i4)))
    i5))

(define arrow-u (bitmap/file "bgup.png"))
(define arrow-d (bitmap/file "bgdown.png"))
(define dice1 (bitmap/file "dice1.png"))
(define dice2 (bitmap/file "dice2.png"))
(define dice3 (bitmap/file "dice3.png"))
(define dice4 (bitmap/file "dice4.png"))
(define dice5 (bitmap/file "dice5.png"))
(define dice6 (bitmap/file "dice6.png"))
(define start-button (bitmap/file "start-b.png"))
(define start-page 
  (let ((i 
         (place-image start-button 900 450 (bitmap/file "start2.jpg"))))
    (place-image (bitmap/file "rules-b.png") 880 530 i)))

(define last-page (bitmap/file "exit-page.png"))
(define inst-page (place-image (bitmap/file "back-b.png")
                               800 450  (bitmap/file "inst-page1.png")))
(define page start-page)

;---------------------------------- MACROS DEFINITIONS------------------------------------------
(define (concat l)
  (foldr append `() l))

(define-syntax lc
  (syntax-rules (: <- @)
    [(lc exp : var <- lexp) (map (lambda (var) exp) lexp)]
    [(lc exp : @ guard) (if guard (list exp) `())]
    [(lc exp : @ guard qualifier ...) 
     (concat (lc (lc exp : qualifier ...) : guard))]
    [(lc exp : var <- lexp qualifier ...) 
     (concat (lc (lc exp :  qualifier ... ) : var <- lexp))]))

(define-syntax for
  (syntax-rules ()
    [(for init bexp change statements)
     (begin 
       init
       (define (loop)
         (cond [bexp (begin 
                       statements
                       change
                       (loop))]))
       (loop))]))

;;------------------------------------BOARD INITIALISATION------------------------------------

(define v1 (make-vector 26 0))
(define v2 (make-vector 26 0))

(define (init-board color)
  (vector-set! (if color v1 v2) (if color 1 24) 2)
  (vector-set! (if color v1 v2) (if color 12 13) 5)
  (vector-set! (if color v1 v2) (if color 17  8) 3)
  (vector-set! (if color v1 v2) (if color 19 6) 5))

(init-board #t)
(init-board #f)

(define v1-init (vector-copy v1))
(define v2-init (vector-copy v2))

(define (initial-board)
  (set! v1 (vector-copy v1-init))
  (set! v2 (vector-copy v2-init)))

;;----------------------------------- BOARD SETTING FUNCTIONS-----------------------------
(define (move-coin vec1 vec2 pos1 pos2 color)
  (if color
      (cond [(> pos2 24) (begin
                           (vector-set! vec1 pos1 
                                        (- (vector-ref vec1 pos1) 1))
                           (vector-set! vec1 25 
                                        (+ (vector-ref vec1 25) 1)))]
            [else    (if (= (vector-ref vec2 pos2) 1)
                         (begin
                           (vector-set! vec2 pos2 0)
                           (vector-set! vec2 0 
                                        (+ (vector-ref vec2 0) 1)))
                         (void))
                     (vector-set! vec1 pos1 
                                  (- (vector-ref vec1 pos1) 1))
                     (vector-set! vec1 pos2 
                                  (+ (vector-ref vec1 pos2) 1))
                     'done])
      (cond
        [(= pos1 0)
         (vector-set! vec1 pos1 
                      (- (vector-ref vec1 pos1) 1))
         (vector-set! vec1 (+ 25 pos2) 
                      (+ (vector-ref vec1 (+ 25 pos2)) 1))
         (cond [(= (vector-ref vec2 (+ 25 pos2)) 1)
                (vector-set! vec2 (+ 25 pos2) 0)
                (vector-set! vec2 0 (+ 1 (vector-ref vec2 0)))])]
        [(< pos2 1) (begin
                      (vector-set! vec1 pos1 
                                   (- (vector-ref vec1 pos1) 1))
                      (vector-set! vec1 25 
                                   (+ (vector-ref vec1 25) 1)))]
        [else
         (if (= (vector-ref vec2 pos2) 1)
             (move-coin vec2 vec1 pos2 0 #t) (void))
         (vector-set! vec1 pos1 
                      (- (vector-ref vec1 pos1) 1))
         (vector-set! vec1 pos2 
                      (+ (vector-ref vec1 pos2) 1))
         'done]))
  )

;;;;---------------------------POSSIBLE MOVE CHECKING FUNCTIONS-----------------------------------
(define (allowed-move? pos1 pos2 vec1 vec2 color)
  (cond [color
         (cond [(or (= pos1 0)
                    (= 0 (vector-ref vec1 0))) 
                ;;;;if any checker is at 0th vec1 then it should start first
                (if (>= (vector-ref vec1 pos1) 1)
                    (cond [(> pos2 24)
                           (can_remove_piece vec1 vec2 'black)]
                          [(<= (vector-ref vec2
                                           pos2) 1) #t]
                          [else #f])
                    #f)]
               [else #f])]
        [else (cond [(= pos1 0)
                     (if (and (not (= (vector-ref vec1 pos1) 0))
                              (<= (vector-ref vec2
                                              (+ 25 pos2)) 1)) #t #f)]
                    [else (if (= 0 (vector-ref vec1 0))
                              (if (>= (vector-ref vec1 pos1) 1)
                                  (cond [(< pos2 1)
                                         (if (can_remove_piece vec2 vec1 'white) #t #f)]
                                        [(<= (vector-ref vec2
                                                         pos2) 1) #t]
                                        [else #f]) #f)
                              #f)]
                    )]))

(define (can_remove_piece vec1 vec2 color)
  (if (equal? color 'black)
      (must-be-empty? vec1 1 18) ;;must be present in inner board
      (must-be-empty? vec2 7 24)))

(define (maxl pair)
  (if (<= (car pair) (cdr pair)) (cdr pair) (car pair)))

(define (minl pair)
  (if (< (car pair) (cdr pair)) (car pair) (cdr pair)))
;;-----------------------------------------------------------------------------------------

(define (possible-next-moves diceval vec1 vec2 color)
  (define movelist `())
  (define operator (if color
                       + -)) ;; returns a function
  (for 
      (define i 0)
    (<= i 24)
    (set! i (+ i 1))
    (cond [(allowed-move? i (operator i diceval) vec1 vec2 color)
           (set! movelist (cons (cons i (operator i diceval)) movelist))]))
  movelist)

(define (terminate object)
  (must-be-empty? object 1 24))

(define (must-be-empty? vec pos1 pos2)
  (cond [(> pos1 pos2) #t]
        [(= (vector-ref vec pos1) 0)
         (must-be-empty? vec (+ pos1 1) pos2)]
        [else #f]))

;;--------------------------------STATIC BOARD VALUE EVALUATION ----------------------------------

;;-----------------BOARD CLASS--------------------------------------
(define board%
  (class object%
    (init-field [val 0])
    (define val1 50)
    (define val2 500)
    (define val3 11)
    (super-new)
    (define (all-v-coin-win v)
      (if (= (vector-ref v 25) 15) #t #f))
    (define (index-sum l i value type)
      (cond [(null? l) value]
            [(= type 1)
             (index-sum (cdr l) (+ i 1) (+ value (* 5 (car l) i)) 1)]
            [else (if (= i 25)
                      (+ value (* 5 (car l) 25))
                      (index-sum (cdr l) (+ i 1) (+ value (* 5 (car l) (- 25 i))) 2))]))
    
    (define (count-coin-pos v i j)
      (cond [(> i j) 0]
            [(not (= (vector-ref v i) 0)) (+ 1 (count-coin-pos v (+ 1 i) j))]
            [else (count-coin-pos v (+ 1 i) j)]))
    (define (single-coin v pos ans)
      (if (= pos 25) ans
          (if (= (vector-ref v pos) 1) (single-coin v (+ 1 pos) (cons pos ans))
              (single-coin v (+ 1 pos) ans))))
    (define (single l type p-v c-v ans)
      (if (= type 1)
          (cond [(null? l) ans]
                [(>= (car l) 13) (cond [(and (= (vector-ref c-v 0) 0) 
                                             (must-be-empty? c-v (+ 1 (car l)) 24)) 
                                        (single (cdr l) type p-v c-v ans)]
                                       [(not (= (vector-ref c-v 0) 0))
                                        (single (cdr l) type p-v c-v (+ ans (car l)))]
                                       [else 
                                        (single (cdr l) type p-v c-v 
                                                (+ ans (* (car l) 
                                                          (count-coin-pos c-v (+ 1 (car l)) 24))))])]
                
                [(< (car l) 13) (if (or (must-be-empty? c-v (+ 1 (car l)) (+ 12 (car l)))
                                        (not (= (vector-ref c-v 0) 0)))
                                    (single (cdr l) type p-v c-v ans)
                                    (single (cdr l) type p-v c-v 
                                            (+ ans (* (car l) 
                                                      (count-coin-pos c-v (+ 1 (car l))
                                                                      (+ 12 (car l)))))))])
          
          (cond [(null? l) ans]
                [(<= (car l) 12) (cond [(must-be-empty? p-v 0 (- (car l) 1)) 
                                        (single (cdr l) type p-v c-v ans)]
                                       [(not (= (vector-ref p-v 0) 0))
                                        (single (cdr l) type p-v c-v (+ ans (- 25 (car l))))]
                                       [else 
                                        (single (cdr l) type p-v c-v 
                                                (+ ans (* (- 25 (car l)) 
                                                          (count-coin-pos p-v 1 (- (car l) 1)))))])]
                [(>= (car l) 13) (if (or (must-be-empty? p-v (- (car l) 12) (- (car l) 1))
                                         (not (= (vector-ref p-v 0) 0)))
                                     (single (cdr l) type p-v c-v ans)
                                     (single (cdr l) type p-v c-v 
                                             (+ ans (* (- 25 (car l)) 
                                                       (count-coin-pos c-v (- (car l) 12) (- (car l) 1))))))])))
    (define/public (eval consed-vec)
      (set! val 0)
      (let ([v1 (car consed-vec)]
            [v2 (cdr consed-vec)])
        (cond [(all-v-coin-win v1) (set! val (- 10000))]
              [(all-v-coin-win v2) (set! val 10000)]
              [else (begin
                      (set! val (+ (- val (index-sum (vector->list v1) 0 0 1)
                                      (index-sum (vector->list v2) 1 0 2))))
                      (set! val (+ (- val (* val1 (vector-ref v1 25)))
                                   (* val1 (vector-ref v2 25))))
                      (set! val (- (+ val (* val2 (vector-ref v1 0)))
                                   (* val2 (vector-ref v2 0))))
                      (cond [(must-be-empty? v1 0 18) 
                             (set! val (- val val3))]
                            [(and (must-be-empty? v2 7 24) (= (vector-ref v2 0) 0)) 
                             (set! val (+ val val3))]
                           
                            [(not (null? (single-coin v2 1 `()))) 
                             (let ((val4 (single (single-coin v2 1 `()) 2 v1 v2 0)))
                               (set! val (- val (* 50 val4))))]))])
        val))))



(define new-board (make-object board%))


;;---------------------- EXPECTIMINIMAX ALGORITHM (BY EVALUATING VIRTUAL BOARD STATES) ----------
(define alpha -inf.0)
(define beta +inf.0)


(define (next-virtual-board-states val curr-p curr-c boolean-value)
  (let* ((tempp (vector-copy curr-p))
         (tempc (vector-copy curr-c))
         (to-be-changed (if boolean-value tempp tempc))
         (not-to-be-changed (if boolean-value tempc tempp)))
    (move-coin to-be-changed 
               not-to-be-changed (car val) (cdr val) boolean-value)
    (cons tempp tempc)
    ))

(define (nextlist pos-list current-player current-computer bool)
  (if (null? pos-list) `()
      (cons (next-virtual-board-states (car pos-list)
                                       current-player
                                       current-computer bool)
            (nextlist (cdr pos-list) current-player current-computer bool))))

(define (next-state dicecons current-player current-computer bool)
  (let* ([d1 (maxl dicecons)]
         [d2 (minl dicecons)]
         [player (if bool current-player current-computer)]
         [computer (if bool current-computer current-player)]
         [pos (possible-next-moves d1 player computer bool)]
         [pos2 (possible-next-moves d2 player computer bool)]
         [state1 (nextlist pos current-player current-computer bool)]
         [state2 (nextlist pos2 current-player current-computer bool)]
         [final-state
          (if (null? state1)
              (nextlist pos2 current-player
                        current-computer bool)
              (concat (map (λ(u) (nextlist (possible-next-moves d2 
                                                                (if bool (car u) (cdr u))
                                                                (if bool (cdr u) (car u)) bool)
                                           (car u) (cdr u) bool)) state1)))])
    (if (null? final-state) state1 final-state
        )))


;;;------------------ALPHA-BETA PRUNING AND EXPECTION VALUE OF A BOARD 
;;-------------------(wrt TAKING ACCOUNT ALL POSSIBLE DICE VALUES)----------------

(define (evaluate-correct-step state player depth)
  (for (begin (define i 1)
              (define val 0))
    (<= i 6)
    (set! i (+ i 1))
    (begin (for (define j 1)
             (<= j 7)
             (set! j (+ j 1))
             (set! val (+ val (alpha-beta 
                               (next-state (cons i j) (car state) (cdr state) (not player))
                               (not player) (- depth 1))))))))


(define l1 (vector-copy v1))
(define l2 (vector-copy v2))

(define (alpha-beta state maxplayer depth dice)
  
  (define (next-comp dice)
    (let* ([possibility (next-state dice v1 v2 #f)]
           [temp (map (λ(u) (cons (send new-board eval u) u)) possibility)]
           [max-board-value (foldr (λ(u v) (if (> (car u) v) (car u) v))
                                   alpha temp)]
           [finalised-state (if (null? possibility) #f 
                                (cdr (assoc max-board-value temp)))])
      (if finalised-state
          (begin
            (set! v1 (car finalised-state))
            (set! v2 (cdr finalised-state)))
          (void))))
  
  (define (alpha-beta-helper node)
    (define operator-new (if maxplayer max min))
    
    (if (or (= depth 0) (must-be-empty? l1 1 24))  
        ;; or depth is 0 or there is a terminating condition
        (max (map (λ(u) (send new-board eval (cons l1 state)))))
        (let* ([new-moves (nextlist dice l1 l2 (not maxplayer))]
               [beta-new (if (operator-new beta (car node)) beta (car node))]
               [reduced-list (cons beta-new (cdr node))])
          (if maxplayer
              (max reduced-list 
                   (map (λ(u) (evaluate-correct-step reduced-list #f (- depth 1)))))
              (min reduced-list
                   
                   (map (λ(u) (evaluate-correct-step reduced-list #t (- depth 1))))))
          )))
  (next-comp dice)
  )

;;---------------------------FRAMES AND BUTTONS -------------------------------------------------

(define (set-image i p-type pos-no ith-coin h image)
  (if (eq? p-type `w)
      (let ((next-image (place-image
                         w (x-cord pos-no p-type) 
                         (y-cord pos-no p-type ith-coin h) image)))
        (if (= i 1) next-image
            (set-image (- i 1) p-type pos-no (+ 1 ith-coin) h next-image)))
      (let ((next-image (place-image
                         b (x-cord pos-no p-type) 
                         (y-cord pos-no p-type ith-coin h) image)))
        (if (= i 1) next-image
            (set-image (- i 1) p-type pos-no (+ 1 ith-coin) h next-image)))))


(define (x-cord pos-no p-type)
  (if (eq? p-type 'w)
      (cond [(<= pos-no 6) (- 965 (* pos-no 65))]
            [(and (> pos-no 6) (<= pos-no 12)) (+ 130 (* 65 (- 12 pos-no)))]
            [(and (> pos-no 12) (<= pos-no 18)) (+ 130 (* 65 (- pos-no 13)))]
            [(and (> pos-no 18) (<= pos-no 24)) (- 900 (* 65 (- 24 pos-no)))]
            [(= pos-no 25) 70])
      (cond [(= pos-no 0) 965]
            [(= pos-no 25) 70]
            [ else (x-cord pos-no 'w)])))


(define (y-cord pos-no p-type i h)
  (if (eq? p-type 'w)
      (cond [(or (<= pos-no 12) (= pos-no 25)) (- 470 (* h (- i 1)))]
            [else (+ 40 (* h (- i 1)))])
      (cond [(or (= pos-no 0) (= pos-no 25)) (+ 40 (* h (- i 1)))]
            [else (y-cord pos-no `w i h)])))

(define (set-board l plyr-type pos-no last-image)
  (foldl (λ(num y) 
           (let ((h (if (<= num 5) 40
                        (/ 200 num))))
             (begin 
               (set! pos-no (+ 1 pos-no))
               (if (= num 0) y
                   (set-image num plyr-type (- pos-no 1) 1 h y))))) 
         last-image l))

(define (create-image l1 l2)
  (let ((image (set-board l1 `w 0 background)))
    (set-board l2 `b 0 image)))


(define game-page1 (create-image (vector->list v1) (vector->list v2)))
(define game-page game-page1)

(define ((show-page) t)  page)

(define (find-pos x y p-type i)
  (cond [(= i 13) 30]
        [(and (<= y 470) (>= y 310) (<= x 985) (>= x 945) (eq? p-type `w)) 0]
        [(and (<= y 200) (>= y 40) (<= x 985) (>= x 945) (eq? p-type `b)) 0]
        [(and (>= x (- (x-cord i p-type) 20)) 
              (<= x (+ (x-cord i p-type) 20)) (and (<= y 470) (>= y 310))) i]
        [(and (>= x (- (x-cord i p-type) 20)) 
              (<= x (+ (x-cord i p-type) 20)) 
              (and (<= y 200) (>= y 40))) (- 25 i)]
        [else (find-pos x y p-type (+ 1 i))]))

;;-------------------------MOUSE CLICK EVENTS-------------------------------------
(define start-game #t)
(define select-coin #f)
(define roll-dice-i #f)
(define roll-dice-p #f)
(define roll-dice-c #f)
(define 1st-possible-move #t)
(define c-move #f)
(define dice-val-p1 0)
(define dice-val-p2 0)
(define inst-page-open #f)
(define p1 #f)
(define final-val -1)
(define move 1)
(define (dice)
  (cons (+ 1 (random 6)) (+ 1 (random 6))))

(define (show-dice x y val)
  (cond [(= val 1) (set! game-page (place-image dice1 x y game-page))]
        [(= val 2) (set! game-page (place-image dice2 x y game-page))]
        [(= val 3) (set! game-page (place-image dice3 x y game-page))]
        [(= val 4) (set! game-page (place-image dice4 x y game-page))]
        [(= val 5) (set! game-page (place-image dice5 x y game-page))]
        [(= val 6) (set! game-page (place-image dice6 x y game-page))]))

(define dice-val 0)
(define player-pos #f)
(define (show-arrow l)
  (foldr (λ(i img) (if (<= i 12)
                       (begin (set! game-page 
                                    (place-image arrow-u (x-cord i `w)  
                                                 (y-cord i `w 1 40) game-page)))
                       (begin (set! game-page 
                                    (place-image arrow-d (x-cord i `w)  
                                                 (y-cord i `w 1 40) game-page)))))  game-page l))
(define (reset-game x y)
  (if (and (<= x 600) (>= x 400) (<= y 590) (>= y 510)) #t #f))


(define (check-exit x y)
  (if (and (<= x 900) (>= x 700) (<= y 575) (>= y 525)) #t #f))

(define (roll-dice-clicked x y)
  (and (<= x 270) (>= x 130) (<= y 600) (>= y 500)))

;;--------------------------------------------------------------------------------------------------------
(define (mouse-event world x y event)
  (cond [(eq? event "button-down")
         
         (cond [(and (not start-game) (check-exit x y))
                (button-s) (set! page last-page)]
               [(and (not start-game) 
                     (reset-game x y)) (button-s)
                                       (set! roll-dice-i #t)
                                       (set! select-coin #f)
                                       (set! roll-dice-p #f)
                                       (set! roll-dice-c #f)
                                       (set! c-move #f)
                                       (set! 1st-possible-move #t)
                                       (initial-board)
                                       
                                       (set! game-page1 (create-image 
                                                         (vector->list v1) (vector->list v2)))
                                       (set! game-page game-page1)
                                       (set! page game-page)
                                       ]
               [(must-be-empty? v1 0 24) (set! game-page (place-image 
                                                          (text "PLAYER WON...RESET TO PLAY AGAIN"
                                                                50 "green") 500 250 game-page1))
                                         (set! page game-page)]
               [(must-be-empty? v2 0 24) (set! game-page (place-image 
                                                          (text "COMPUTER WON...RESET TO PLAY AGAIN"
                                                                50 "green") 500 250 game-page1))
                                         (set! page game-page)]
               
               [start-game (cond [(and (<= x 950) (>= x 850) (<= y 500) (>= y 400)) 
                                  (button-s)
                                  (set! start-game #f)
                                  (set! roll-dice-i #t)
                                  (set! page game-page)
                                  (set! game-page game-page1)]
                                 [(and (<= x 990) (>= x 770) (<= y 560) (>= y 500)) 
                                  (button-s)
                                  (set! start-game #f)
                                  (set! inst-page-open #t)
                                  (set! page inst-page)])]
               
               [(and inst-page-open (<= x 860) (>= x 740)
                     (<= y 510) (>= y 390))
                (button-s)
                (set! start-game #t)
                (set! inst-page-open #f)
                (set! page start-page)]
               
               [roll-dice-i (if (roll-dice-clicked x y)
                                (let* ((q (dice))
                                       (dice-val-p (car q))
                                       (dice-val-c (cdr q)))
                                  (button-s)
                                  (cond [(= dice-val-p dice-val-c)
                                         (show-dice 700 250 dice-val-p)
                                         (show-dice 300 250 dice-val-c)
                                         (set! game-page (place-image 
                                                          (text "OOPS! TIE ... TOSS AGAIN" 
                                                                50 "green") 500 250 game-page)) 
                                         (dice-s)
                                         (set! page game-page)
                                         (set! game-page game-page1)] 
                                        [(> dice-val-p dice-val-c)
                                         (set! roll-dice-i #f)
                                         (set! roll-dice-p #t)
                                         (show-dice 700 250 dice-val-p)
                                         (show-dice 300 250 dice-val-c)
                                         (set! game-page (place-image 
                                                          (text "PLAYER WON THE TOSS" 
                                                                50 "green") 500 250 game-page)) 
                                         (dice-s)
                                         (set! page game-page)
                                         (set! game-page game-page1)]
                                        [else (set! roll-dice-i #f)
                                              (set! roll-dice-c #t)
                                              (show-dice 700 250 dice-val-p)
                                              (show-dice 300 250 dice-val-c)
                                              (set! game-page (place-image 
                                                               (text "COMPUTER WON THE TOSS" 
                                                                     50 "green") 500 250 game-page))
                                              (dice-s)
                                              (set! page game-page)
                                              (set! game-page game-page1)
                                              ]))
                                (void))]
               [roll-dice-p 
                (if (roll-dice-clicked x y)
                    (let* ((pair (dice)))
                      (button-s)
                      (set! dice-val-p1 (maxl pair))
                      (set! dice-val-p2 (minl pair))
                      (set! dice-val dice-val-p1)
                      (set! player-pos (possible-next-moves
                                        dice-val v1 v2 #t))
                      (set! dice-val dice-val-p1)
                      (set! roll-dice-p #f)
                      (set! select-coin #t)
                      (show-dice 700 250 dice-val-p1)
                      (show-dice 850 250 dice-val-p2)
                      (show-arrow (map (λ(u) (car u)) player-pos))
                      
                      (if (null? player-pos)
                          (begin 
                            (display "no-1st-move")
                            (set! move 1)
                            (set! 1st-possible-move #f)
                            (set! game-page 
                                  (place-image (text "1ST DICE MOVE NOT POSSIBLE" 50 "green")
                                               500 250 game-page)))
                          (begin (set! 1st-possible-move #t)
                                 (set! move 1)))
                      (dice-s)
                      (set! page game-page)
                      (set! game-page game-page1))
                    
                    (void))]
               [roll-dice-c (let* ((p (dice))
                                   
                                   (dice-val-c1 (car p))
                                   (dice-val-c2 (cdr p)))
                              (begin (button-s)
                                     (set! p1 p) 
                                     (set! roll-dice-c #f)
                                     (set! c-move #t)
                                     
                                     (show-dice 200 250 dice-val-c1)
                                     (show-dice 350 250 dice-val-c2)
                                     (dice-s)
                                     (set! page (place-image (text 
                                                              "CLICK TO SEE COMPUTER MOVE" 
                                                              50 "green") 500 250 game-page))))]
               
               [c-move (button-s) 
                       (set! c-move #f)
                       (set! roll-dice-p #t)
                       (alpha-beta l1 #f 4 p1)
                       (set! game-page1 (create-image 
                                         (vector->list v1) (vector->list v2)))
                       (set! game-page game-page1)
                       (show-dice 200 250 (car p1))
                       (show-dice 350 250 (cdr p1))
                       (checker-s)
                       (set! page game-page)
                       
                       (set! game-page game-page1)
                       ]
               [select-coin 
                (cond [(= move 1) (if 1st-possible-move
                                      (let* ((pos-p (find-pos x y `w 1)))
                                        (set! final-val (assoc pos-p player-pos))
                                        (cond [final-val
                                               (move-coin v1 v2 pos-p (cdr final-val) #t)
                                               (set! game-page1 
                                                     (create-image (vector->list v1)
                                                                   (vector->list v2)))
                                               (set! game-page game-page1)
                                               (show-dice 700 250 dice-val-p1)
                                               (show-dice 850 250 dice-val-p2)]))
                                      (set! move 2)
                                      )
                                  (if (or final-val (not 1st-possible-move))
                                      (begin
                                        (set! player-pos 
                                              (possible-next-moves dice-val-p2 v1 v2 #t))
                                        (show-arrow (map (λ(u) (car u)) player-pos))
                                        (cond [(null? player-pos) 
                                               (set! move 2)
                                               (set! game-page 
                                                     (place-image 
                                                      (text "2ND DICE MOVE NOT POSSIBLE" 50 "green")
                                                      500 250 game-page))
                                               (set! select-coin #f)
                                               (set! roll-dice-c #t)])
                                        (checker-s)
                                        (set! page game-page)
                                        (set! game-page game-page1)
                                        (set! move 2))
                                      (void))]
                      [(= move 2)
                       (let* ((pos-p (find-pos x y `w 1))
                              (final-val (assoc pos-p player-pos)))
                         (cond [final-val
                                (move-coin v1 v2 pos-p (cdr final-val) #t)
                                (set! game-page1 
                                      (create-image (vector->list v1)
                                                    (vector->list v2)))
                                (set! game-page game-page1)
                                (checker-s)
                                (set! page (place-image (text 
                                                         "CLICK TO SEE COMPUTER DICE-ROLL" 
                                                         50 "indigo") 500 250 game-page))
                                
                                (set! move 1)
                                (set! select-coin #f)
                                (set! roll-dice-c #t)]))
                       ])])
         ]))


(big-bang 0
          (to-draw (show-page))
          (on-mouse mouse-event))

;;-----------END OF SOURCE CODE ------------------------------------------------------------------------





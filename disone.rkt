#lang racket
(require (planet clements/rsound))
(require 2htdp/image)
(require 2htdp/universe)





;; =================
;; Constants:

(define WIDTH 1100)
(define HEIGHT 700)
(define MTS (rectangle WIDTH HEIGHT "solid" "white"))
(define SHIP1 (circle 10 "solid" "green"))
(define SHIP2 (circle 10 "solid" "cyan"))

(define COIN (circle 5 "solid" "gold"))
(define SCORE 0)

(define MISSILE (circle 5 "solid" "red"))

(define MISSILE-SPEED 10)

(define PROX (sub1 (image-width SHIP1)))
(define SPEEDINC 5)
(define-struct pos (x y))
(define-struct now (ship1 ship2 missiles loc coin))
(define-struct loc (x y))
(define-struct coin (x y))
(define AIRRES 0.1)
(define MSPEED 40)
(define EASYNESS 12)
(define COINFREQ 500)
(define MOTION (/ 1 30))
(define TIME 1)
(define RANDW (random WIDTH))
(define RANDH (random HEIGHT))


;; two players, random missiles firing everywhere. last player standing wins. whoever gets the gold coin gains the power to eat the other player. 

;; =================
;; Data definitions:

(define-struct ship (x y vx vy disabled? superpowered?))
(define S1 (make-ship (* (/ WIDTH 2) 1.5) (/ HEIGHT 2) 0 0 false false))
(define S2 (make-ship (/ (/ WIDTH 2) 1.5) (/ HEIGHT 2) 0 0 false false))
(define-struct missile (x y vx vy))
(define START (make-now S1 S2 empty (make-loc 0 0) (make-coin (+ WIDTH 1) (+ HEIGHT 2))))


;; =================
;; Functions:

;; Now -> Now

;; ==========================================================================


(define (tock now)
  (local [(define ship1 (now-ship1 now))
          (define ship2 (now-ship2 now))
          (define missiles (now-missiles now))
          (define loc (now-loc now))
          (define coin (now-coin now))]
    (begin (set! TIME (add1 TIME))
           (cond [(false? (destroyed now))
                  (make-now (fn-for-ship ship1 now 10) (fn-for-ship ship2 now 20) (fn-for-missiles missiles now) (fn-for-loc loc now) (fn-for-coin coin now))]
                 [(= (destroyed now) 1) (make-now (make-ship 0 0 0 0 true false) (make-ship WIDTH HEIGHT 0 0 true false) (list (make-missile 0 0 0 0)) loc coin)]
                 [(= (destroyed now) 2) (make-now (make-ship WIDTH HEIGHT 0 0 true false) (make-ship 0 0 0 0 true false) (list (make-missile 0 0 0 0)) loc coin)]))))


(define (fn-for-ship ship now n)
  (local [(define x (ship-x ship))
          (define y (ship-y ship))
          (define vx (ship-vx ship))
          (define vy (ship-vy ship))
          (define disabled? (ship-disabled? ship))
          (define superpowered? (ship-superpowered? ship))
          (define coin (now-coin now))
          (define (get-power coin n)
            (and (= (+ n WIDTH) (coin-x coin)) (= (+ n HEIGHT) (coin-y coin))))]   
    (cond [(> x WIDTH) (make-ship 0 y vx vy disabled? (get-power coin n))]
          [(> y HEIGHT) (make-ship x 0 vx vy disabled? (get-power coin n))]
          [(< x 0) (make-ship WIDTH y vx vy disabled? (get-power coin n))]
          [(< y 0) (make-ship x HEIGHT vx vy disabled? (get-power coin n))]
          [(and (> vx 0) (> vy 0)) (make-ship (+ x vx) (+ y vy) (- vx AIRRES) (- vy AIRRES) disabled? (get-power coin n))]
          [(> vx 0) (make-ship (+ x vx) (+ y vy) (- vx AIRRES) vy disabled? (get-power coin n))]
          [(> vy 0) (make-ship (+ x vx) (+ y vy) vx (- vy AIRRES) disabled? (get-power coin n))]
          [(and (< vx 0) (> vy 0)) (make-ship (+ x vx) (+ y vy) (+ vx AIRRES) (+ vy AIRRES) disabled? (get-power coin n))]
          [(< vx 0) (make-ship (+ x vx) (+ y vy) (+ vx AIRRES) vy disabled? (get-power coin n))]
          [(< vy 0) (make-ship (+ x vx) (+ y vy) vx (+ vy AIRRES) disabled? (get-power coin n))]
          [else (make-ship (+ x vx) (+ y vy) vx vy disabled? (get-power coin n))])))

(define (fn-for-missiles missiles now)
  (local [(define loc (now-loc now))
          (define RANDW (random WIDTH))
          (define RANDH (random HEIGHT))
          (define RAND4 (random 4))
          (define RANDEDGE (cond  [(= RAND4 3) (make-pos (random WIDTH) 0)]
                                  [(= RAND4 2) (make-pos 0 (random HEIGHT))]
                                  [(= RAND4 1) (make-pos (random HEIGHT) 0)]
                                  [(= RAND4 0) (make-pos 0 (random HEIGHT))]))]
    
    (if (= (random EASYNESS) 1)
        (move-missiles (cons (make-missile (pos-x RANDEDGE) (pos-y RANDEDGE) (/ (- (loc-x loc) (pos-x RANDEDGE)) MSPEED) (/ (-  (loc-y loc) (pos-y RANDEDGE)) MSPEED)) missiles) loc)
        (move-missiles missiles loc))))

(define (fn-for-loc loc now)
  (local [(define ship1 (now-ship1 now))
          (define ship2 (now-ship2 now))]
    (if (= (random 2) 1)
        (make-loc (ship-x ship1) (ship-y ship1))
        (make-loc (ship-x ship2) (ship-y ship2)))))

(define (fn-for-coin coin now)
  (local [(define ship1 (now-ship1 now))
          (define ship2 (now-ship2 now))
          (define coinx (coin-x coin))
          (define coiny (coin-y coin))
          (define ship1x (ship-x ship1))
          (define ship2x (ship-x ship2))
          (define ship1y (ship-y ship1))
          (define ship2y (ship-y ship2))]
    (cond [(and (> coinx (- ship1x PROX)) (< coinx (+ ship1x PROX)) (> coiny (- ship1y PROX)) (< coiny (+ ship1y PROX))) (begin (play ding) (make-coin (+ WIDTH 10) (+ HEIGHT 10)))]
          [(and (> coinx (- ship2x PROX)) (< coinx (+ ship2x PROX)) (> coiny (- ship2y PROX)) (< coiny (+ ship2y PROX))) (begin (play ding) (make-coin (+ WIDTH 20) (+ HEIGHT 20)))]
          [else (if (= (random COINFREQ) 1)
                    (make-coin (random WIDTH) (random HEIGHT))
                    coin)])))


(define (destroyed now)
  (local [(define ship1 (now-ship1 now))
          (define ship2 (now-ship2 now)) 
          (define missiles (now-missiles now))]
    (cond [(or (missiles-in-contact? missiles ship1) (killed? ship2 ship1)) 1]
          [(or (missiles-in-contact? missiles ship2) (killed? ship1 ship2)) false]
          [else false])))

(define (killed? shipa shipb)
  (and (ship-superpowered? shipa) (< (ship-x shipa) (+ (ship-x shipb) PROX)) (> (ship-x shipa) (- (ship-x shipb) PROX))
       (< (ship-y shipa) (+ (ship-y shipb) PROX)) (> (ship-y shipa) (- (ship-y shipb) PROX))))


(define (missiles-in-contact? missiles shipa)
  (cond [(empty? missiles) false]
        [else (if  (and (< (missile-x (first missiles)) (+ (ship-x shipa) PROX)) (> (missile-x (first missiles)) (- (ship-x shipa) PROX))
                        (< (missile-y (first missiles)) (+ (ship-y shipa) PROX)) (> (missile-y (first missiles)) (- (ship-y shipa) PROX))) 
                   true
                   (missiles-in-contact? (rest missiles) shipa))]))

;; ==========================================================================

(define (move-missiles missiles loc)
  (cond [(empty? missiles) empty]
        [else
         (if (or (> (missile-x (first missiles)) WIDTH) (> (missile-y (first missiles)) HEIGHT) (< (missile-x (first missiles)) 0) (< (missile-y (first missiles)) 0))
             (move-missiles (rest missiles) loc)
             (cons (move-missile (first missiles) loc) (move-missiles (rest missiles) loc)))]))

(define (move-missile miss loc)
  (make-missile (+ (missile-x miss) (missile-vx miss)) (+ (missile-y miss) (missile-vy miss)) (missile-vx miss) (missile-vy miss)))


;; ==========================================================================

(define (rendernow now)
  (local [(define ship1 (now-ship1 now))
          (define ship2 (now-ship2 now))
          (define missiles (now-missiles now))
          (define coin (now-coin now))]
    (cond [(false? (destroyed now)) (cond [(ship-superpowered? ship1) (place-image (powerflash SHIP1) (ship-x ship1) (ship-y ship1) (place-image SHIP2 (ship-x ship2) (ship-y ship2) (place-image (2flash COIN) (coin-x coin) (coin-y coin) (place-missiles missiles))))]
                                          [(ship-superpowered? ship2) (place-image SHIP1 (ship-x ship1) (ship-y ship1) (place-image (powerflash SHIP2) (ship-x ship2) (ship-y ship2) (place-image (2flash COIN) (coin-x coin) (coin-y coin) (place-missiles missiles))))]
                                          [else (place-image SHIP1 (ship-x ship1) (ship-y ship1) (place-image SHIP2 (ship-x ship2) (ship-y ship2) (place-image (2flash COIN) (coin-x coin) (coin-y coin) (place-missiles missiles))))])]
          
          [(= (destroyed now) 2) (place-image (above (text "PLAYER1 WINS" 30 "GREEN")
                                                     (text "space to restart" 10 "black")) (/ WIDTH 2) (/ HEIGHT 2) MTS)]
          [(= (destroyed now) 1) (place-image (above (text "PLAYER2 WINS" 30 "CYAN")
                                                     (text "space to restart" 10 "black")) (/ WIDTH 2) (/ HEIGHT 2) MTS)])))

(define (place-missiles missiles)
  (cond [(empty? missiles) MTS]
        [else
         (place-image MISSILE (missile-x (first missiles)) (missile-y (first missiles)) (place-missiles (rest missiles)))]))

(define (2flash img)
  (if (= (modulo TIME 28) 0)
      empty-image
      img))

(define (powerflash img)
  (overlay (circle (modulo TIME 15) "outline" (make-color 225 0 0 225)) img))

;; ==========================================================================

(define (handle-key now ke)
  (local [(define ship1 (now-ship1 now))
          (define ship2 (now-ship2 now))
          (define missiles (now-missiles now))
          (define loc (now-loc now))
          (define coin (now-coin now))]
    (local [(define x1 (ship-x ship1))
            (define y1 (ship-y ship1))
            (define vx1 (ship-vx ship1))
            (define vy1 (ship-vy ship1))
            (define disabled?1 (ship-disabled? ship1))
            (define superpowered?1 (ship-superpowered? ship1))
            (define x2 (ship-x ship2))
            (define y2 (ship-y ship2))
            (define vx2 (ship-vx ship2))
            (define vy2 (ship-vy ship2))
            (define disabled?2 (ship-disabled? ship2))
            (define superpowered?2 (ship-superpowered? ship2))]
      
      
      
      (if (false? (or disabled?1 (ship-disabled? ship2)))
          (cond [(key=? ke " ") START]
                [(key=? ke "down") (make-now (make-ship x1 y1 vx1 (+ vy1 SPEEDINC) disabled?1 superpowered?1) ship2 missiles loc coin)]
                [(key=? ke "up") (make-now (make-ship x1 y1 vx1 (- vy1 SPEEDINC) disabled?1 superpowered?1) ship2 missiles loc coin)]
                [(key=? ke "left") (make-now (make-ship x1 y1 (- vx1 SPEEDINC) vy1 disabled?1 superpowered?1) ship2 missiles loc coin)]
                [(key=? ke "right") (make-now (make-ship x1 y1 (+ vx1 SPEEDINC) vy1 disabled?1 superpowered?1) ship2 missiles loc coin)]
                [(key=? ke "s") (make-now ship1 (make-ship x2 y2 vx2 (+ vy2 SPEEDINC)  disabled?2 superpowered?2) missiles loc coin)]
                [(key=? ke "w") (make-now ship1 (make-ship x2 y2 vx2 (- vy2 SPEEDINC)  disabled?2 superpowered?2) missiles loc coin)]
                [(key=? ke "a") (make-now ship1 (make-ship x2 y2 (- vx2 SPEEDINC) vy2  disabled?2 superpowered?2) missiles loc coin)]
                [(key=? ke "d") (make-now ship1 (make-ship x2 y2 (+ vx2 SPEEDINC) vy2  disabled?2 superpowered?2) missiles loc coin)]
                
                
                
                [else 
                 now])
          (if (key=? ke " ")
              START
              now)))))

;; ==========================================================================


(define (main now)
  (begin (play snare)
         (big-bang now                   
                   (on-tick   tock MOTION)
                   (to-draw   rendernow)
                   (on-key   handle-key))))

(main START)



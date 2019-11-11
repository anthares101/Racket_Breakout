;;Load necesary libraries
(require 2htdp/universe 2htdp/image)
(require racket/list)
(require racket/gui/base)


;;Necesary data structures
(define-struct ball (x y speed direction))
(define-struct world (ball))

;;Addcional getters for ball struct
(define (ball-Hspeed ball)
  (* (cos (ball-direction ball)) (ball-speed ball))
)

(define (ball-Vspeed ball)
  (* (sin (ball-direction ball)) (ball-speed ball))
)

;;Game atriibutes 
(define WIDTH 800)
(define HEIGHT 600)
(define INIT_BALL_SPEED 0)
(define INIT_BALL_DIR (/ pi 2)) ;;Initial ball direction is 90º
(define BALL_IMAGE (bitmap/file "ball.png")) ;;Ball image
(define BACKGROUND (bitmap/file "background.png")) ;;Background image

;;Initial game conditions
(define BALL (make-ball (/ WIDTH 2) (/ HEIGHT 2) INIT_BALL_SPEED INIT_BALL_DIR)) ;;Initial ball conditions

;;Create the inicial world
(define w (make-world BALL))


;;Render world 
(define (render-scene world)
 (draw-ball (world-ball world) BACKGROUND) 
)

;;Render objects functions
(define (draw-ball ball background)
   (place-image BALL_IMAGE (ball-x ball) (ball-y ball) BACKGROUND)
)


;;World evolution functions
;;Ball collision manager
(define (ball-wall-collide? ball)
  (cond
    ((> (ball-x ball) WIDTH) #t)
    ((> (ball-y ball) HEIGHT) #t)
    ((< (ball-y ball) 0) #t)
    ((< (ball-x ball) 0) #t)
    (else #f)
  )
)

(define (ball-collide ball)
  ;;Check if the ball hit a wall
  (cond
    ((ball-wall-collide? ball);;If it collide with someting the direction is recalculated
     (define direction (+ (* 2 (- pi (ball-direction ball))) (ball-direction ball)))
     (make-ball (ball-x ball) (ball-y ball) (ball-speed ball) direction)
    )
    (else (make-ball (ball-x ball) (ball-y ball) (ball-speed ball) (ball-direction ball)))
  )
)

;;Update ball state
(define (move-ball ball)
  (define new_ball (ball-collide ball));;Check for collisions
  (make-ball (- (ball-x new_ball) (ball-Hspeed new_ball)) (- (ball-y new_ball) (ball-Vspeed new_ball)) (ball-speed new_ball) (ball-direction new_ball))
)

;;Manage the world evolution
(define (progress-world world)
  (define NEW_BALL (move-ball (world-ball world)))

  (make-world NEW_BALL)
)


;;Just for testing
(define (end-game? world)
  #f
)


;;Start game
(define (start-game w)
  (define new_ball (make-ball (ball-x (world-ball w))
                           (ball-y (world-ball w))
                           5 ;;Initial speed
                           (ball-direction (world-ball w))
                )
  )

  (make-world new_ball)
)

;;Game keys manager
(define (deal-with-guess w key)
  (cond
        ((and (key=? key " ") (= (ball-speed (world-ball w)) 0)) (start-game w))
        (else w)
  )
)

  
;;Create the game manager
(define (game)
  (big-bang w
    (to-draw render-scene)
    (on-key deal-with-guess)
    (on-tick progress-world)
    (name "Racket_Breakout")
    (stop-when end-game?))
)

(game)












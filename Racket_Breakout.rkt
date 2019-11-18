;;Load necesary libraries
(require 2htdp/universe 2htdp/image)
(require racket/list)
(require racket/gui/base)


;;Necesary data structures
(define-struct ball (x y speed direction))
(define-struct bar (x y speed long))
(define-struct world (ball bar key_right key_left))

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
(define BAR_SPEED 8)
(define BAR_LONG (/ WIDTH 5))
(define INIT_BALL_SPEED 10)
(define INIT_BALL_DIR (/ pi 2)) ;;Initial ball direction is 90º
(define BALL_RADIUS 7.5) ;; Ball radius in pixels
(define BALL_IMAGE (bitmap/file "ball.png")) ;;Ball image
(define BACKGROUND (bitmap/file "background.png")) ;;Background image


;;Initial game conditions
(define BALL (make-ball (/ WIDTH 2) (/ HEIGHT 1.2) 0 INIT_BALL_DIR)) ;;Initial ball conditions
(define BAR (make-bar (/ WIDTH 2) (/ HEIGHT 1.09) BAR_SPEED BAR_LONG))

;;Create the inicial world
(define w (make-world BALL BAR #f #f))


;;Render world 
(define (render-scene world)
  (draw-ball (world-ball world)
             (draw-bar (world-bar world) BACKGROUND))
)

;;Render objects functions
(define (draw-ball ball background)
   (place-image BALL_IMAGE (ball-x ball) (ball-y ball) background)
)
(define (draw-bar bar background)
  (place-image (rectangle BAR_LONG 12 "solid" "blue") (bar-x bar) (bar-y bar) background)
)

;;World evolution functions
;;Ball wall collision manager
(define (ball-wall-collide_checker ball)
  (cond
    ((or (>= (ball-x ball) (- WIDTH BALL_RADIUS))
         (<= (ball-x ball) BALL_RADIUS))
     (- pi (ball-direction ball))
    )
    ((or (>= (ball-y ball) (- HEIGHT BALL_RADIUS))
         (<= (ball-y ball) BALL_RADIUS))
     (- (* 2 pi) (ball-direction ball))
    )
    (else #f)
  )
)

(define (ball-collide ball)
  ;;If the ball touch a wall the direction is recalculates
  (define new_direction (ball-wall-collide_checker ball))
  (cond
    (new_direction
     (make-ball (ball-x ball) (ball-y ball) (ball-speed ball) new_direction)
    )
    (else (make-ball (ball-x ball) (ball-y ball) (ball-speed ball) (ball-direction ball)))
  )
)

;;Update ball state
(define (move-ball ball)
  (define new_ball (ball-collide ball));;Check for collisions
  (make-ball (- (ball-x new_ball) (ball-Hspeed new_ball)) (- (ball-y new_ball) (ball-Vspeed new_ball)) (ball-speed new_ball) (ball-direction new_ball))
)

;;Move bar left
(define (move-bar-left bar)
  (make-bar (- (bar-x bar) BAR_SPEED)
            (bar-y bar)
            (bar-speed bar)
            (bar-long bar)
  )
)

;;Move bar right
(define (move-bar-right bar)
  (make-bar (+ (bar-x bar) BAR_SPEED)
            (bar-y bar)
            (bar-speed bar)
            (bar-long bar)
  )
)

;;Update bar state
(define (move-bar world)
  (cond
    ((and (world-key_right world) (world-key_left world)) (world-bar world))
    ((world-key_right world) (move-bar-right (world-bar world)))
    ((world-key_left world) (move-bar-left (world-bar world)))
    (else (world-bar world))
  )
)

;;Manage the world evolution
(define (progress-world world)
  (define new_ball (move-ball (world-ball world)))
  (define new_bar (move-bar world))

  (make-world new_ball new_bar (world-key_right world) (world-key_left world))
)


;;Just for testing
(define (end-game? world)
  #f
)


;;Start game
(define (start-game world)
  (define new_ball (make-ball (ball-x (world-ball world))
                              (ball-y (world-ball world))
                              INIT_BALL_SPEED
                              (ball-direction (world-ball world))
                   )
  )

  (make-world new_ball (world-bar world) (world-key_right world) (world-key_left world))
)

;;Game keys manager
(define (deal-with-guess world key)
  (cond
        ((and (key=? key " ") (= (ball-speed (world-ball world)) 0)) (start-game world))
        [(key=? key "left") (make-world (world-ball world) (world-bar world) (world-key_right world) #t)]
        [(key=? key "right") (make-world (world-ball world) (world-bar world) #t (world-key_left world))]
        (else world)
  )
)

(define (release-key world key)
  (cond
        [(key=? key "left") (make-world (world-ball world) (world-bar world) (world-key_right world) #f)]
        [(key=? key "right") (make-world (world-ball world) (world-bar world) #f (world-key_left world))]
        (else world)
  )
)

;;Create the game manager
(define (game)
  (big-bang w
    (to-draw render-scene)
    (on-key deal-with-guess)
    (on-release release-key)
    (on-tick progress-world)
    (name "Racket_Breakout")
    (stop-when end-game?))
)

(game)












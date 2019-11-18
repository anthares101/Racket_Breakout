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
(define INIT_BAR_X (/ WIDTH 2))
(define INIT_BAR_Y (/ HEIGHT 1.09))
(define INIT_BALL_X (/ WIDTH 2))
(define INIT_BALL_Y (/ HEIGHT 1.2))
(define INIT_BALL_SPEED 10)
(define INIT_BALL_DIR (/ pi 2)) ;;Initial ball direction is 90º
(define BALL_RADIUS 8.5) ;; Ball radius in pixels
(define BALL_IMAGE (bitmap/file "ball.png")) ;;Ball image
(define BACKGROUND (bitmap/file "background.png")) ;;Background image


;;Initial game conditions
(define BALL (make-ball INIT_BALL_X INIT_BALL_Y 0 INIT_BALL_DIR)) ;;Initial ball conditions
(define BAR (make-bar INIT_BAR_X INIT_BAR_Y BAR_SPEED BAR_LONG))

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
  (place-image (rectangle (bar-long bar) 12 "solid" "blue") (bar-x bar) (bar-y bar) background)
)

;;World evolution functions
;;Ball wall collision manager
(define (ball-collide_checker world)
  (cond
    ;;Wall collisions
    ((or (>= (ball-x (world-ball world)) (- WIDTH BALL_RADIUS));;Vertical collision
         (<= (ball-x (world-ball world)) BALL_RADIUS))
     (- pi (ball-direction (world-ball world)))
    )
    ((or (>= (ball-y (world-ball world)) (- HEIGHT BALL_RADIUS));;Horizontal collision
         (<= (ball-y (world-ball world)) BALL_RADIUS))
     (- (* 2 pi) (ball-direction (world-ball world)))
    )
    ;;Bar collisions
    ((and (>= (ball-y (world-ball world)) (- (bar-y (world-bar world)) (+ 6 BALL_RADIUS)))
          (<= (ball-y (world-ball world)) (+ (bar-y (world-bar world)) (+ 6 BALL_RADIUS)))
          (>= (ball-x (world-ball world)) (- (bar-x (world-bar world)) (+ (/ (bar-long (world-bar world)) 2) BALL_RADIUS)))
          (<= (ball-x (world-ball world)) (+ (bar-x (world-bar world)) (+ (/ (bar-long (world-bar world)) 2) BALL_RADIUS))))
     (let
         (;;Variables
          (posDiff (- (ball-x (world-ball world)) (bar-x (world-bar world))))
         )
       ;;Cuerpo
       (cond ;;Determine new ball direction according to the collision distance to center
         ((<= (abs posDiff) 3) (/ pi 2));;The ball bounce perpendicular to the bar
         ((< posDiff 0) (* (- 1 (/ (abs posDiff) (/ (bar-long (world-bar world)) 2))) (/ pi 2)));;The ball bounce with a certain angle to the right
         ((> posDiff 0) (- pi (* (- 1 (/ (abs posDiff) (/ (bar-long (world-bar world)) 2))) (/ pi 2))));;The ball bounce with a certain angle to the left
       )
     )
    )
    ;;No collision detected
    (else #f)
  )
)

(define (ball-collide world)
  ;;If the ball touch something the direction is recalculated
  (define new_direction (ball-collide_checker world))
  (cond
    (new_direction
     (make-ball (ball-x (world-ball world)) (ball-y (world-ball world)) (ball-speed (world-ball world)) new_direction)
    )
    (else (make-ball (ball-x (world-ball world)) (ball-y (world-ball world)) (ball-speed (world-ball world)) (ball-direction (world-ball world))))
  )
)

;;Update ball state
(define (move-ball world)
  (define new_ball (ball-collide world));;Check for collisions
  (make-ball (- (ball-x new_ball) (ball-Hspeed new_ball)) (- (ball-y new_ball) (ball-Vspeed new_ball)) (ball-speed new_ball) (ball-direction new_ball))
)

;;Move bar left
(define (move-bar-left bar)
  (make-bar (- (bar-x bar) (bar-speed bar))
            (bar-y bar)
            (bar-speed bar)
            (bar-long bar)
  )
)

;;Move bar right
(define (move-bar-right bar)
  (make-bar (+ (bar-x bar) (bar-speed bar))
            (bar-y bar)
            (bar-speed bar)
            (bar-long bar)
  )
)

;;Update bar state
(define (move-bar world)
  (cond
    ((and (world-key_right world) (world-key_left world)) (world-bar world))
    ((and (world-key_right world)
          (<= (+ (bar-x (world-bar world)) (/ (bar-long (world-bar world)) 2)) WIDTH)
     )
     (move-bar-right (world-bar world)))
    ((and (world-key_left world)
          (>= (- (bar-x (world-bar world)) (/ (bar-long (world-bar world)) 2)) 0) 
     )
     (move-bar-left (world-bar world)))
    (else (world-bar world))
  )
)

;;Manage the world evolution
(define (progress-world world)
  (define new_ball (move-ball world))
  (define new_bar (move-bar world))

  (make-world new_ball new_bar (world-key_right world) (world-key_left world))
)


;;If the ball touch the floor the game ends
(define (end-game? world)
  (cond
    ((>= (+ (ball-y (world-ball world)) BALL_RADIUS) HEIGHT) #t)
    (else #f)
  )
)


;;Start game
(define (start-game world)
  (define new_ball (make-ball (ball-x BALL)
                              (ball-y BALL)
                              INIT_BALL_SPEED
                              (ball-direction BALL)
                   )
  )

  (make-world new_ball (world-bar world) (world-key_right world) (world-key_left world))
)

;;Game keys manager
(define (deal-with-guess world key)
  (cond
       ;;(and (key=? key " ") (= (ball-speed (world-ball world)) 0))
        ((key=? key " ") (start-game world))
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












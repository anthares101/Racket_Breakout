;;Load necesary libraries
(require 2htdp/universe 2htdp/image)
(require racket/list)
(require racket/gui/base)


;;Necesary data structures
(define-struct ball (x y speed direction))
(define-struct world (ball))

;;Game atriibutes 
(define WIDTH 800)
(define HEIGHT 600)
(define INIT_BALL_SPEED 5)
(define INIT_BALL_DIR 90)
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
(define (move-ball ball)
  (make-ball (ball-x ball) (- (ball-y ball) (ball-speed ball)) (ball-speed ball) (ball-direction ball))
)

;;Manage the world evolution
(define (progress-world world) 
  (define NEW_BALL (move-ball (world-ball world)))

  (make-world NEW_BALL)
)

;;Just for testing
(define (end-game? world)
  (cond
    ((> (ball-x (world-ball world)) WIDTH) #t)
    ((> (ball-y (world-ball world)) HEIGHT) #t)
    ((< (ball-y (world-ball world)) 0) #t)
    ((< (ball-x (world-ball world)) 0) #t)
    (else #f)
  )
)

;;Create the game manager
(define (game)
  (big-bang w
    (to-draw render-scene)
    (on-tick progress-world)
    (name "Racket_Breakout")
    (stop-when end-game?))
)

(game)












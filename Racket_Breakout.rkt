;;Load necesary libraries
(require 2htdp/universe 2htdp/image)
(require racket/list)
(require racket/gui/base)


;;Necesary data structures
(define-struct ball (x y speed direction))
(define-struct bar (x y speed long))
(define-struct block (x y image))
(define-struct world (ball bar blocks lifes key_right key_left))

;;Addcional getters for ball struct
(define (ball-Hspeed ball)
  (* (cos (ball-direction ball)) (ball-speed ball))
)

(define (ball-Vspeed ball)
  (* (sin (ball-direction ball)) (ball-speed ball))
)

;;Game atriibutes
(define FINAL_FRAME #f)

(define WIDTH 800)
(define HEIGHT 600)

(define VICTORY_SCREEN (bitmap/file "resources/victory.png"))
(define DEFEAT_SCREEN (bitmap/file "resources/defeat.png"))

(define BLOCK_WIDTH 84)
(define BLOCK_HEIGHT 28)
(define BLOCK_ROWS 9)
(define BLOCK_COLS 8)
(define GROUP_BLOCKS_ORIGIN (+ (/ BLOCK_WIDTH 2) (/ (- WIDTH (* BLOCK_COLS BLOCK_WIDTH)) 2)))
(define BLUE_BLOCK_IMAGE (bitmap/file "resources/blue_block.png"))
(define RED_BLOCK_IMAGE (bitmap/file "resources/red_block.png"))
(define GREEN_BLOCK_IMAGE (bitmap/file "resources/green_block.png"))
(define YELLOW_BLOCK_IMAGE (bitmap/file "resources/yellow_block.png"))
(define BLOCK_LIST (list BLUE_BLOCK_IMAGE RED_BLOCK_IMAGE GREEN_BLOCK_IMAGE YELLOW_BLOCK_IMAGE))

(define BAR_SPEED 14)
(define BAR_LONG (/ WIDTH 5))
(define INIT_BAR_X (/ WIDTH 2))
(define INIT_BAR_Y (/ HEIGHT 1.09))

(define INIT_BALL_X (/ WIDTH 2))
(define INIT_BALL_Y (/ HEIGHT 1.2))
(define INIT_BALL_SPEED 8)
(define MAX_BALL_SPEED 12)
(define INIT_BALL_DIR (/ pi 2)) ;;Initial ball direction is 90º
(define BALL_RADIUS 9) ;; Ball radius in pixels
(define BALL_IMAGE (bitmap/file "resources/ball.png")) ;;Ball image

(define INIT_LIFES 3) ;;Initial player lifes
(define HEART_IMAGE (bitmap/file "resources/heart.png"))
(define HEART_WIDTH 38)
(define BACKGROUND (bitmap/file "resources/background.png")) ;;Background image


;;Level generation
(define (make-blocks)
  (do
      ;;Variables del bucle
      (
       (i 0 (cond
              ((= j 0) (+ i 1))
              (else i)
            ))
       (j 1 (modulo (+ j 1) BLOCK_COLS))
       (colour (random (length BLOCK_LIST)) (random (length BLOCK_LIST)))
       (pos_x GROUP_BLOCKS_ORIGIN (cond
                                    ((= j 0) GROUP_BLOCKS_ORIGIN)
                                    (else (+ pos_x BLOCK_WIDTH))
                                   ))
       (pos_y (* 2 BLOCK_HEIGHT) (cond
                                   ((= j 0) (+ pos_y BLOCK_HEIGHT))
                                   (else pos_y)
                                 ))
       (result (list) (append result (list (make-block pos_x pos_y colour))))
      )
    ;;Condicion y sentencia de salida
    ((= i BLOCK_ROWS) result)
  )
)


;;Initial game conditions
(define BALL (make-ball INIT_BALL_X INIT_BALL_Y 0 INIT_BALL_DIR)) ;;Initial ball conditions
(define BAR (make-bar INIT_BAR_X INIT_BAR_Y BAR_SPEED BAR_LONG))
(define BLOCKS (make-blocks))

;;Create the inicial world
(define w (make-world BALL BAR BLOCKS INIT_LIFES #f #f))


;;Render world 
(define (render-scene world)
  (cond
    ((= (world-lifes world) 0)
     (set! FINAL_FRAME #t)
     
     (place-image DEFEAT_SCREEN (/ WIDTH 2) (/ HEIGHT 2)
                  (draw-lifes (world-lifes world) HEART_WIDTH
                              (draw-ball (world-ball world)
                                         (draw-bar (world-bar world)
                                                   (draw-blocks (world-blocks world) BACKGROUND)))))
    )
    ((null? (world-blocks world))
     (set! FINAL_FRAME #t)
     
     (place-image VICTORY_SCREEN (/ WIDTH 2) (/ HEIGHT 2)
                  (draw-lifes (world-lifes world) HEART_WIDTH
                              (draw-ball (world-ball world)
                                         (draw-bar (world-bar world)
                                                   (draw-blocks (world-blocks world) BACKGROUND)))))
    )
    (else
     (draw-lifes (world-lifes world) HEART_WIDTH
                 (draw-ball (world-ball world)
                            (draw-bar (world-bar world)
                                      (draw-blocks (world-blocks world) BACKGROUND))))
    )
  )
)

;;Render objects functions
(define (draw-ball ball background)
   (place-image BALL_IMAGE (ball-x ball) (ball-y ball) background)
)

(define (draw-bar bar background)
  (place-image (rectangle (bar-long bar) 12 215 "grey") (bar-x bar) (bar-y bar) background)
)

(define (draw-blocks blocks background)
  (cond ((empty? blocks)  background)
        (else (draw-block (car blocks) (draw-blocks (cdr blocks) background)))
  )
)

(define (draw-block block background)
  (place-image (list-ref BLOCK_LIST (block-image block)) (block-x block) (block-y block) background)
)

(define (draw-life x background)
  (place-image HEART_IMAGE x 20 background)
)

(define (draw-lifes lifes x background)
   (cond ((<= lifes 0)  background)
         (else (draw-life x (draw-lifes (- lifes 1) (+ x HEART_WIDTH 5) background)))
   )
)


;;World evolution functions
;;Determine if the ball collide with a block and recalculate the direction if necesary
(define (ball-collide_block ball blocks)
  (cond
    ((null? blocks) #f)
    ((and (>= (ball-y ball) (- (block-y (car blocks)) (+ (/ BLOCK_HEIGHT 2) BALL_RADIUS)))
          (<= (ball-y ball) (+ (block-y (car blocks)) (+ (/ BLOCK_HEIGHT 2) BALL_RADIUS)))
          (>= (ball-x ball) (- (block-x (car blocks)) (+ (/ BLOCK_WIDTH 2) BALL_RADIUS)))
          (<= (ball-x ball) (+ (block-x (car blocks)) (+ (/ BLOCK_WIDTH 2) BALL_RADIUS))))
     (play-sound "resources/sound/bounce2.wav" #t)
     (cond ;;Determine from where the ball hit the block
       ((and (>= (ball-x ball) (- (block-x (car blocks)) (/ BLOCK_WIDTH 2)))
             (<= (ball-x ball) (+ (block-x (car blocks)) (/ BLOCK_WIDTH 2))))
        (- (* 2 pi) (ball-direction ball)) ;;Horizontal collision
       )
       ((and (>= (ball-y ball) (- (block-y (car blocks)) (/ BLOCK_HEIGHT 2)))
             (<= (ball-y ball) (+ (block-y (car blocks)) (/ BLOCK_HEIGHT 2))))
        (- pi (ball-direction ball)) ;;Vertical collision
       )
       (else (+ (ball-direction ball) pi));;Corner
     )
    )
    (else (ball-collide_block ball (cdr blocks)))
  )
)

;;Ball collision manager
(define (ball-collide_checker world)
  (cond
    ;;Wall collisions
    ((or (>= (ball-x (world-ball world)) (- WIDTH BALL_RADIUS));;Vertical collision
         (<= (ball-x (world-ball world)) BALL_RADIUS))
     (play-sound "resources/sound/bounce1.wav" #t)
     (- pi (ball-direction (world-ball world)))
    )
    ((or (>= (ball-y (world-ball world)) (- HEIGHT BALL_RADIUS));;Horizontal collision
         (<= (ball-y (world-ball world)) BALL_RADIUS))
     (- (* 2 pi) (ball-direction (world-ball world)))
     (play-sound "resources/sound/bounce1.wav" #t)
    )
    ;;Bar collisions
    ((and (>= (ball-y (world-ball world)) (- (bar-y (world-bar world)) (+ 6 BALL_RADIUS)))
          (<= (ball-y (world-ball world)) (+ (bar-y (world-bar world)) (+ 6 BALL_RADIUS)))
          (>= (ball-x (world-ball world)) (- (bar-x (world-bar world)) (+ (/ (bar-long (world-bar world)) 2) BALL_RADIUS)))
          (<= (ball-x (world-ball world)) (+ (bar-x (world-bar world)) (+ (/ (bar-long (world-bar world)) 2) BALL_RADIUS))))
     (play-sound "resources/sound/bounce3.wav" #t)
     (let
         (;;Variables
          (posDiff (- (ball-x (world-ball world)) (bar-x (world-bar world))))
         )
       ;;Cuerpo
       (cond ;;Determine new ball direction according to the collision distance to center
         ((< (abs posDiff) 0.001) (/ pi 2));;The ball bounce perpendicular to the bar
         ((< posDiff 0) (* (- 1 (/ (abs posDiff) (/ (bar-long (world-bar world)) 2))) (/ pi 2)));;The ball bounce with a certain angle to the right
         ((> posDiff 0) (- pi (* (- 1 (/ (abs posDiff) (/ (bar-long (world-bar world)) 2))) (/ pi 2))));;The ball bounce with a certain angle to the left
       )
     )
    )
    ;;Block collisions
    (else
     (ball-collide_block (world-ball world) (world-blocks world))
    )
  )
)

;;Recalculate the ball direction if a collision happens and increase ball speed
(define (ball-collide world)
  ;;If the ball touch something the direction is recalculated
  (define new_direction (ball-collide_checker world))
  (cond
    (new_direction
     (if (< (ball-speed (world-ball world)) MAX_BALL_SPEED)
         (make-ball (ball-x (world-ball world)) (ball-y (world-ball world)) (+ (ball-speed (world-ball world)) 0.2) new_direction)
         (make-ball (ball-x (world-ball world)) (ball-y (world-ball world)) (ball-speed (world-ball world)) new_direction)
     )
    )
    (else (make-ball (ball-x (world-ball world)) (ball-y (world-ball world)) (ball-speed (world-ball world)) (ball-direction (world-ball world))))
  )
)

;;Update ball state
(define (move-ball world)
  (cond
    ((or (= (world-lifes world) 0) (null? (world-blocks world))) (world-ball world))
    ((ball-fallen? (world-ball world)) BALL)
    (else
     (define new_ball (ball-collide world));;Check for collisions
     (make-ball (- (ball-x new_ball) (ball-Hspeed new_ball)) (- (ball-y new_ball) (ball-Vspeed new_ball)) (ball-speed new_ball) (ball-direction new_ball))
    )
  )
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

;;Update blocks
(define (update-blocks blocks ball)
  (cond
    ((null? blocks) blocks)
    ((and (>= (ball-y ball) (- (block-y (car blocks)) (+ (/ BLOCK_HEIGHT 2) BALL_RADIUS)))
          (<= (ball-y ball) (+ (block-y (car blocks)) (+ (/ BLOCK_HEIGHT 2) BALL_RADIUS)))
          (>= (ball-x ball) (- (block-x (car blocks)) (+ (/ BLOCK_WIDTH 2) BALL_RADIUS)))
          (<= (ball-x ball) (+ (block-x (car blocks)) (+ (/ BLOCK_WIDTH 2) BALL_RADIUS))))
     (update-blocks (cdr blocks) ball)
    )
    (else (cons (car blocks) (update-blocks (cdr blocks) ball)))
  )
)

(define (ball-fallen? ball)
  (>= (+ (ball-y ball) BALL_RADIUS) HEIGHT)
)

;;Update player lifes
(define (update-lifes world)
  (if (and (ball-fallen? (world-ball world)) (> (world-lifes world) 0))
      (- (world-lifes world) 1)
      (world-lifes world)
  )
)

;;Manage the world evolution
(define (progress-world world)
  (define new_lifes (update-lifes world))
  (define new_bar (move-bar world))
  ;;Necesary to avoid the ball from restart his position if the game has ended or bounce wrong on the bar
  (define new_ball (move-ball (make-world
                               (world-ball world)
                               new_bar
                               (world-blocks world)
                               new_lifes
                               (world-key_right world)
                               (world-key_left world))))
  (define new_blocks (update-blocks (world-blocks world) (world-ball world)))
  
  (make-world new_ball new_bar new_blocks new_lifes (world-key_right world) (world-key_left world))
)


;;If the ball touch the floor or all the blocks are destroyed the game ends
(define (end-game? world) FINAL_FRAME)

;;Start game
(define (start-game world)
  (cond
    ((and (= (ball-speed (world-ball world)) 0) (> (world-lifes world) 0) (not (null? (world-blocks world))))
     (define new_ball (make-ball (ball-x (world-ball world))
                                 (ball-y (world-ball world))
                                 INIT_BALL_SPEED
                                 (ball-direction (world-ball world))
                      )
     )

     (make-world new_ball (world-bar world) (world-blocks world) (world-lifes world) (world-key_right world) (world-key_left world))
    )
    (else world)
  )
)

;;Game keys manager
(define (deal-with-guess world key)
  (cond
       ;;(and (key=? key " ") (= (ball-speed (world-ball world)) 0))
        ((key=? key " ") (start-game world))
        [(key=? key "left") (make-world (world-ball world) (world-bar world) (world-blocks world) (world-lifes world) (world-key_right world) #t)]
        [(key=? key "right") (make-world (world-ball world) (world-bar world) (world-blocks world) (world-lifes world) #t (world-key_left world))]
        (else world)
  )
)

(define (release-key world key)
  (cond
        [(key=? key "left") (make-world (world-ball world) (world-bar world) (world-blocks world) (world-lifes world) (world-key_right world) #f)]
        [(key=? key "right") (make-world (world-ball world) (world-bar world) (world-blocks world) (world-lifes world) #f (world-key_left world))]
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












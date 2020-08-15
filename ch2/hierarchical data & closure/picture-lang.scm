#lang scheme

;; For creating a picture language that allows us to transform pictures, we build on base abstractions
;; As a starting point, we create primitives such as above, below, beside that allow
;;    us to display images next to one another.
;; We then continue for other transformations like direction-splitting to create
;;    images that are placed in a given direction relative to itself at half-size on each
;;    recursive call

(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define wave4 (below wave2 wave2))

(define (right-split painter n)
  (if (= n 0) painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0) painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter))) (below (flip-vert half) half))))

;; 2.44: define up-split
(define (up-split painter n)
  (if (= n 0) painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;; 2.45: generalized split
(define (split primary secondary)
  (lambda (painter n)
    (if (= n 0) painter
    (let ((smaller ((split primary secondary) painter n)))
      (primary painter (secondary smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))

;; 2.46: vector abstractions
(define (make-vect x y)
  (list x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cadr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (subt-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect v k)
  (make-vect (* k (xcor-vect v))
             (* k (ycor-vect v))))

;; 2.47: frame abstractions
;; implementation 1
;; constructor
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
;; selectors
(define (get-origin frame)
  (car frame))
(define (get-edge1 frame)
  (cadr frame))
(define (get-edge2 frame)
  (caddr frame))

;; implementation 2
;; constructor
(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
;; selectors - get-origin and get-edge1 are the same, get-edge2 is different here
(define (get-origin2 frame)
  (car frame))
(define (get-edge12 frame)
  (cadr frame))
(define (get-edge22 frame)
  (cddr frame))

;; 2.48: directed-line segment abstraction with vectors
;; constructor
(define (make-segment start end)
  (make-vect start end))

;; selectors
(define (start-segment line)
  (car line))

(define (end-segment line)
  (cadr line))

;; 2.49: define following primitives for segments->painter
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line ((frame-coord-map frame) (start-segment segment))
                  ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

;; first, let's define some important primitives for frames, segments -> get-corner, get-midpoint
(define (get-corner frame)
  (add-vect (get-edge1 frame) (get-edge2 frame)))

(define (get-midpoint line)
  (scale (add-vect (start-segment line) (end-segment line)) 0.5))
;; a: outline of frame
(define (frame-outline frame)
  ((segments-painter
    (list (make-segment (get-origin frame) (get-edge1 frame))
          (make-segment (get-edge1 frame) (get-corner frame))
          (make-segment (get-corner frame) (get-edge2 frame))
          (make-segment (get-edge2 frame)) (get-origin frame)))
   frame))
;; b: X in frame
(define (frame-x frame)
  ((segments-painter
    (list (make-segment (get-origin frame) (get-corner frame))
          (make-segment (get-edge1 frame) (get-edge2 frame))))
   frame))
;; c: diamond that connects midpoints of frame
(define (frame-outline frame)
  ((segments-painter
    (list (make-segment
           (get-midpoint (make-segment (get-origin frame) (get-edge1 frame)))
           (get-midpoint (make-segment (get-edge1 frame) (get-corner frame))))
          (make-segment
           (get-midpoint (make-segment (get-edge1 frame) (get-corner frame)))
           (get-midpoint (make-segment (get-corner frame) (get-edge2 frame))))
          (make-segment
           (get-midpoint (make-segment (get-corner frame) (get-edge2 frame)))
           (get-midpoint (make-segment (get-edge2 frame) (get-origin frame))))
          (make-segment
           (get-midpoint (make-segment (get-edge2 frame) (get-origin frame)))
           (get-midpoint (make-segment (get-origin frame) (get-edge1 frame))))))
   frame))
                         
;; d: wave painter
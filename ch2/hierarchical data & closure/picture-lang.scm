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
;; selectors
(define (get-origin2 frame)
  (car frame))
(define (get-edge12 frame)
  (cadr frame))
(define (get-edge22 frame)
  (cddr frame))

               
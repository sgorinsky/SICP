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
    (below painter2 painter2)))(define wave4 (below wave2 wave2))

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

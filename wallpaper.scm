(use-modules (ice-9 binary-ports)
             (srfi srfi-1))

(set! *random-state* (random-state-from-platform)) ; Enables random state

;; Simply a placeholder list that contains gruvbox colors
(define color-list
  (list
   #vu8(251 73 27)
   #vu8(250 189 47)
   #vu8(131 165 152)
   #vu8(211 134 155)
   #vu8(142 192 124)
   #vu8(254 128 25)
   #vu8(184 187 38)))

;; Random color index
(define (make-next-color! n)
  "Creates a counter which takes a number, increments it by one, and returns the number.

(define a (make-next-color! 1))
(a)
=> 1
(a)
=> 2"
  (lambda ()
    (define last-n n)
    (set! n (+ n 1))
    last-n))

(define next-color!
  (make-next-color! (random 100)))


;; Map color index to color
(define (get-color colors number)
  "Glorified \"Get value at index\" function. Takes a list of colors and the index. Wraps the index around the list to make sure it doesn't overflow.

(get-color (list '(1 2 3) '(2 3 4)) 0)
=> (1 2 3)
(get-color (list '(1 2 3) '(2 3 4)) 100)
=> (1 2 3)"
  (if (> number (floor-remainder number (length colors)))
      (get-color colors (floor-remainder number (length colors)))
      (if (= number 0)
          (car colors)
          (get-color (cdr colors) (- number 1)))))

;; Generate random (x . y) pairs for seeds
(define (random-position max-width max-height)
  "Takes a maximum (x,y) and returns a list containing (x . y)

(random-position 100 100)
=> (39 . 18)"
  (cons (random max-width) (random max-height)))

;; Create Image
(define (create-seeds number max-width max-height colors)
  "Takes the number of seeds, an x and y maximum, and a list of colors to iterate over.
It returns a list of format: \"(((x . y) color) ((x . y) color))\"

(create-seeds 3 10 10 '(1 2))
=> (((5 . 1) 1) ((9 . 9) 2) ((4 . 0) 1))"
    (if (= number 1)
      (list (cons
             (random-position max-width max-height)
             (list (get-color colors (next-color!)))))
      (cons (cons
             (random-position max-width max-height)
             (list (get-color colors (next-color!)))) (create-seeds (- number 1) max-width max-height colors))))

(define (get-closest-seed pixel seeds formula)
  (define (inner pixel seeds formula shortest)
    (if (null? seeds)
        shortest
        (if (> (formula pixel (car shortest))
               (formula pixel (car (car seeds))))
            (inner pixel (cdr seeds) formula (car seeds))
            (inner pixel (cdr seeds) formula shortest))))

  (inner pixel seeds formula (car seeds)))

;; Version that uses guile custom arguments, though really isn't needed.
;; (define* (get-closest-seed pixel seeds formula #:optional shortest)
;;   (if (eq? shortest #f)
;;       (get-closest-seed pixel (cdr seeds) formula (car seeds))
;;       (if (null? seeds)
;;           shortest
;;           (if (> (formula pixel (car shortest))
;;                  (formula pixel (car (car seeds))))
;;               (get-closest-seed pixel (cdr seeds) formula (car seeds))
;;               (get-closest-seed pixel (cdr seeds) formula shortest)))))

(define (build-image seeds max-width max-height output-port formula)
  "Creates an image by iterating over all of the pixels, comparing distances through the formula, sets their color, and writes it to the file"

  (define (build-image-column height)
    "Iterates over each height, building each image row-by-row"
    (if (< height max-height)
        (begin
          (build-image-row 0 height)
          (build-image-column (+ height 1)))))

  (define (build-image-row width height)
    "Iterates over the whole row, and writes each pixel individually"
    (if (< width max-width)
        (begin
          (put-bytevector output-port (cadr (get-closest-seed (cons width height) seeds formula)))
          (build-image-row (+ width 1) height))))

  (build-image-column 0))

(define (create-image seed-num width height colors formula path)
  "Runs through the steps of creating a PPM image, calling all of the functions"
    (initialize-file width height path)
    (define seeds (create-seeds seed-num width height colors))
    (define output-port (open-file path "a"))
    (build-image seeds width height output-port formula))

(define (initialize-file width height path)
  "Writes out boilerplate for a PPM file"
  (let ((output-port (open-file path "w")))
    (display "P6" output-port)
    (newline output-port)
    (display width output-port)
    (newline output-port)
    (display height output-port)
    (newline output-port)
    (display 255 output-port)
    (newline output-port)
    (close output-port)))

;; Calculate Distances
(define (calculate-manhattan-distance start end)
  "Takes two points (start-x . start-y) and (end-x . end-y), and returns the distance between the two, using Manhattan Distance

(calculate-manhattan-distance '(1 . 2) '(3 . 4))
=> 4"
    (+ (abs
        (- (car start) (car end)))
       (abs
        (- (cdr start) (cdr end)))))

(define (calculate-euclidean-distance start end)
  "Takes two points (start-x . start-y) and (end-x . end-y), and returns the euclidean distance squared (which is fine for distance comparison)

(calculate-euclidean-distance '(1 . 2) '(3 . 4))
=> 8"
  (+ (*
      (- (car start) (car end))
      (- (car start) (car end)))
     (*
      (- (cdr start) (cdr end))
      (- (cdr start) (cdr end)))))

;; Generates image
(create-image 50 1920 1080 color-list calculate-manhattan-distance "/tmp/wallpaper.ppm")

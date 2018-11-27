"Parallel Image Darkening"

(defvar type 0)
(defvar width 0)
(defvar height 0)
(defvar data ())



;; set the data values for the image
(defun loadImage (filename)

  (setq fileList ())
  (setq fs (open filename :element-type 'unsigned-byte))

  (setq keepGoing t)
  (loop while keepGoing do
    ;; load the fileList
    (setq b (read-byte fs nil 'eof))
    (cond
      ((eql b 'eof) (setq keepGoing nil))
      (t (setq fileList (append fileList (cons b nil))))
    )
  )

  (close fs)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; set the image data based on the fileList
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; set image type (color or gray)
  (cond
    ((= (nth 1 fileList) 50) (setq type 0))
    ((= (nth 1 fileList) 53) (setq type 0))
    (t (setq type 1))
  )

  ;; skip comments
  (setq index 3)
  (loop while (= (nth index fileList) 35) do
    (loop while (/= (nth index fileList) 10) do
      (setq index (+ index 1))
    )
    (setq index (+ index 1))
  )

  ;; get width and height numbers
  (setq widthVector ())
  (loop while (/= (nth index fileList) 32) do
    (setq widthVector (append widthVector (cons (- (nth index fileList) 48) nil)))
    (setq index (+ index 1))
  )
  (setq index (+ index 1))

  (setq heightVector ())
  (loop while (/= (nth index fileList) 32) do
    (setq heightVector (append heightVector (cons (- (nth index fileList) 48) nil)))
    (setq index (+ index 1))
  )
  (setq index (+ index 1))

  ;; convert vectors to int
  (setq width 0)
  (setq i (- (list-length widthVector) 1))
  (loop while (>= i 0) do
    (setq width (+ width (* (nth i widthVector) (expt 10 (- (- (list-length widthVector) 1) i)))))
    (setq i (- i 1))
  )

  (setq height 0)
  (setq i (- (list-length heightVector) 1))
  (loop while (>= i 0) do
    (setq height (+ height (* (nth i heightVector) (expt 10 (- (- (list-length heightVector) 1) i)))))
    (setq i (- i 1))
  )

  ;; skip past max value
  (loop while (/= (nth index fileList) 10) do
    (setq index (+ index 1))
  )
  (setq index (+ index 1))

  ;; copy pixel data to data list
  (loop while (< index (list-length fileList)) do
    (setq data (append data (cons (nth index fileList) nil)))
  )

)


(defun darken (pixel)
  (cond
    ((< (- pixel 30) 0) 0)
    (t (- pixel 30))
  )
)


(defun darkenImage (L)
  (cons
    (darken (car L))
    (darkenImage (cdr L))
  )
)


(loadImage "image.pnm")
;; (darkenImage data)
;; (saveImage "darkenedImage.pnm")

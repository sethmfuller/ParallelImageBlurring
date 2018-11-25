"Parallel Image Darkening"


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
(darkenImage data)
(saveImage "darkenedImage.pnm")


"final command"
(saveImage
  "darkenedImage.pnm"
  height
  width
  (darkenImage
    (loadImage "image.pnm")
  )
)

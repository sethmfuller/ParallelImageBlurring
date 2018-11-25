"Parallel Image Darkening"




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



"final command"
(saveImage
  "darkenedImage.pnm"
  height
  width
  (darkenImage
    (loadImage "image.pnm")
  )
)

;-- DIB class
;-- Device Independant Bitmap in Win32
;-- dib-clear must be called to delete dib !!!

(in-package :common-graphics-user)

(defclass dib ()
  ((p
    :accessor p
    :initarg :p
    :initform nil)
   (dc
    :accessor dc
    :initarg :dc
    :initform nil)
   (bmp
    :accessor bmp
    :initarg :bmp
    :initform nil)
   (w
    :accessor w
    :initarg :w
    :initform 0)
   (h
    :accessor h
    :initarg :h
    :initform 0)
   (flipy
    :accessor flipy
    :initarg :flipy
    :initform nil)      
   ))


(defmethod print-object ((o dib) stream)
  (format stream "<DIB p:~S dc:~S bmp:~S w:~S h:~S flipy:~S>" (p o) (dc o) (bmp o) (w o) (h o) (flipy o)))

;-- Allocate DIB section
(defmethod initialize-instance :after ((b dib) &rest initargs)         
  (when (and (plusp (w b)) (plusp (h b)))    
    (let ((bmp-info (ff:allocate-fobject 'win:bitmapinfo))        
          (buf      (make-array 1 :element-type '(unsigned-byte 32) :initial-element 0))) ; Hold Pointer of Pointer      
      (setf (w b) (ceiling (w b)))
      (setf (h b) (ceiling (h b)))            
      (setf 
       (ff:fslot-value bmp-info 'bmiHeader 'biSize)       #x28
       (ff:fslot-value bmp-info 'bmiHeader 'biWidth)        (w b)
       (ff:fslot-value bmp-info 'bmiHeader 'biHeight)       (h b)
       (ff:fslot-value bmp-info 'bmiHeader 'biPlanes)        1
       (ff:fslot-value bmp-info 'bmiHeader 'biBitCount)     32
       (ff:fslot-value bmp-info 'bmiHeader 'biCompression)   0
       (ff:fslot-value bmp-info 'bmiHeader 'biSizeImage)     0
       (ff:fslot-value bmp-info 'bmiHeader 'biXPelsPerMeter) 0
       (ff:fslot-value bmp-info 'bmiHeader 'biYPelsPerMeter) 0
       (ff:fslot-value bmp-info 'bmiHeader 'biClrUsed)       0
       (ff:fslot-value bmp-info 'bmiHeader 'biClrImportant)  0)
      (setf (dc  b) (win:createcompatibledc 0))
      (setf (bmp b) (createdibsection (dc b) bmp-info 0 buf 0 0))
      (setf (p   b) (aref buf 0))
      (win:selectobject (dc b) (bmp b))      
      )))



;-- Clear DIB section
(defmethod dib-clear ((b dib))  
  (when (bmp b) 
    (win:deleteobject (bmp b))
    (setf (bmp b) nil))
  (when (dc b)
    (win:deleteobject (dc  b))
    (setf (dc b) nil))  
  (setf (p b) nil)
  (setf (w b) 0)
  (setf (h b) 0))

;-- validity check
(defun dib-valid (b)
  (if (and b (typep b 'dib) 
           (p   b) (plusp (p b))
           (dc  b) 
           (bmp b) (plusp (bmp b))
           (integerp (w b)) (plusp (w b)) 
           (integerp (h b)) (plusp (h b))) t nil))







;-- Save AREA of DIB to PNG
(defmethod dib-to-png ((b dib) x1 y1 x2 y2 fn)    
  (let  ((pos   0)
         (alpha 0)
         (pitch (* 4 (w b)))         
         (png   nil))    
    (declare (fixnum pos alpha pitch x1 y1 x2 y2 ))
    
    ;-- Bound check
    (when (> x1 x2)    (rotatef x1 x2))
    (when (> y1 y2)    (rotatef y1 y2))
    (when (minusp x1)  (setf x1 0))
    (when (minusp y1)  (setf y1 0))
    (when (> x2 (w b)) (setf x2 (1- (w b))))
    (when (> y2 (h b)) (setf y2 (1- (h b))))
    
    ;-- Write to File ( R G B A )
    (setf png (make-instance 'zpng:pixel-streamed-png :color-type :truecolor-alpha :width (- x2 x1) :height (- y2 y1)))       
    (with-open-file (stream fn :direction :output :if-exists :supersede :if-does-not-exist :create :element-type '(unsigned-byte 8))              
      (zpng:start-png png stream)                                                      
      (do ((y y1 (+ y 1))) 
          ((>= y y2))        
        (setf pos (+ (* y pitch) (* x1 4)))   
        (do ((x x1 (+ x 1))) 
            ((>= x x2))
          (declare (fixnum x y))
          (incf pos 4)                    
          (setf alpha (ff:fslot-value-typed '(:array :unsigned-char) nil (p b) (+ pos 3)))
          (zpng:write-pixel 
           (if (zerop alpha) (list 0 0 0 0)
             (list 
              (ff:fslot-value-typed '(:array :unsigned-char) nil (p b) (+ pos 2)) 
              (ff:fslot-value-typed '(:array :unsigned-char) nil (p b) (+ pos 1)) 
              (ff:fslot-value-typed '(:array :unsigned-char) nil (p b) (+ pos 0)) 
              alpha
              )) 
           png)                                                            
          ))                                
      (zpng:finish-png png))
    t))


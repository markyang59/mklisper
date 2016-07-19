(in-package :common-graphics-user)


;-- convert to double float
(defmacro to-double (x)  `(coerce ,x 'double-float))
(defmacro to-float  (x)  `(coerce ,x 'single-float))

;-- Swap object in list
(declaim (inline swap move-down move-up))
(defun swap (lst pa pb)
  (declare (list lst) (fixnum pa pb))
  (let ((tp nil))
    (setf 
     tp (nth pa lst)
     (nth pa lst) (nth pb lst)
     (nth pb lst) tp
     )))

;-- MOVE object in list
(defun move-down (lst p)
  (declare (list lst) (fixnum p))
  (when (and lst (>= p 0) (< p (1- (length lst))))
    (swap lst p (1+ p))))

(defun move-up (lst p)
  (declare (list lst) (fixnum p))
  (when (and lst (> p 0) (< p (length lst)))
    (swap lst p (1- p))))

;-- Last element in sequence
(defmacro slast (seq)
  `(aref ,seq (1- (length ,seq))))

;-- replace last element and append rest
(defun last-append (seq tk)
  (let ((s seq))
    (setf (slast s) (aref tk 0))
    (setf s (concatenate 'string s (subseq tk 1)))
    ))

;-- Insert seq2 in seq1 at position pos, when replace t char[pos] will be replaced
(declaim (inline insert-seq))
(defun insert-seq  (seq1 seq2 &key (pos (length seq1)) (replace nil))
  (declare (fixnum pos))
  (unless pos (setf pos (length seq1)))
  (when (and (>= pos 0)(<= pos (length seq1)))
    (concatenate 'string (subseq seq1 0 pos) seq2 (subseq seq1 (if replace (1+ pos) pos)))
    ))

;-- Insert list in list
(declaim (inline insert-list))
(defun insert-list (seq1 seq2 &key (pos (length seq1)))  
  (declare (fixnum pos))
  (when (and (>= pos 0)(<= pos (length seq1)))
    (concatenate 'list (subseq seq1 0 pos) seq2 (subseq seq1 pos))  
    ))

;-- nil 0 1 2 3 ...
(declaim (inline nil-inc nil-dec))
(defun nil-inc (i &optional (d 1))
  (declare (fixnum i d))
  (if i (+ i d) 0))

(defun nil-dec (i &optional (d 1))
  (declare (fixnum i d))
  (if (or (null i) (zerop i)) nil (- i d)))

;-- Transform mode symbol to number
(declaim (inline transform-mode-to-number))
(defun transform-mode-to-number(mode)
  (declare (fixnum mode))
  (case mode
    (:affine      0)
    (:perspective 1)
    (:bilinear    2)
    (:parallel    3)))

;-- Convert (list x y) tuples to double array
(declaim (inline dpoints-to-array))
(defun dpoints-to-array (pl)
  (let ((arr (make-array (* 2 (length pl)) :element-type 'double-float :initial-element 0d0))
        (i 0))
    (declare (list pl) ((vector double-float) arr) (fixnum i))
    (dolist (x pl)
      (setf
       (aref arr     i) (to-double (first  x))
       (aref arr (1+ i))(to-double (second x))
       )
      (incf i 2))
    arr))

;-- Convert #xRRGGBBAA color list to array
(declaim (inline colors-to-array))
(defun colors-to-array (cl)
  (let ((arr (make-array (length cl) :element-type '(unsigned-byte 32) :initial-element 0))
        (i 0))
    (declare (list cl) ((vector (unsigned-byte 32)) arr) (fixnum i))
    (dolist (x cl)
      (setf (aref arr i) (round x))
      (incf i)
      )    
    arr))

;-- Array to list tuples
(declaim (inline array-to-dpoint))
(defun array-to-dpoint (arr)
  (declare ((vector double-float) arr))
  (do ((i 0 (+ i 2))
       (pts nil))
      ((>= i (length arr)) (nreverse pts))
    (push (list (aref arr i) (aref arr (1+ i))) pts)
    ))

;-- Gradient dir to number
(defun gradient-dir-to-number (dir)
  (case dir
    (:DIAMOND 0)
    (:X       1)
    (:XY      2)
    (:SQRT-XY 3)
    (:CONIC   4)
    (:Y       5)
    (:CIRCLE  6)
    ))


;-- COPY CLOS OBJECT
(defmethod copy-instance ((object null           ))  nil)
(defmethod copy-instance ((object symbol         ))  object)
(defmethod copy-instance ((object string         ))  object)
(defmethod copy-instance ((object character      ))  object)
(defmethod copy-instance ((object number         ))  object)
(defmethod copy-instance ((object pathname       ))  object)
(defmethod copy-instance ((object list           ))  (mapcar #'copy-instance object))
(defmethod copy-instance ((object sequence       ))  (copy-seq object))
(defmethod copy-instance ((object standard-object))  (copy-instance-saving-slots object))
(defmethod copy-instance ((object dib            ))  nil)
(defmethod copy-instance ((object aggcontext     ))  nil)
(defmethod copy-instance-saving-slots ((object t))  
  (let ((copy (allocate-instance (class-of object))))
    (dolist (slot-name (mapcar #'aclmop:slot-definition-name  (aclmop:class-slots (class-of object))))
      (setf  (slot-value copy slot-name) (copy-instance (slot-value object slot-name))))
    copy
    ))

;-- Original But genid make same id error
(defun copy-instance-list (ol)
  (mapcar #'(lambda (o) 
              (let ((to (copy-instance o)))
                (setf (oid to) (genid))
                to)) ol))



;-- SAVE CLOS OBJECT
;-- print out clos object in text form
;(defun     float-limit (n) (if (floatp n) (/ (ffloor (* 1000 n)) 1000) n)) ; float under 3
(defun     float-limit (n) (if (floatp n) (/ (ftruncate n 0.001) 1000.0) n))
(defmacro  mi (&rest param) `(make-instance ,@param)) ; use abbrev for 'make-instance' to minimize size
(defmethod save-instance ((object null           ))  nil)
(defmethod save-instance ((object symbol         ))  (car `(',object)))
(defmethod save-instance ((object string         ))  object)
(defmethod save-instance ((object character      ))  object)
(defmethod save-instance ((object number         ))  (float-limit object))
(defmethod save-instance ((object pathname       ))  object)
(defmethod save-instance ((object list           ))  `(list ,@(mapcar #'save-instance object)))
(defmethod save-instance ((object sequence       ))  nil)
(defmethod save-instance ((object standard-object))  (save-instance-saving-slots object))
(defmethod save-instance ((object dib            ))  nil)
(defmethod save-instance ((object aggcontext     ))  nil)

(defmethod save-instance-saving-slots ((object t))
  `(mi ',(class-name (class-of object))
       ,@(loop for slot-name in (mapcar #'aclmop:slot-definition-name  (aclmop:class-slots (class-of object)))
             when (and (slot-boundp object slot-name)
                       (slot-value  object slot-name))
             collect (find-symbol (string slot-name) 'keyword)
             and collect (save-instance (slot-value object slot-name))))) 


;-- REPLACE CLOS OBJECT
(defmethod replace-instance ((d       null) (s       null))  (setf d s))
(defmethod replace-instance ((d     symbol) (s     symbol))  (setf d s))
(defmethod replace-instance ((d  character) (s  character))  (setf d s))
(defmethod replace-instance ((d     number) (s     number))  (setf d s))
(defmethod replace-instance ((d   pathname) (s   pathname))  (setf d s))
(defmethod replace-instance ((d       list) (s       list))  (replace d s))
(defmethod replace-instance ((d   sequence) (s   sequence))  (replace d s))
(defmethod replace-instance ((d        dib) (s        dib))  (setf d nil))
(defmethod replace-instance ((d aggcontext) (s aggcontext))  (setf d nil))
(defmethod replace-instance ((d standard-object) (s null)) nil)
(defmethod replace-instance ((d standard-object) (s standard-object))  
  (if* (string-equal (class-name (class-of d)) (class-name (class-of s)))
     then
          (dolist (slot-name (mapcar #'aclmop:slot-definition-name  (aclmop:class-slots (class-of d))))            
            (setf (slot-value d slot-name) (copy-instance (slot-value s slot-name))))
          t
     else
          nil))
  

;-- Dynamically Generate Symbol
(defun mkword (str num)
  (intern (concatenate 'string
            (change-case-like-reader str)
            (format nil "~A" num)) "KEYWORD"))


;-- Insert item into lst at pos
(declaim (inline ninsert))
(defun ninsert (x lst n)
  (declare (list lst) (fixnum n))
  (when (integerp n)
    (setf n (bound n :end (length lst)))
    (if (zerop n)
        (push x lst)
      (push x (cdr (nthcdr (1- n) lst))))
    )
  lst)

;-- Move element from ps to pd in lst
(declaim (inline nmove))
(defun nmove (lst ps pd)
  (declare (list lst) (fixnum ps pd))
  (let ((s (nth ps lst)))
    (setf (nth ps lst) nil)
    (when (> pd ps) (incf pd))
    (delete nil (ninsert s lst pd))
    ))

;-- minus returns zero
(declaim (inline bound))
(defun bound (val &key (start 0) (end nil))                
  (when (and val start) (if (< val start) (setf val start)))
  (when (and val end  ) (if (> val end  ) (setf val end  )))
  val)

;-- plus bound
(defun mkplus (val)
  (if (minusp val) 0 val))

;-- Not equal
(defmacro nequal (a b) `(null (equal ,a ,b)))


;-- MSSQL DB string to Unicode
(defun dbstr-to-uni (str)
  (if (null str) "NULL"
    (octets-to-string (string-to-octets str :external-format :latin1))))

;-- Unicode to MSSQL DB string
(defun uni-to-dbstr (str)
  (if (null str) "NULL"
    (octets-to-string (string-to-octets str) :external-format :latin1)))


;-- Modify corrupt hangul character to proper unicode
(defun path-filter (fn)
  (pathname (dbstr-to-uni (namestring fn))))


;-- Generate unique ID
(declaim (inline genid))
(defun genid()   
  (gensym) ; set *gensym-counter* at the beginning of program that ensure unique id 
  )

;-- Symbol eqaul check
(defun symbol-equal (a b)  (and (symbolp a) (symbolp b) (string-equal (symbol-name a) (symbol-name b))))
(defun symbol-integer (s)  (parse-integer (remove-if-not #'digit-char-p (symbol-name s))))

;-- Random Color
(defmacro random-color() `(make-rgb :RED (- 255 (random 100)) :GREEN (- 255 (random 100)) :BLUE (- 255 (random 100))))

;-- Remove last
(defun limit-cutoff (l limit)  (if (> (length l) limit) (subseq l 0 limit) l))


;-- Object in View updated
(defmacro obj-updated () `(when (value *view*) (setf (updated (value *view*)) t)))

;-- Status report on status-bar
(defmacro log-message (form &rest args)
  `(status-bar-message (status-bar *main*) (format nil ,form ,@args) :part 1))


;-- Replace one char in string at pos, string make copy for safety
(declaim (inline replace-char))
(defun replace-char (s c pos)
  (let ((ns (copy-seq s)))
    (setf (char ns pos) (if (typep c 'character) c (code-char c)))
    ns))

;-- newimage get file type number from path
(defun fn-to-typenum (fn)
  (if* (pathnamep fn)
     then
          (let ((ftstr (pathname-type fn)))
            (cond
             ((string-equal ftstr "bmp") 1)
             ((string-equal ftstr "tga") 2)
             (t nil)))
     else nil))


;-- Execute Thread safe atomic NOT VALIFIED
(let ((lock (mp:make-process-lock :name "atomic-run-lock")))
  (defmacro atomic-run (&body body)
    `(mp:with-process-lock (,lock)
       ,@body)))

;-- Frame to TC (non-dropframe)
(declaim (inline frame-to-tc))
(defun frame-to-tc (frame)  
  (declare (fixnum frame))
  (multiple-value-bind (h r0) (floor frame 108000)
    (multiple-value-bind (m r1) (floor r0 1800)
      (multiple-value-bind (s f) (floor r1 30)
        (format nil "~2,'0,,D:~2,'0,,D:~2,'0,,D:~2,'0,,D" h m s f)
        ))))

;-- Frame to TC (dropframe)
(declaim (inline count-dropframe frame-to-tc-dropframe))
(defun count-dropframe (nframes)
  (flet ((pos (x) (if (< x 0) 0 x)))
    (multiple-value-bind (lc sc) (floor nframes 17982)
      (* 2 (+ (pos (floor (- sc 2) 1798))
              (* lc 9))))))

(defun frame-to-tc-dropframe (nframes)
  (let ((new-nframes (+ nframes (count-dropframe nframes))))
    (multiple-value-bind (s f) (floor new-nframes 30)
      (multiple-value-bind (m s) (floor s 60)
        (multiple-value-bind (h m) (floor m 60)
          (format nil "~2,'0d:~2,'0d:~2,'0d;~2,'0d" h m s f))))))


;-- make sensor box from center point
(declaim (inline sensor-box))
(defun sensor-box (center size)
  (let ((hs (round size 2)))
    (declare (fixnum hs) (fixnum size) (position center))
    (make-box (- (position-x center) hs) (- (position-y center) hs) 
              (+ (position-x center) hs) (+ (position-y center) hs))))

(declaim (inline nsensor-box))
(defun nsensor-box (b center size)
  (let ((hs (round size 2)))
    (declare (box b) (fixnum hs) (fixnum size) (position center))
    (nmake-box b 
      (- (position-x center) hs) (- (position-y center) hs) 
      (+ (position-x center) hs) (+ (position-y center) hs))))

  
;-- Grouping Rects by Y
(defun group-by-y (s spos epos)  ; s hstring
  (let ((gl  nil)              
        (g   nil))                    
    (dotimes (i (length (str s)))
      (when (and (>= i spos) (< i epos))
        (if* (equal (char (str s) i) #\Newline)
           then (when g (pushnew g gl)) (setf g nil)
           else (pushnew (rect (nth i (chs s))) g))
        ))
    (when g (pushnew g gl))
    gl))

(defun remove-overlap (rs)
  (let ((srs (stable-sort rs #'< :key #'bottom))
        (prev_bottom nil))    
    (dolist (r srs)      
      (when prev_bottom         
        (incf (height r) (- (top r) prev_bottom))
        (setf (top    r) prev_bottom))
      (setf prev_bottom (bottom r)))        
    srs))

;-- Gaussian Distance
(defun gdist (p0 p1)
  (declare (position p0 p1))
  (+ (abs (- (position-x p0) (position-x p1)))
     (abs (- (position-y p0) (position-y p1)))))


;-- apply offset change
(defun update-offset (ctx)
  (agg-offset (ptr ctx) *offset-x* *offset-y* 1d0)
  (when (value *view*)
    (dolist (o (objects (value *view*))) ; calc new position
      (setf (cache-recalc o) t))))

;-- delete nth element in list
(defun remove-nth (lst n)
  (remove-if (constantly t) lst :start n :count 1))

;-- move element in list
(defun move-element (lst sn dn)    
  (let ((rst nil)
        (se  nil))
    (cond
     ((or (equal sn dn) (minusp sn) (minusp dn)) lst)
     ((< dn sn)
      (setf rst (concatenate 'list (subseq lst 0 dn) (list (nth sn lst)) (subseq lst dn)))      
      (remove-nth rst (1+ sn)))
     ((> dn sn)      
      (setf se  (nth sn lst))
      (setf rst (remove-nth lst sn))            
      (concatenate 'list (subseq rst 0 dn) (list se) (subseq rst dn))      
      )
     (t t))))
  

;-- inspector dialog abbrev
(defun ins-g () (inspector-graphics-wnd  *main*))
(defun ins-p () (inspector-property-wnd  *main*))
(defun ins-a () (inspector-animation-wnd *main*))

;-- position valid check
(defmethod position-valid-p ((pos position))
  (and (integerp (position-x pos))
       (integerp (position-y pos))))

;-- Safe 1+ , 1-
(defun s1+ (n)
  (cond
   ((null    n) 1)
   ((numberp n) (+ n 1))
   (t 0)))

(defun s1- (n)
  (cond
   ((null    n) 1)
   ((numberp n) (- n 1))
   (t 0)))

(defun s/ (a b)
  (cond
   ((and (numberp a) (numberp b) (null (zerop a)))
    (/ a b))
   (t 1d0)
   ))

(defun s< (a b)
  (cond
   ((and (numberp a) (numberp b)) 
    (< a b))
   (t nil)
   ))

(defun s> (a b)
  (cond
   ((and (numberp a) (numberp b))
    (> a b))
   (t nil)
   ))

;-- use a if not nil, or b
(defun a-or-b (a b)
  (if a a b))
   


;-- Unicode Pritable Check
(let ((codes (make-array 65536 :element-type '(unsigned-byte 8) :initial-element 0)))
  (defun printable-codes (hdc)
    (let ((cnt (get-printable-code hdc codes)))
      (values codes cnt))))
   
(defun one-char-p (c)
  (equal (length (format nil "~:C" (code-char c))) 1))


;-- unicode vector to string
(defun unicodevec-to-string (vec)
  (let ((str nil)
        (i   0))    
    (while (and (< i (length vec)) (/= 0 (aref vec i)))
      (setf str (concatenate 'string str (list (code-char (aref vec i)))))
      (incf i))
    str))
  

;-- rect a is in rect b, 
;-- a,b is array x1,y1,x2,y2
(defun isin (a b)            
  (and 
   (<  (nth 0 a) (+ (nth 0 a) (nth 2 a)))
   (<  (nth 1 a) (+ (nth 1 a) (nth 3 a)))
   (>= (nth 0 a) (nth 0 b))
   (>= (nth 1 a) (nth 1 b))
   (<= (+ (nth 0 a) (nth 2 a)) (+ (nth 0 b) (nth 2 b)))
   (<= (+ (nth 1 a) (nth 3 a)) (+ (nth 1 b) (nth 3 b)))     
   ))

;--IDE page size
;(setf (ide:ide-page-size (configuration ide:*ide-system*)) '(20000 12000))




;-- Thread Safe format
(defvar *format-lock* (mp:make-process-lock))
(defun sformat (stream form &rest args)
  (mp:with-process-lock (*format-lock*)
    (apply #'format `(,stream ,form ,@args))
    ))

;-- Error logging
(defparameter *LOGWND* nil) ; Log Window Handle
(defun log-message (form &rest args)
  (when (> (file-length *LOGWND*) 10000000) (clear-page *LOGWND*))
  (apply #'sformat `(,*LOGWND* 
                     ,(concatenate 'string "[~A] " form "~%") 
                     ,(multiple-value-bind (s mm h d m y) (get-decoded-time) (format nil "~4,,,'0@A-~2,,,'0@A-~2,,,'0@A ~2,,,'0@A:~2,,,'0@A:~2,,,'0@A" y m d h mm s))  
                     ,@args))
  (file-position *LOGWND* :end))




;-- Integer to Byte seq
(declaim (inline tobyte))
(defun tobyte (val bsize)
  (if* (integerp val)
     then
          (let ((tb  nil) (c 0))
            (dotimes (i  bsize)
              (push (ldb (byte 8 c) val) tb)
              (incf c 8))
            (concatenate '(array (unsigned-byte 8)) tb))
     else
          val))


;-- Count list size
(defun count-list-size (l)
  (let ((s 0))
    (dolist (x l)
      (if (integerp (first x))
          (incf s (second x))
        (incf s (length (first x)))))
    s
    ))
  
 ;-- Continuity_counter setting
(defvar *cnt_count* (make-hash-table))
(defun continuity-counter-set (buf)
  (do* ((i     0  (+ i 188))
        (pid   0)
        (cnt   0)
        (pknum 0))
       ((>=  i  (length buf)) pknum) ; Return pknum as result
    (setf pid (logand #b0001111111111111 (logior (ash (aref buf (+ i 1)) 8) (aref buf (+ i 2))))) ; Get PID
    (unless (equal pid #x1FFF) ; Not NULL pkt
      (incf pknum)
      (setf cnt (gethash pid *cnt_count*))
      (if   cnt (setf cnt (mod (1+ cnt) 16)) (setf cnt 0))
      (setf (gethash pid *cnt_count*) cnt)
      (setf (aref buf (+ i 3)) (logior (logand #b11110000 (aref buf (+ i 3))) (logand #b00001111 cnt)))   
      )))

;-- Change Character Code
(defmacro uni-to-han(str)
  `(if (null ,str) "NULL"
     (octets-to-string (string-to-octets ,str :external-format :latin1))))

;-- Get Current Time in YYYY-MM-DD HH:MM:SS
(defun get-current-time ()
  (let ((dt (multiple-value-list (get-decoded-time))))
    (format nil "~4,,,'0@A-~2,,,'0@A-~2,,,'0@A ~2,,,'0@A:~2,,,'0@A:~2,,,'0@A" 
      (nth 5 dt) (nth 4 dt) (nth 3 dt) (nth 2 dt) (nth 1 dt) (nth 0 dt))))

;-- Convert time string from DB to universal time
(defmacro trim-subsec (str)
  `(subseq ,str 0 (position #\. ,str)))

(defun parse-time-str (string)
  (apply #'encode-universal-time
         (nreverse (string-to-integer-list string))))

(defun string-to-integer-list (str)
  (do* ((string (substitute-if-not #\  #'alphanumericp (trim-subsec str)))
        (start 0)
        (res '()))
       ((null start) (nreverse res))
    (multiple-value-bind (object new-start)
        (parse-integer string :start start :junk-allowed t)
      (if* (null object)
         then ;; EOF
              (setq start nil)
         else (push object res)
              (setq start new-start)))))


;-- If it is binary array skip encoding, if not encode
(defun encode-binary (val)
  (if (typep val '(array (unsigned-byte 8)))
      val
    (encode val)))


;-- Reserve dolist
(defmacro rdolist ((var lst) &body body)    
  (let ((i (gensym)))
    `(do* ((,i (1- (length ,lst)) (1- ,i))
           (,var (nth (abs ,i) ,lst) (nth (abs ,i) ,lst))
           )
          ((minusp ,i) t)
       ,@body
       )))


(in-package :common-graphics-user)

;-- Foreign Functions
(ff:def-foreign-call (create-timer-queue       "CreateTimerQueue"     ) () :strings-convert nil)
(ff:def-foreign-call (delete-timer-queue       "DeleteTimerQueueEx"   ) (htimerqueue hcomple))
(ff:def-foreign-call (create-timer-queue-timer "CreateTimerQueueTimer") ((phtimer :foreign-address) htimerqueue (proc :foreign-address) param duetime period flags))
(ff:def-foreign-call (delete-timer-queue-timer "DeleteTimerQueueTimer") (htimerqueue htimer hcomplete)) 

(defparameter *mktimer-gate* (make-hash-table))

;-- Class Definition
(defclass mktimer()
  ((fun-data
    :accessor fun-data
    :initarg :fun-data
    :initform nil)
   ))

;-- Data for each thread
(defclass thread-data()
  ((fname
    :accessor fname
    :initarg :fname
    :initform nil)
   (param
    :accessor param
    :initarg :param
    :initform 0)
   (duetime
    :accessor duetime
    :initarg :duetime
    :initform 0)
   (period
    :accessor period
    :initarg :period
    :initform 0)
   (flags
    :accessor flags
    :initarg :flags
    :initform #x20)
   (ptr
    :accessor ptr
    :initarg :ptr
    :initform nil)
   (idx
    :accessor idx
    :initarg :idx
    :initform nil)
   (old-ptr
    :accessor old-ptr
    :initarg :old-ptr
    :initform nil)
   (htimer
    :accessor htimer
    :initarg :htimer
    :initform (make-array 1 :element-type '(unsigned-byte 32) :initial-element 0))
   (running
    :accessor running
    :initarg :running
    :initform nil)
   ))


;-- Registering
;-- flags #x20  run in single thread
;-- flags #x00  run in individual thread
(defmethod mktimer-register ((mkt mktimer) &key (fname nil) (param 0) (duetime 0) (period 0) (flags #x20))
  (when fname
    (multiple-value-bind (ptr idx old-ptr) (ff:register-foreign-callable fname)
      (push
       (make-instance 'thread-data
         :fname   fname
         :param   param
         :duetime duetime
         :period  period
         :flags   flags
         :ptr     ptr
         :idx     idx
         :old-ptr old-ptr)
       (fun-data mkt))
      (setf (gethash idx *mktimer-gate*) (mp:make-gate nil))
      )))

(defmethod mktimer-unregister ((mkt mktimer))
  (dolist (f (fun-data mkt))
    (setf (gethash (idx f) *mktimer-gate*) nil)
    (ff:unregister-foreign-callable (idx f))
    )
  (setf (fun-data mkt) nil))

(defmethod mktimer-start ((mkt mktimer))
  (dolist (f (fun-data mkt))
    (unless (running f)
      (create-timer-queue-timer (htimer f) 0 (ptr f) (idx f) (duetime f) (period f) (flags f)) ; use idx for param
      (setf (running f) t)
      )))

(defmethod mktimer-stop ((mkt mktimer))
  (dolist (f (fun-data mkt))
    (when   (running f)
      (mp:process-wait "mktimer thread finish-wait" #'(lambda (g) (null (mp:gate-open-p g))) (gethash (idx f) *mktimer-gate*))
      (delete-timer-queue-timer 0 (aref (htimer f) 0) #xFFFFFFFF)
      (setf (running f) nil)
      ))
  (kill-invalid-process))


;-- Clear Remain Garbage Foreign Process
(defun kill-invalid-process()
  (let ((proc (mp:process-name-to-process "Immigrant Process" :abbrev t :error nil)))
    (while proc
      (mp:process-kill proc)
      (setf proc (mp:process-name-to-process "Immigrant Process" :abbrev t :error nil))
      )))


;---test
#|
(ff:defun-foreign-callable proc-1 (param timeout)
  (declare (:convention :stdcall))
  (mp:open-gate  (gethash param *mktimer-gate*))
  (format t "Proc-1 called ~S ~S~%" param timeout)
  (mp:close-gate (gethash param *mktimer-gate*)))

(ff:defun-foreign-callable proc-2 (param timeout)
  (declare (:convention :stdcall))
  (mp:open-gate  (gethash param *mktimer-gate*))
  (format t "Proc-2 called ~S ~S~%" param timeout)
  (mp:close-gate (gethash param *mktimer-gate*)))

(ff:defun-foreign-callable proc-3 (param timeout)
  (declare (:convention :stdcall))
  (mp:open-gate  (gethash param *mktimer-gate*))
  (format t "Proc-3 called ~S ~S~%" param timeout)
  (mp:close-gate (gethash param *mktimer-gate*)))


(defparameter mkt (make-instance 'mktimer))
(mktimer-register mkt :fname 'proc-1 :period 100 :flags #x20)
(mktimer-register mkt :fname 'proc-2 :period 200 :flags #x20)
(mktimer-register mkt :fname 'proc-3 :period 300 :flags #x20)
(mktimer-start mkt)
(mktimer-stop  mkt)
|#


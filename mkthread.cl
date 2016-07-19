;-------------------------------
;-- MKThread  version 2
;-- 2011.8.19
;-- Mark Yang

(in-package :common-graphics-user)

(eval-when (compile load eval)
  (ff:def-foreign-call (begin-thread            "_beginthreadex"         ) (security stacksize (proc :foreign-address) arg flag (threadid :foreign-address)))
  (ff:def-foreign-call (end-thread              "_endthreadex"           ) (retval))    
  (ff:def-foreign-call (set-thread-priority     "SetThreadPriority"      ) (hthread priority))
  (ff:def-foreign-call (resume-thread           "ResumeThread"           ) (hthread))
  (ff:def-foreign-call (getcurrentprocess       "GetCurrentProcess"      ) () :strings-convert nil)
  (ff:def-foreign-call (getcurrentthread        "GetCurrentThread"       ) () :strings-convert nil)
  (ff:def-foreign-call (setpriorityclass        "SetPriorityClass"       ) (hProc priority))
  (ff:def-foreign-call (setthreadpriority       "SetThreadPriority"      ) (hTh   priority))
  (ff:def-foreign-call (setprocesspriorityboost "SetProcessPriorityBoost") (hProc disable))
  (ff:def-foreign-call (sleep-ex                "SleepEx") (timems alertable) :release-heap :always) ; already defined  
  )

(defclass thread-info ()
    ((fname
      :accessor fname
      :initarg :fname
      :initform nil)
     (security
      :accessor security
      :initarg :security
      :initform 0)
     (stacksize
      :accessor stacksize
      :initarg :stacksize
      :initform 0)
     (pproc
      :accessor pproc
      :initarg :pproc
      :initform nil)
     (flag
      :accessor flag
      :initarg :flag
      :initform 4)
     (thread-id
      :accessor thread-id
      :initarg :thread-id
      :initform (make-array 1 :element-type '(unsigned-byte 32) :initial-element 0))
     (priority
      :accessor priority
      :initarg :priority
      :initform 8)
     (period
      :accessor period
      :initarg :period
      :initform 1)
     (hthread
      :accessor hthread
      :initarg :hthread
      :initform nil)
     (idx
      :accessor idx
      :initarg :idx
      :initform nil)
     (param
      :accessor param
      :initarg :param
      :initform nil)
     (gate-run
      :accessor gate-run
      :initarg :gate-run
      :initform (mp:make-gate nil))
     (gate-stop
      :accessor gate-stop
      :initarg :gate-stop
      :initform (mp:make-gate t))     
     ))


(let ((mkthread-table (make-hash-table)))
  
  (defun mkthread-register (&key (fname nil) (security 0) (stacksize 0) (param 0) (flag 4) (priority 8) (period 1))
    (when fname
      (multiple-value-bind (ptr idx old-ptr) (ff:register-foreign-callable fname)
        (declare (ignorable old-ptr param))
        
        ;(let (v (gethash fname mkthread-table))
        ;  (when v
        ;    (mkthread-stop :fname k)
        ;    (ff:unregister-foreign-callable (idx v))
        ;    (remhash fname mkthread-table)))
        
        (setf (gethash fname mkthread-table)
          (make-instance 'thread-info
            :fname     fname
            :security  security
            :stacksize stacksize
            :pproc     ptr
            :flag      flag         
            :idx       idx
            :param     idx
            :priority  priority
            :period    period
            ))
        fname
        )))
  
  (defun mkthread-unregister (&key (fname nil))                        
    (maphash #'(lambda (k v)
                 (declare (ignorable k v))
                 (when (or (and  fname (equal fname k))
                           (null fname))
                   (mkthread-stop :fname k)
                   (ff:unregister-foreign-callable (idx v))
                   (remhash k mkthread-table)                  
                   ))
             mkthread-table))
  
  (defun mkthread-start (&key (fname nil))           
    (maphash #'(lambda (k v)
                 (declare (ignorable k v))
                 (when (or (and  fname (equal fname (fname v)))
                           (null fname))                                                          
                   (unless (mp:gate-open-p (gate-run v))
                     (mp:open-gate  (gate-run  v))
                     (mp:close-gate (gate-stop v))
                     (setf (hthread v) (begin-thread (security v) (stacksize v) (pproc v) (param v) (flag v) (thread-id v)))
                     (set-thread-priority (hthread v) (priority v))
                     (resume-thread (hthread v))
                     (sleep-ex 10 1)
                     )))
             mkthread-table))
  
  (defun mkthread-stop (&key (fname nil))
    (maphash #'(lambda (k v)
                 (declare (ignorable k v))
                 (when (or (and  fname (equal fname (fname v)))
                           (null fname))
                   (when (mp:gate-open-p (gate-run v))
                     (mp:close-gate (gate-run v))
                     (mp:process-wait "thread stop waiting" #'mp:gate-open-p (gate-stop v))                     
                     )))
             mkthread-table))
  
  
  (defun mkthread-disp ()
    (maphash #'(lambda (k v)
                 (declare (ignorable k v))
                 (format t "~%KEY:~S  VALUE:~S  GATE-RUN:~S  GATE-STOP:~S" k v (gate-run v) (gate-stop v)))
             mkthread-table))
  
  (declaim (inline mkthread-get-ht))
  (defun mkthread-get-ht (k)
    (gethash k mkthread-table))
  
  (defmacro mkthread-defun (name args &rest body)
    (let ((d (gensym)))
      `(progn
         (ff:defun-foreign-callable ,name ,args
           (declare (:convention :stdcall))
           (declare (ignorable ,args))
           (let ((,d (mkthread-get-ht ',name)))
             (mp:open-gate  (gate-run  ,d))
             (mp:close-gate (gate-stop ,d))
             ,@body 
             (mp:close-gate (gate-run  ,d))
             (mp:open-gate  (gate-stop ,d))         
             ))    
         (mkthread-register :fname ',name))))
  
  (defmacro mkthread-loop (name args period &rest body)
    (let ((d (gensym)))      
      `(progn
         (ff:defun-foreign-callable ,name ,args
           (declare (:convention :stdcall))
           (declare (ignorable ,args))             
           (let ((,d (mkthread-get-ht ',name)))
             (while (mp:gate-open-p (gate-run ,d))          
               ,@body
               (sleep-ex (period ,d) 1)          
               )
             (mp:open-gate (gate-stop ,d))             
             ))
         (mkthread-register :fname ',name :period ,period))))        
  )


;-- TEST
#|
(macroexpand-1
'(mkthread-defun proc5 ()
                (format t "Hello proc5~%")
                ))

(mkthread-loop loop1 () 100               
               (format t "loop1:~S~%" (get-internal-real-time))
               )
(mkthread-loop loop2 () 500               
               (format t "loop2:~S~%" (get-internal-real-time))
               )
(mkthread-loop loop3 () 1000               
               (format t "loop3:~S~%" (get-internal-real-time))
               )

(mkthread-disp)
(mkthread-start :fname 'loop1)
(mkthread-stop  :fname 'loop1)
(mkthread-start)
(mkthread-stop)
(mkthread-unregister)
|#

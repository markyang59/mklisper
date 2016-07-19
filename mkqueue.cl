;-- queue by Mark Yang
; 2014.1.1

(defclass mkqueue (mp:queue)
  ((size
    :accessor size
    :initarg :size
    :initform 0)
   (limit
    :accessor limit
    :initarg :limit
    :initform nil)   
   (gate-avail
    :accessor gate-avail
    :initarg :gate-avail
    :initform (mp:make-gate t))
   ))


(defmethod dequeue ((q mkqueue) &key (wait nil))
  (if* (mp:queue-empty-p q)
     then nil
     else
          (decf (size q))
          (mp:open-gate (gate-avail q))
          (mp:dequeue q :wait wait)
          ))

(defmethod enqueue ((q mkqueue) v)  
  (when (limit q)
    (when (>= (size q) (limit q)) (mp:close-gate  (gate-avail q)))
    (mp:process-wait "mkq avail" #'mp:gate-open-p (gate-avail q))
    )  
  (incf (size q))
  (mp:enqueue q v)  
  )

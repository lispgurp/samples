(defun sleep-units (value unit)
  (sleep
    (* value
       (case unit
         ((s) 1)
         ((m) 60)
         ((h) 3600)
         ((d) 86400)
         ((ms) 1/1000)
         ((us) 1/1000000)))))

(sleep-units 2 'm)
(trace sleep-units)

(defmacro sleep-units (value unit)
  `(sleep
     (* ,value
        ,(case unit
           ((s) 1)
           ((m) 60)
           ((h) 3600)
           ((d) 86400)
           ((ms) 1/1000)
           ((us) 1/1000000)))))

(defmacro nlet (aname letargs &rest body)
  `(labels ((,aname ,(mapcar #'car letargs)
              ,@body))
     (,aname ,@(mapcar #'cadr letargs))))

(macroexpand
    '(nlet fact ((n n))
       (if (zerop n)
         1
         (* n (fact (- n 1))))))

(macroexpand
 '(for (x 1 5)
     (princ x)))

(for (limit 1 5)
     (princ limit))


(defmacro for ((var start stop) &body body) ; wrong
  `(do ((,var ,start (1+ ,var))
        (limit ,stop))
       ((> ,var limit))
     ,@body))

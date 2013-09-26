; make a test hash table to play with
(defun make-a-test-hashtable-to-play-with ()
  (let ((tst-hash (make-hash-table :test 'equal)))
    (setf (gethash "Riya" tst-hash) 'Cute)
    (setf (gethash "Preto" tst-hash) 'LoveHer)
    (setf (gethash "NewSmallBaby" tst-hash) 'Growin)
    (setf (gethash "Dad" tst-hash) 'Codin)
    tst-hash))

(defun reset-global-hash-table ()
  (setf *tst-hash* (make-a-test-hashtable-to-play-with)))

; state change in setf of let of an object slot? ;;;
(defun object-slot-reference-test ()
  (let* ((tst-obj (make-instance 'critter-map-entry))
		 (a-place (direction tst-obj)))
	(format t "direction slot before being set indirectly: ~A ~%" (direction tst-obj))
	(format t "value of the direct reference: ~A ~%" a-place)
	(setf a-place 'north)
	(format t "value of the reference after reference has been set: ~A ~%" a-place)
	(format t "direction slot after being set indirectly: ~A ~%" (direction tst-obj))))

  ; lesson: turns out you cannot modify the slot value to point to something else directly through a let binding

; state change of setf of element looping in a data structure? ;;;
(defun testing-setffing-of-looping-elements ()
  (let ((an-array (make-array 5 :initial-element 1)))
	(format t "Array before being set individually: ~A ~%" an-array)
    ;(loop for ele across an-array
    ;     do (setf ele 2)) doesn't work
    (loop for i from 0 to  (1- (length an-array))
         do (setf (aref an-array i) 2))
    (format t "Array after being set individually: ~A ~%" an-array)))

; playing with alexandria shuffle ;;;

;(ql:quickload 'alexandria) 
(defun play-alexandria ()
  (let ((tst-hash (make-a-test-hashtable-to-play-with)))
    (format t "Hash Table before alex shuffles:~%")
    (print-hash tst-hash)
    (let* ((kys (get-hash-keys tst-hash))
           (shuffled-kys (alexandria:shuffle kys)))
      (format t "After alex shuffles the keys: ~A ~%" shuffled-kys))))

; comparing jaava comparator to cl sort ;;;

; Java Comparator behaves like the following
; -: if x < y
; 0: if x = y
; 1: if x > y

; in Cl this translates to
; true if x < y
; nil if x = y
; nil if x > y -- no distinction made between equal and greater than

(defun wierd-sort ()
  (let ((alst '(1 7 16 3 19 3 0 44 3)))
    (sort alst #'<)
    (sort alst #'strange-predicate)))

(defun strange-predicate (x y)
  (let ((val (- (min x 10) (min y 10))))
    (< val 0)))

; play with loop

(defun loop-play ()
  (let ((tst-array (make-array 10 :initial-contents '(1 2 3 4 5 6 7 8 9 10))))
    (loop for ele across tst-array
         for i from 0 to (1- (length tst-array))
         for r = (random 10)
         for result = (* ele r)
         for e = (mod result 2) then (/ result 2) 
         do (progn
              (format t "Array[~A]:~A" i ele)
              (format t ", rand:~A" r)
              (format t ", a[i] * rand is equivelent to ~A" result)
              (if (equal i 0)
                  (format t ", mod ~A by 2 = ~A  ~%" result e
				  
; more loop play

(defun test-loop (lst target-val)
  (loop for val in lst
     if (evenp val)
     do (let ((plusone (1+ val)))
          (break)
          (when (eq plusone target-val)
            (return val)))))
      

                 




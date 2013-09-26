(defclass simulation-object ()
  ((a-point :accessor pt :initarg :pt)
   (a-color :accessor col :initarg :col)
   (a-string :accessor str :initarg :str))
  (:default-initargs
   :pt nil
    :col nil
    :str nil))

(defclass zombie (simulation-object)
  ((infection-count :accessor infection-scount :initarg :infect))
  (:default-initargs
   :infect nil))

(defclass coordinate-plane ()
  ((height :accessor height :initarg :h)
   (width :accessor width :initarg :w)
   (plane :accessor plane :initarg :plane)
   (total-objects :accessor total-objects :initarg :total))
  (:default-initargs
   :h nil :w nil 
   :plane nil :total 0)) 

(defvar *directions* '(north south east west))

(defmethod overpopulated? ((p coordinate-plane) n)
  (let ((new-count (+ (total-objects p) n))
		(max-population (* (width p) (height p))))
	(> new-count max-population)))

(defun find-random-location (p)
  (let ((aimless (nth (random (length *directions*)) *directions*)))
	(loop 
	   for x = (random (width p))
	   for y = (random (height p))
	   until (null (aref (plane p) x y))
	   finally (values x y aimless))))


(defmethod add-objects ((p coordinate-plane) n class-name)
  (when (overpopulated? p n)
	(error ":( Too many critters. Not enough space on the plane of existence. Use blub birth control :("))
  (loop for i from 0 to n
       for new-object = (make-instance class-name)
     do 	  
       (multiple-value-bind (x y random-dir)
           (find-random-location (plane p))
         (setf (aref (plane p) x y) new-object))))

(defun update ((p coordinate-plane))
  ())

 

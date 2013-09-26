;(defun hello-1()
;  (with-ltk ()
;    (let ((b (make-instance 'button
;                                 :master nil
;                                 :text "Press Me"
;                                 :command (lambda ()
;                                        (format t "Hello World!~&")))))
;      (pack b))))

;(defun hello-2()
;  (with-ltk ()
;    (let* ((f (make-instance 'frame))
;           (b1 (make-instance 'button
;                               :master f
;                               :text "Button 1"
 ;                              :command (lambda () (format t "Button1~&"))))
 ;          (b2 (make-instance 'button
;                               :master f
;                               :text "Button 2"
;                               :command (lambda () (format t "Button2~&")))))
;      (pack f)
;      (pack b1 :side :left)
;      (pack b2 :side :left)
;      (configure f :borderwidth 3)
;      (configure f :relief :sunken)
;      )))

(defun canvastest()
  (with-ltk ()
    (let* ((sc (make-instance 'scrolled-canvas))
           (c (canvas sc))
           (line (create-line c (list 100 100 400 50 700 150)))
           (polygon (create-polygon c (list 50 150 250 160 250
                                            300 50 330 )))
           (text (create-text c 260 250 "Canvas test")))
      (pack sc :expand 1 :fill :both)
      (scrollregion c 0 0 800 800)
      )))

; we will wrap ltk elements as needed


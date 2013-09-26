; model ;
; coordinate-plane
; simulation-objects
;
;
;  visual (string, color)
;     
;    fly trap: ("T",Red)
;    bad food: ("F", Green)
;    bear (alternate slash per move, 50% white, 50% black)
;    lion: ("L",wierd random pick of Red,Green or Blue)
;    giant: ("fee/6 moves","foo/6 moves", Gray)
; 
;  actions
;    move based on neighbor, direction, infection-count
;    rule (object,nieghbor,direction, infection-count)
;      all living beings = "frontal enemy infection"
;      fly trap: otherwise "rotate left"
;      bad food: "infect" 
;      bear: otherwise "hop check", otherwise "infect"  
;      lion: otherwise if wall is front or right "rotate left", if lion is front "rotate right", otherwise "hop" 
;      giant: otherwise "hop check", otherwise "rotate right"
;
;
; actions
;  add 
;  rotate left
;  rotate right
;  hop 
;  infect  
;
;  frontal enemy infection 
;  hop check  

; ui ;
; interaction controls
;   start
;   stop
;   step
;   debug
;   next 100

; indicators
;   for each object
;   for the current step
; redraw
;
; simulation unit tests as specified at end of blubspec
;
;

; engine
;  add
;  update    

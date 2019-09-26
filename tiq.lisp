(ql:quickload :glaw)
(ql:quickload :glaw-examples)
(ql:quickload :cl-opengl)

(defclass movelight-window (glut:window)
  ((spin :initform 0))
  (:default-initargs :width 500 :height 500
		     :pos-x 100 :pos-y 100
		     :mode '(:single :rgb :depth)
		     :title "movelight.lisp"))

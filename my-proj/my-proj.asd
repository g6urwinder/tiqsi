;;;; my-proj.asd

(asdf:defsystem #:my-proj
  :description "Describe my-proj here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cepl #:rtg-math.vari #:cepl.sdl2 #:swank #:livesupport #:cepl.skitter.sdl2 #:dirt)
  :components ((:file "package")
               (:file "my-proj")))

(in-package #:option-9)
#+option-9-debug (declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

(defclass entity ()
  ((%id :initarg :id
	:initform (new-id)
	:accessor id)
   (%game-context :initarg :game-context
		  :initform nil
		  :accessor game-context)
   (%max-hit-points :initarg :max-hit-points
		    :initform 10
		    :accessor max-hit-points)
   (%hit-points :initarg :hit-points
		:initform 10
		:accessor hit-points)
   (%damage-points :initarg :damage-points
		   :initform 10
		   :accessor damage-points)
   (%points :initarg :points
	    :initform 0
	    :accessor points)
   (%status :initarg :status
	    :initform :alive
	    :accessor status)
   (%charge :initarg :charge
	    :initform -1
	    :accessor charge)
   (%initial-sparks :initarg :initial-sparks
		    :initform 10
		    :accessor initial-speaks)
   (%additional-sparks :initarg :additional-sparks
		       :initform 50
		       :accessor additional-sparks)
   (%auto-finish-construction :initargs :auto-finish-construction
			      :initform t
			      :accessor auto-finish-construction))

  (:documentation "The Entity Class"))

(defclass ephemeral ()
  ((%ttl :initarg :ttl
	 :initform nil
	 :accessor ttl)
   (%ttl-max :initarg :ttl-max
	     :initform nil
	     :accessor ttl-max))
  (:documentation
   "The Ephemeral Class.Used for things which need a temporal time limit"))

(defclass location ()
  ((%x :initarg :x
       :initform 0
       :accessor x)
   (%y :initarg :y
       :initform 0
       :accessor y)
   (%dx :initarg :dx
	:initform 0
	:accessor dx)
   (%dy :initarg :dy
	:initform 0
	:accessor dy))
  (:documentation
   "The Location Class. Used to hold the current position and direction vector at that position"))

(defclass frame (ephemeral location)
  ()
  (:documentation "The Frame Class"))

(defclass shape ()
  ((%primitives :initarg :primitives
		:initform nil
		:accessor primitives))
  (:documentation "The Shape Class"))

(defclass drawable (entity frame shape)
  ()
  (:documentation "The Drawable Class"))

(defclass collidable (drawable)
  ((%radius :initarg :radius
	    :initform 0
	    :accessor radius))
  (:documentation "The Collidable Class"))

(defclass digits (drawable)
  ()
  (:documentation "The Digit Class"))

(defclass spark (drawable)
  ()
  (:documentation "The Spark Class"))

(defclass brain (collidable)
  ((%until-next-action :initarg :until-next-action
		       :initform 0
		       :accessor until-next-action))
  (:documentation "The Brain Class"))

(defclass powerup (brain)
  ((%main-gun :initarg :main-gun
	      :initform nil
	      :accessor powerup-main-gun)
   (%passive-gun :initarg :passive-gun
		 :initform nil
		 :accessor powerup-passive-gun)
   (%main-shield :initarg :main-shied
		 :initform nil
		 :accessor powerup-main-shield)
   (%health-level :initarg :health-level
		  :initform 0
		  :accessor powerup-health-level))
  (:documentation "The Powerup Class"))

(defclass ship (brain)
  ((%main-gun :initarg :main-gun
	      :initform nil
	      :accessor ship-main-gun)
   (%passive-gun :initarg :passive-gun
		 :initform nil
		 :accessor ship-passive-gun)
   (%main-shield :initarg :main-shied
		 :initform nil
		 :accessor ship-main-shield))
  (:documentation "The Ship Class"))

(defclass player (ship)
  ()
  (:documentation "The Player Class"))

(defclass enemy (ship)
  ()
  (:documentation "The Enemy Base Class"))
(defclass enemy-1 (enemy)
  ()
  (:documentation "The Enemy 1 Class"))
(defclass enemy-2 (enemy)
  ()
  (:documentation "The Enemy 2 Class"))
(defclass enemy-3 (enemy)
  ()
  (:documentation "The Enemy 3 Class"))

(defclass weapon (brain)
  ()
  (:documentation "The Weapon Class"))

(defclass shot (weapon)
  ()
  (:documentation "The Shot Class"))
(defclass simple-shot (shot)
  ()
  (:documentation "The Simple Shot Class"))
(defclass hardnose-shot (shot)
  ()
  (:documentation "Shots which aren't destroyed by bullets!"))
(defclass super-shot (shot)
  ()
  (:documentation "Shots which aren't destroyed by ships!"))

(defclass mine (weapon)
  ()
  (:documentation "The base mine class"))
(defclass proximity-mine (mine)
  ()
  (:documentation "The Proximity Mine Class"))
(defclass field-mine (mine)
  ()
  (:documentation "The Field Mine Class"))

(defclass shield (brain)
  ((%shots-absorbed :initarg :shots-absorbed
		    :initform 5
		    :accessor shots-absorbed))
  (:documentation "The Shield Base Class"))
(defclass shot-shield (shield)
  ()
  (:documentation "The Shot Shield Class"))
(defclass ship-shield (shield)
  ()
  (:documentation "The Ship Shield Class"))

(defclass fieldpath ()
  ((%steps :initarg :steps
	   :initform 0
	   :accessor steps)
   (%path :initarg :path
	  :initform nil
	  :accessor path))
  (:documentation "The Field Path Class"))

(defclass pathcontact ()
  ((%number-of-contacts :initarg :number-of-contacts
			:initform 0
			:accessor number-of-contacts)
   (%path-ids :initarg :contacts
	      :initform nil
	      :accessor path-ids))
  (:documentation "The Path Contact Class. This is stored on a per entity basis
and records the field path-ids that touch that particular entity."))

(defclass field ()
  ((%range :initarg :range
	   :initform 1
	   :accessor range)
   (%num-paths :initarg :num-paths
	       :initform 1
	       :accessor num-paths)
   (%paths :initarg :traces
	   :initform nil
	   :accessor paths)
   (%entity-contacts :initarg :contacts
		     :initform (make-hash-table :test #'equal)
		     :accessor entity-contacts))
  (:documentation "The Field Class"))

(defclass tesla-field (field weapon)
  ((%power-range :initarg :power-range
		 :initform 1
		 :reader power-range)
   (%power-lines :initarg :power-lines
		 :initform 1
		 :reader power-lines))
  (:documentation "The Tesla Field Class"))

(defclass game ()
  ((%players :initarg :players
	     :initform nil
	     :accessor players)
   (%player-shots :initarg :player-shots
		  :initform nil
		  :accessor player-shots)
   (%enemy-mines :initarg :mines
		 :initform nil
		 :accessor enemy-mines)
   (%enemies :initarg :enemies
	     :initform nil
	     :accessor enemies)
   (%enemy-shots :initarg :enemy-shots
		 :initform nil
		 :accessor enemy-shots)
   (%sparks :initarg :sparks
	    :initform nil
	    :accessor sparks)
   (%power-ups :initarg :power-ups
	       :initform nil
	       :accessor power-ups)
   (%score :initarg :score
	   :initform 0
	   :accessor score)
   (%score-board :initarg :score-board
		 :initform 0
		 :accessor score-board)
   (%highscore :initarg :highscore
	       :initform 0
	       :accessor highscore)
   (%highscore-board :initarg :highscore-board
		     :initform nil
		     :accessor highscore-board)
   (%enemy-spawn-timer :initarg :enemy-spawn-timer
		       :initform 60
		       :accessor enemy-spawn-timer)
   (%paused :initarg :paused
	    :initform nil
	    :accessor paused))
  (:documentation "The Game Class"))

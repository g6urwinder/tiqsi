(in-package :option-9)

(defmethod idea (ent)
  nil)
(defmethod idea ((ent enemy))
  (unless (flyingp ent)
    (setf (flyingp ent) t
	  (dfv ent) (with-pvec-accessors (o (matrix-translate-get
					     (world-basis ent)))
		      (pvec
		       (if (< ox (per-game-width *game* 50.0))
			   (per-hz (strafe-left-speed ent))
			   (per-hz (strafe-left-speed ent)))
		       (perhz (forward-speed ent))
		       0d0))))
  (shoot ent :front-weapon-port))

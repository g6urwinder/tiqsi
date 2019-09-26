(in-package :option-9)
#+option-9-debug (declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

(defclass spawnable ()
  ((%ioi/e :initarg :ioi/e
	   :initform nil
	   :accessor spawnable-ioi/e)
   (%spawn-context :initarg :spawn-context
		   :initform nil
		   :accessor spawnable-spawn-context)
   (%initializer :initarg :initializer
		 :initform nil
		 :accessor spawnable-initializer)
   (%parent :initarg :parent
	    :initform :universe
	    :accessor spawnable-parent)
   (%mutator :initarg :parent
	     :initform :universe
	     :accessor spawnable-parent)
   (%game :initarg :game
	  :initform nil
	  :accessor spawnable-game)))

(defclass sp-ship (spawnable) ())
(defclass sp-player (sp-shit) ())
(defclass sp-player-shot (spawnable) ())
(defclass sp-player-mine (spawnable) ())
(defclass sp-player-powerup (spawnable) ())
(defclass sp-enemy (sp-ship) ())
(defclass sp-enemy-shot (spawnable) ())
(defclass sp-enemy-mine (spawnable) ())
(defclass sp-sparks (spawnable) ())
(defclass sp-shrapnel (spawnable) ())

(defclass sp-alphanum (spawnable) ())
(defclass sp-realize (spawnable) ())

(defun make-spawnable (spawn-type &rest initargs)
  (apply #'make-instance spawn-type initargs))
(defgeneric spawn (spawn-class ioi/e loc/ent game
		   &key spawn-context parent orphan-policy mutator extra-init
		     &allow-other-keys)
  (:documentation "TODO"))

(defgeneric realize-spawn (spawnable)
  (:documentation "If the parent of the spawn promise is still alive
then force the promise into a real entity and insert it intot he scene tree
in accordance witht he spawn's wishers. Return two values of T/NIL if the
spawn succeeded and a REASON about the success or failure"))
(defgeneric reclaim-failed-spawn (spawnable reason-failed)
  (:documentation "When a spawn failed, do something meaningful with it, if anything."))
(defun realize-spawns (game)
  "Iterate the spawnables list and if possible, realize all of the
spawnables into real in game entities. Those that can't be immediately realized
due to loss of parents are funneled to RECLAIM-FAILED-SPAWN for now."
  (dolist (spawnable (spawnables game))
    (multiple-value-bind (spawnedp reason-failed)
	(realize-spawn spawnable)
      (unless spawnedp
	(reclaim-failed-spawn spawnable reason-failed))))
  (clear-spawnables game))

(defmethod resolve-spawn-location (loc/ent)
  (error "resolve-spawn-location: Can't resolve the location of something i dont understand:"))
(defmethod resolve-spawn-location ((loc/ent simple-array))
  loc/ent)
(defmethod resolve-spawn-location ((loc/ent entity))
  (matrix-translate-get (world-basis loc/ent)))

(defmethod reclaim-failed-spawn ((spawnable spawnable) reason-failed)
  (error "Failed to spawn ~A because of ~A. Sorry~%"
	 spawnable reason-failed)
  nil)
(defmethod realize-spawn ((spawnable spawnable))
  (let ((entity (funcall (spawnable-mutator spawnable)
			 (apply #'make-entity
				(spawnable-initializer spawnable)))))
    (insert-into-scene (scene-man (spawnable-game spawnable))
		       entity
		       (spawnable-parent spawnable))
    (values T :spawned)))
(defmethod realize-spawn ((spawnable sp-ship))
  (let ((entity (funcall (spawnable-mutator spawnable)
			 (apply #'make-entity
				(spawnable-initializer spawnable)))))
    (insert-into-scene (scene-man (spawnable-game spawnable))
		       entity
		       (spawnable-parent spawnable))

    (dolist (layout (port-layout entity))
      (destructuring-bind (port turret-instance payload) layout
	(let* ((port-frame
		(cadadr (assoc port (ports (geometry entity)))))

	       (a-turret
		(make-entity
		 (specialize-generic-instance-name
		  (instance-name entity) turret-instance)
		 :orphan-policy :destroy
		 :payload
		 (let ((payload (specialize-generic-instance-name
				 (instance-name entity) payload)))
		   (when payload
		     (make-entity payload)))
		 :port port
		 :local-basis (matrix-copy port-frame))))
	  (insert-into-scene (scene-man (spawnable-game spawnable))
			     a-turret
			     entity)
	  (when (payload a-turret)
	    (insert-into-scene (scene-man (spawnable-game spawnable))
			       (payload a-turret)
			       a-turret))
	  (setf (turret entity port) a-turret))))
    (values T :spawned)))
(defmethod spawn ((spawn-class (eql 'sp-realize)) ioi/e location game
		  &key spawn-context
		    (parent :universe)
		    (orphan-policy :destroy)
		    (mutator #'identity)
		    extra-init
		    &allow-other-keys)
  (let ((initializer `(,(insts/equiv-choice ioi/e)
			:orphan-policy ,orphan-policy
			,@extra-init)))
    (add-spawnable
     (make-spawnable 'sp-realize
		     :ioi/e ioi/e
		     :spawn-context spawn-context
		     :initializer initializer
		     :parent parent
		     :mutator mutator
		     :game game)
     game)))
(defmethod spawn ((spawn-class (eql 'sp-player)) ioi/e loc/ent game
		  &key
		    (spawn-context 1)
		    (parent :universe)
		    (orphan-policy :destroy)
		    (mutator #'identity)
		    extra-init
		    &allow-other-keys)
  (declare (ignorable loc/ent))
  (let ((initializer `(,(insts/equiv-choice ioi/e)
			:orphan-policy ,orphan-policy
			:roles (:player)
			:flyingp t
			:dv ,(pvec (per-game-width game 50.0)
				   (per-game-height game 5.0)
				   0d0)
			,@extra-init)))
    (add-spawnable
     (make-spawnable 'sp-ship
		     :ioi/e ioi/e
		     :spawn-context spawn-context
		     :initializer initializer
		     :parent parent
		     :mutator mutator
		     :game game)
     game)))
(defmethod spawn ((spawn-class (eql 'sp-player-shot)) ioi/e loc/ent game
		  &key
		    spawn-context
		    (parent :universe)
		    (orphan-policy :destroy)
		    (mutator #'identity)
		    extra-init
		    &allow-other-keys)
  (let ((loc (resolve-spawn-location loc/ent)))
    (with-pvec-accessors (o loc)
      (let ((initializer `(,ioi/e
			   :orphan-policy ,orphan-policy
			   :roles (:player-shot)
			   :local-basis ,(matrix-copy (world-basis loc/ent))
			   ,@extra-init)))
	(add-spawnable
	 (make-spawnable spawn-class
			 :ioi/e ioi/e
			 :spawn-context spawn-context
			 :initializer initializer
			 :parent parent
			 :mutator mutator
			 :game game)
	 game)))))
(defmethod spawn ((spawn-class (eql 'sp-player-mine)) ioi/e loc/ent game
		  &key
		    spawn-context
		    (parent :universe)
		    (orphan-policy :destroy)
		    (mutator #'identity)
		    extra-init
		    &allow-other-keys)
  (let* ((loc (resolve-spawn-location loc/ent))
	 (the-mine-instance-name
	  (specialize-generic-instance-name (instance-name loc/ent) ioi/e))
	 (initializer `(,the-mine-instance-name
			:orphan-policy ,orphan-policy
			:roles (:player-mine)
			:dv ,(vcopy loc)
			,@extra-init)))
    (add-spawnable
     (make-spawnable spawn-class
		     :ioi/e ioi/e
		     :spawn-context spawn-context
		     :initializer initializer
		     :parent parent
		     :mutator mutator
		     :game game)
     game)))
(defmethod spawn ((spawn-class (eql 'sp-enemy)) ioi/e loc/ent game
		  &key
		    spawn-context
		    (parent :universe)
		    (orphan-policy :destroy)
		    (mutator #'identity)
		    extra-init
		    &allow-other-keys)
  (declare (ignorable loc/ent))
  (let* ((xloc (coerce (random (game-width game)) 'double-float))
	 (initializer `(,(insts/equiv-choice ioi/e)
			 :orphan-policy ,orphan-policy
			 :roles (:enemy)
			 :flyingp nil
			 :dr ,(pvec 0d0 0d0 pi)
			 :dv ,(pvec xloc (per-game-height game 95.0) 0d0)
			 ,@extra-init)))
    (add-spawnable
     (make-spawnable 'sp-ship
		     :ioi/e ioi/e
		     :spawn-context spawn-context
		     :initializer initializer
		     :parent parent
		     :mutator mutator
		     :game game)
     game)))
(defmethod spawn ((spawn-class (eql 'sp-enemy-shot)) ioi/e loc/ent game
		  &key
		    spawn-context
		    (parent :universe)
		    (orphan-policy :destroy)
		    (mutator #'identity)
		    extra-init
		    &allow-other-keys)
  (let ((loc (resolve-spawn-location loc/ent)))
    (with-pvec-accessors (o loc)
      (let ((initializer `(,ioi/e
			   :orphan-policy ,orphan-policy
			   :roles (:enemy-shot)
			   :local-basis ,(matrix-copy (world-basis loc/ent))
			   :dfv ,(pvec 0d0
				       (+ .5d0 (random .5d0)
					  (dfvy
					   (if (subtypep
						(type-of loc/ent) 'turret)
					       (parent loc/ent)
					       loc/ent)))
				       0d0)
			   ,@extra-init)))
	(add-spawnable
	 (make-spawnable spawn-class
			 :ioi/e ioi/e
			 :spawn-context spawn-context
			 :initializer initializer
			 :parent parent
			 :mutator mutator
			 :game game)
	 game)))))
(defmethod spawn ((spawn-class (eql 'sp-sparks)) ioi/e loc/ent game
		  &key
		    spawn-context
		    (parent :universe)
		    (orphan-policy :destroy)
		    (mutator #'identity)
		    (num-sparks 10)
		    (ttl-max 0 ttl-max-supplied-p)
		    (initial-velocity 0d0)
		    (velocity-factor 2d0)
		    extra-init
		    &allow-other-keys)
  (let ((loc (resolve-spawn-location loc/ent)))
    (dotimes (p num-sparks)
      (let ((initializer `(,(insts/equiv-choice ioi/e)
			    :orphan-policy ,orphan-policy
			    :roles (:scenery)
			    :dv ,(vcopy loc)
			    :dtv ,(let ((dirvec (vrand :span :xy)))
				    (vscalei dirvec
					     dirvec
					     (+ initial-velocity
						(random-velocity-factor))))
			    ,@(when ttl-max-supplied-p (list :ttl-max-ttl-max))
			    ,@extra-init)))
	(add-spawnable
	 (make-spawnable spawn-class
			 :ioi/e ioi/e
			 :spawn-context spawn-context
			 :initializer initializer
			 :parent parent
			 :mutator mutator
			 :game game)
	 game)))))
(defmethod spawn ((spawn-class (eql 'sp-player-powerup)) ioi/e loc/ent game
		  &key
		    spawn-context
		    (parent :universe)
		    (orphan-policy :destroy)
		    (mutator #'identity)
		    extra-init
		    &allow-other-keys)
  (let* ((loc (resolve-spawn-location loc/ent))
	 (initializer `(,(insts/equiv-choice ioi/e)
			 :orphan-policy ,orphan-policy
			 :roles (:player-powerup)
			 :flyingp t
			 :dv ,(vcopy loc)
			 ,@extra-init)))
    (add-spawnable
     (make-spawnable spawn-class
		     :ioi/e ioi/e
		     :spawn-context spawn-class
		     :initializer initializer
		     :parent parent
		     :mutator mutator
		     :game game)
     game)))
(defmethod spawn ((spawn-class (eql 'sp-enemy-mine)) ioi/e loc/ent game
		  &key
		    spawn-context
		    (parent :universe)
		    (orphan-policy :destroy)
		    (mutator #'identity)
		    extra-init
		    &allow-other-keys)
  (let* ((loc (resolve-spawn-location loc/ent))
	 (the-mine-instance-name
	  (if (subtypep (type-of loc/ent) 'instance)
	      (the-mine-instance-name
	       (if (subtypep (type-of loc/ent) 'instance)
		   (specialize-generic-instance-name
		    (instance-name loc/ent)
		    (insts/equiv-choice ioi/e))
		   ioi/e))
	      (initializer `(,the-mine-instance-name
			     :orphan-policy ,orphan-policy
			     :roles (:enemy-mine)
			     :dv ,(vcopy loc)
			     ,@extra-init)))
	   (add-spawnable
	    (make-spawnable spawn-class
			    :ioi/e ioi/e
			    :spawn-context spawn-context
			    :initializer initializer
			    :parent parent
			    :mutator mutator
			    :game game)
	    game)))))
(defmethod spawn ((spawn-class (eql 'sp-alphanum)) ioi/e loc/ent game
		  &key
		    spawn-context
		    (parent :universe)
		    (orphan-policy :destroy)
		    (mutator #'identity)
		    roles
		    extra-init
		    &allow-other-keys)
  (let* ((loc (resolve-spawn-location loc/ent))
	 (initializer `(,(insts/equiv-choice ioi/e)
			 :orphan-policy ,orphan-policy
			 :roles ,roles
			 :dv ,(vcopy loc)
			 ,@extra-init)))
    (add-spawnable
     (make-spawnable spawn-class
		     :ioi/e ioi/e
		     :spawn-context spawn-class
		     :initializer initializer
		     :parent parent
		     :mutator mutator
		     :game game)
     game)))
(defmethod spawn ((spawn-class (eql 'sp-shrapnel)) ioi/e loc/ent game
		  &key
		    spawn-context
		    (parent :universe)
		    (orphan-policy :destroy)
		    (mutator #'identity)
		    (velocity-factor 50d0)
		    extra-init
		    &allow-other-keys)
  (let* ((loc (resolve-spawn-location loc/ent))
	 (initializer `(,(insts/equiv-choice ioi/e)
			 :orphan-policy ,orphan-policy
			 :roles (:shrapnel)
			 :dv ,(vcopy loc)
			 :rotatingp t
			 :drv ,(pvec 0d0 0d0 (/ pi (+ 64d0 (random 64d0))))
			 :dtv ,(pvec (* (random-delta) velocity-factor)
				     (* (random-delta) velocity-factor)
				     0d0)
			 ,@extra-init)))
    (add-spawnable
     (make-spawnable spawn-class
		     :ioi/e ioi/e
		     :spawn-context spawn-context
		     :initializer initializer
		     :parent parent
		     :mutator mutator
		     :game game)
     game)))

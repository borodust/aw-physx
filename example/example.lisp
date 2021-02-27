(cl:defpackage :physx.example
  (:use :cl)
  (:export #:run))
(cl:in-package :physx.example)

;;;
;;; UTIL
;;;
(defmacro with-scale ((scale) &body body)
  `(let ((,scale (iffi:make-intricate-instance '%physx:physx+px-tolerances-scale)))
     (unwind-protect
          (progn ,@body)
       (iffi:destroy-intricate-instance '%physx:physx+px-tolerances-scale ,scale))))

;;;
;;; MATH
;;;
(defun make-vec3 ()
  (iffi:make-intricate-instance '%physx:physx+px-vec3 :float 0f0))


(defun destroy-vec3 (vec)
  (iffi:destroy-intricate-instance '%physx:physx+px-vec3 vec))


(defmacro with-vec3 ((vec) &body body)
  `(let ((,vec (make-vec3)))
     (unwind-protect
          (progn ,@body)
       (destroy-vec3 ,vec))))


(defun x (vec)
  (iffi:intricate-slot-value vec
                             '%physx:physx+px-vec3
                             '%physx:x))


(defun (setf x) (value vec)
  (setf (iffi:intricate-slot-value vec
                                   '%physx:physx+px-vec3
                                   '%physx:x)
        (float value 0f0)))


(defun y (vec)
  (iffi:intricate-slot-value vec
                             '%physx:physx+px-vec3
                             '%physx:y))


(defun (setf y) (value vec)
  (setf (iffi:intricate-slot-value vec
                                   '%physx:physx+px-vec3
                                   '%physx:y)
        (float value 0f0)))


(defun z (vec)
  (iffi:intricate-slot-value vec
                             '%physx:physx+px-vec3
                             '%physx:z))


(defun (setf z) (value vec)
  (setf (iffi:intricate-slot-value vec
                                   '%physx:physx+px-vec3
                                   '%physx:z)
        (float value 0f0)))


(defun make-transform ()
  (iffi:make-intricate-instance '%physx:physx+px-transform
                                '%physx:physx+px-identity :px-identity))


(defun destroy-transform (transform)
  (iffi:destroy-intricate-instance '%physx:physx+px-transform transform))


(defmacro with-transform ((transform) &body body)
  `(let ((,transform (make-transform)))
     (unwind-protect
          (progn ,@body)
       (destroy-transform ,transform))))


(defun transform-position (transform)
  (iffi:intricate-slot-value transform '%physx:physx+px-transform '%physx:p))


(defun transform-rotation (transform)
  (iffi:intricate-slot-value transform '%physx:physx+px-transform '%physx:q))


;;;
;;; FOUNDATION
;;;
(defun make-foundation (allocator error-callback)
  (%physx:px-create-foundation '%physx:physx+px-u32 67174656
                               '(:pointer %physx:physx+px-allocator-callback) allocator
                               '(:pointer %physx:physx+px-error-callback) error-callback))


(defun destroy-foundation (foundation)
  (%physx:physx+release '(:pointer %physx:physx+px-foundation) foundation))


(defun run-with-default-callbacks (action)
  (let ((default-allocator (iffi:make-intricate-instance '%physx:physx+px-default-allocator))
        (default-error-callback (iffi:make-intricate-instance '%physx:physx+px-default-error-callback)))
    (unwind-protect
         (funcall action default-allocator default-error-callback)
      (iffi:destroy-intricate-instance '%physx:physx+px-default-allocator
                                       default-allocator)
      (iffi:destroy-intricate-instance '%physx:physx+px-default-error-callback
                                       default-error-callback))))


(defun run-with-foundation (action)
  (flet ((%run-with-foundation (allocator error-callback)
           (let ((foundation (make-foundation allocator error-callback)))
             (unwind-protect
                  (funcall action foundation)
               (destroy-foundation foundation)))))
    (run-with-default-callbacks #'%run-with-foundation)))


;;;
;;; VISUAL DEBUGGER
;;;
(defun make-pvd (foundation)
  (%physx:physx+px-create-pvd '(:pointer %physx:physx+px-foundation) foundation))


(defun make-instrumentation-flags (flag)
  (iffi:make-intricate-instance
   '%physx:physx+px-flags<physx+px-pvd-instrumentation-flag+enum+unsigned+char>
   '%physx:physx+px-pvd-instrumentation-flag+enum (cffi:foreign-enum-value
                                                   '%physx:physx+px-pvd-instrumentation-flag+enum
                                                   flag)))


(defun destroy-instrumentation-flags (instance)
  (iffi:destroy-intricate-instance
   '%physx:physx+px-flags<physx+px-pvd-instrumentation-flag+enum+unsigned+char>
   instance))


(defun connect-pvd (pvd &key (host "127.0.0.1") (port 5425))
  (let ((transport (cffi:with-foreign-string (address host)
                     (%physx:physx+px-default-pvd-socket-transport-create
                      'claw-utils:claw-string address
                      :int port
                      :unsigned-int 10)))
        (instrumentation-flags (make-instrumentation-flags :all)))
    (unwind-protect
         (prog1 transport
           (%physx:physx+connect '(:pointer %physx:physx+px-pvd) pvd
                                 '(:pointer %physx:physx+px-pvd-transport) transport
                                 '(:pointer %physx:physx+px-pvd-instrumentation-flags) instrumentation-flags))
      (destroy-instrumentation-flags instrumentation-flags))))


(defun destroy-transport (connection)
  (%physx:physx+release '(:pointer %physx:physx+px-pvd-transport) connection))


(defun destroy-pvd (pvd)
  (%physx:physx+release '(:pointer %physx:physx+px-pvd) pvd))

;;;
;;; PHYSICS
;;;
(defun make-physics (foundation pvd scale)
  (%physx:px-create-physics '%physx:physx+px-u32 67174656
                            '(:pointer %physx:physx+px-foundation) foundation
                            '(:pointer %physx:physx+px-tolerances-scale) scale
                            :bool t
                            '(:pointer %physx:physx+px-pvd) pvd))


(defun destroy-physics (physics)
  (%physx:physx+release '(:pointer %physx:physx+px-physics) physics))


(defun run-with-physics (foundation action)
  (with-scale (scale)
    (let* ((pvd (make-pvd foundation))
           (connection (connect-pvd pvd)))
      (let ((physics (make-physics foundation pvd scale)))
        (unwind-protect
             (funcall action physics)
          (destroy-physics physics)
          (destroy-transport connection)
          (destroy-pvd pvd))))))

;;;
;;; DISPATCHER
;;;
(defun make-cpu-dispatcher (thread-count)
  (%physx:physx+px-default-cpu-dispatcher-create
   '%physx:physx+px-u32 thread-count
   '(:pointer %physx:physx+px-u32) (cffi:null-pointer)))


(defun destroy-cpu-dispatcher (dispatcher)
  (%physx:physx+release
   '(:pointer %physx:physx+px-default-cpu-dispatcher) dispatcher))

;;;
;;; MATERIAL
;;;
(defun make-material (physics)
  (%physx:physx+create-material '(:pointer %physx:physx+px-physics) physics
                                 '%physx:physx+px-real 0.5f0
                                 '%physx:physx+px-real 0.5f0
                                 '%physx:physx+px-real 0.6f0))


(defun destroy-material (material)
  (%physx:physx+release '(:pointer %physx:physx+px-material) material))

;;;
;;; SCENE
;;;
(defun make-scene-descriptor (physics dispatcher)
  (let* ((scale (%physx:physx+get-tolerances-scale
                 :const '(:pointer %physx:physx+px-physics) physics))
         (descriptor (iffi:make-intricate-instance
                      '%physx:physx+px-scene-desc
                      '(:pointer %physx:physx+px-tolerances-scale) scale))
         (default-filter (iffi:intricate-function-pointer
                          '%physx:physx+px-default-simulation-filter-shader
                          '(:pointer %physx:physx+px-filter-flags)
                          '%physx:physx+px-filter-object-attributes
                          '(:pointer %physx:physx+px-filter-data)
                          '%physx:physx+px-filter-object-attributes
                          '(:pointer %physx:physx+px-filter-data)
                          '(:pointer %physx:physx+px-pair-flags)
                          '(:pointer :void)
                          '%physx:physx+px-u32)))
    (setf
     (iffi:intricate-slot-value descriptor '%physx:physx+px-scene-desc '%physx:cpu-dispatcher) dispatcher
     (iffi:intricate-slot-value descriptor '%physx:physx+px-scene-desc '%physx:filter-shader) default-filter)
    descriptor))


(defun scene-descriptor-valid-p (descriptor)
  (%physx:physx+is-valid :const '(:pointer %physx:physx+px-scene-desc) descriptor))


(defun destroy-scene-descriptor (descriptor)
  (iffi:destroy-intricate-instance '%physx:physx+px-scene-desc descriptor))


(defun (setf gravity) (value descriptor)
  (setf (iffi:intricate-slot-value descriptor '%physx:physx+px-scene-desc '%physx:gravity) value))


(defmacro with-scene-descriptor ((descriptor physics dispatcher) &body body)
  `(let ((,descriptor (make-scene-descriptor ,physics ,dispatcher)))
     (unwind-protect
          (progn ,@body)
       (destroy-scene-descriptor ,descriptor))))


(defun make-scene (physics descriptor)
  (%physx:physx+create-scene
   '(:pointer %physx:physx+px-physics) physics
   '(:pointer %physx:physx+px-scene-desc) descriptor))


(defun destroy-scene (scene)
  (%physx:physx+release '(:pointer %physx:physx+px-scene) scene))


(defun simulate-scene (scene step)
  (%physx:physx+simulate
   '(:pointer %physx:physx+px-scene) scene
   '%physx:physx+px-real (float step 0f0)
   '(:pointer %physx:physx+px-base-task) (cffi:null-pointer)
   '(:pointer :void) (cffi:null-pointer)
   '%physx:physx+px-u32 0
   ':bool t))


(defun finish-simulation (scene)
  (%physx:physx+fetch-results
   '(:pointer %physx:physx+px-scene) scene
   ':bool t
   '(:pointer %physx:physx+px-u32) (cffi:null-pointer)))


;;;
;;; ACTORS
;;;
(defun add-box (physics scene material)
  (let* ((geom (iffi:make-intricate-instance '%physx:physx+px-box-geometry
                                             '%physx:physx+px-real 0.5f0
                                             '%physx:physx+px-real 0.5f0
                                             '%physx:physx+px-real 0.5f0))
         (flags (iffi:make-intricate-instance
                 '%physx:physx+px-flags<physx+px-shape-flag+enum+unsigned+char>
                 '%physx:physx+px-shape-flag+enum (cffi:foreign-bitfield-value
                                                   '%physx:physx+px-shape-flag+enum
                                                   '(:visualization
                                                     :scene-query-shape
                                                     :simulation-shape))))
         (shape (%physx:physx+create-shape
                 '(:pointer %physx:physx+px-physics) physics
                 '(:pointer %physx:physx+px-geometry) geom
                 '(:pointer %physx:physx+px-material) material
                 ':bool nil
                 '(:pointer %physx:physx+px-shape-flags) flags))
         (transform (iffi:make-intricate-instance '%physx:physx+px-transform
                                                  '%physx:physx+px-identity :px-identity))
         (body (%physx:physx+create-rigid-dynamic
                '(:pointer %physx:physx+px-physics) physics
                '(:pointer %physx:physx+px-transform) transform)))
    (prog1 body
      (unwind-protect
           (progn
             (%physx:physx+attach-shape
              '(:pointer %physx:physx+px-rigid-actor) body
              '(:pointer %physx:physx+px-shape) shape)
             (%physx:physx+add-actor
              '(:pointer %physx:physx+px-scene) scene
              '(:pointer %physx:physx+px-actor) body
              '(:pointer %physx:physx+px-bvh-structure) (cffi:null-pointer)))
        (%physx:physx+release '(:pointer %physx:physx+px-shape) shape)
        (iffi:destroy-intricate-instance '%physx:physx+px-transform transform)
        (iffi:destroy-intricate-instance '%physx:physx+px-box-geometry geom)))))


(defun actor-global-pose (transform actor)
  (%physx:physx+get-global-pose :const
                                '(:pointer %physx:physx+px-transform) transform
                                '(:pointer %physx:physx+px-rigid-actor) actor))



;;;
;;; DEMO
;;;
(defun report-actor-position (actor)
  (with-transform (transform)
    (actor-global-pose transform actor)
    (let ((pos (transform-position transform)))
      (format t "~&POSITION: ~8F ~8F ~8F" (x pos) (y pos) (z pos)))
    (finish-output)))


(defun setup-scene (physics dispatcher)
  (with-scene-descriptor (descriptor physics dispatcher)
    (with-vec3 (vec)
      (setf (x vec) 0
            (y vec) -9.81
            (z vec) 0)
      (setf (gravity descriptor) vec))
    (unless (scene-descriptor-valid-p descriptor)
      (error "Scene descriptor invalid"))
    (make-scene physics descriptor)))


(defun run-simulation (physics)
  (let* ((dispatcher (make-cpu-dispatcher 2))
         (scene (setup-scene physics dispatcher))
         (material (make-material physics))
         (box (add-box physics scene material)))
    (unwind-protect
         (progn
           (report-actor-position box)
           (loop repeat 10
                 do (simulate-scene scene 0.14)
                    (finish-simulation scene)
                    (report-actor-position box)))
      (destroy-material material)
      (destroy-cpu-dispatcher dispatcher)
      (destroy-scene scene))))


(defun run-physics (foundation)
  (run-with-physics foundation #'run-simulation))


(defun run ()
  (run-with-foundation #'run-physics))

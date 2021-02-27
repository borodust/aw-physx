(asdf:defsystem :claw-physx
  :description "Wrapper over physx rendering engine"
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (:claw-physx-bindings))


(asdf:defsystem :claw-physx/wrapper
  :description "Wrapper over physx rendering engine"
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (:alexandria :cffi :cffi-c-ref :claw :claw-utils)
  :serial t
  :pathname "src/"
  :components ((:file "claw")
               (:module :bindings)
               (:module :physx-shared-includes :pathname "lib/physx/pxshared/include/")
               (:module :physx-includes :pathname "lib/physx/physx/include/")))


(asdf:defsystem :claw-physx/example
  :description ""
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (:claw-physx)
  :serial t
  :pathname "example/"
  :components ((:file "example")))

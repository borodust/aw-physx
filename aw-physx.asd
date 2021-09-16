(asdf:defsystem :aw-physx
  :description "Wrapper over physx rendering engine"
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (:aw-physx-bindings))


(asdf:defsystem :aw-physx/wrapper
  :description "Wrapper over physx rendering engine"
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (:alexandria :cffi :cffi-c-ref :claw :claw-utils)
  :serial t
  :components ((:file "src/claw")
               (:module :physx-shared-includes :pathname "src/lib/physx/pxshared/include/")
               (:module :physx-includes :pathname "src/lib/physx/physx/include/")))


(asdf:defsystem :aw-physx/example
  :description ""
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (:aw-physx)
  :serial t
  :pathname "example/"
  :components ((:file "example")))

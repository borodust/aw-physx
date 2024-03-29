(cl:defpackage :physx
  (:use :cl)
  (:export))

(cl:in-package :physx)

(claw.wrapper:defwrapper (:aw-physx
                          (:system :aw-physx/wrapper)
                          (:headers "PxConfig.h"
                                    "PxPhysicsAPI.h")
                          (:defines "NDEBUG" 1)
                          (:includes :physx-shared-includes :physx-includes)
                          (:targets ((:and :x86-64 :linux) "x86_64-pc-linux-gnu")
                                    ((:and :aarch64 :android) "aarch64-linux-android")
                                    ((:and :x86-64 :windows) "x86_64-w64-mingw32"))
                          (:persistent t :depends-on (:claw-utils))
                          (:language :c++)
                          (:include-definitions "^physx::.*" "^Px.*")
                          (:exclude-definitions "physx::PxVehicleDrive"
                                                "physx::PxRepXInstantiationArgs"
                                                "__"
                                                "^_"))
  :in-package :%physx
  :trim-enum-prefix t
  :recognize-bitfields t
  :recognize-strings t
  :with-adapter (:static
                 :extract-pointers ("PxDefaultSimulationFilterShader")
                 :path "src/lib/adapter.cxx")
  :override-types ((:string claw-utils:claw-string)
                   (:pointer claw-utils:claw-pointer)))

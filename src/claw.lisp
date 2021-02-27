(cl:defpackage :physx
  (:use :cl)
  (:export))

(cl:in-package :physx)

(defun build-physx ()
  (flet ((sh (command &optional dir)
           (uiop:with-current-directory (dir)
             (uiop:run-program command :force-shell t :output t :error-output t))))
    (let* ((work-dir (asdf:system-relative-pathname :claw-physx "src/lib/"))
           (physx-dir (merge-pathnames "physx/physx/" work-dir))
           (debug-project-dir (merge-pathnames "compiler/linux-debug/" physx-dir))
           (debug-bin-dir (merge-pathnames "bin/linux.clang/debug/" physx-dir))
           (repack-target (merge-pathnames "libphysx.a" work-dir)))
      (sh "./generate_projects.sh linux" physx-dir)
      (sh "cmake --build ." debug-project-dir)
      (apply #'claw.util::repack-blob-archives
             repack-target
             (uiop:directory-files debug-bin-dir "libPhysX*.a"))
      (claw.generator.common:build-adapter 'physx::claw-physx
                                           :dependencies repack-target
                                           :flags '("-Wno-c++11-narrowing")))))


(claw.wrapper:defwrapper (physx::claw-physx
                          (:system :claw-physx/wrapper)
                          (:headers "PxPhysicsAPI.h")
                          (:defines "_DEBUG" 1)
                          (:includes :physx-shared-includes :physx-includes)
                          (:targets ((:and :x86-64 :linux) "x86_64-pc-linux-gnu")
                                    ((:and :aarch64 :android) "aarch64-linux-android"))
                          (:persistent :claw-physx-bindings
                           :asd-path "../claw-physx-bindings.asd"
                           :bindings-path "../bindings/"
                           :depends-on (:claw-utils))
                          (:language :c++)
                          (:include-definitions "physx::.*" "Px.*")
                          (:exclude-definitions "physx::PxVehicleDrive"
                                                "physx::PxRepXInstantiationArgs"))
  :in-package :%physx
  :trim-enum-prefix t
  :recognize-bitfields t
  :recognize-strings t
  :with-adapter (:static
                 :extract-pointers ("PxDefaultSimulationFilterShader")
                 :path "lib/adapter.cxx")
  :override-types ((:string claw-utils:claw-string)
                   (:pointer claw-utils:claw-pointer)))

(cl:defpackage :physx
  (:use :cl)
  (:export))
(uiop:define-package :%physx
  (:use))

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
                          (:headers "PxPhysicsAPI.h")
                          (:defines "_DEBUG" 1)
                          (:includes :physx-shared-includes :physx-includes)
                          (:targets :local)
                          (:persistent nil)
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
                 :path "lib/adapter.c")
  :override-types ((:string claw-utils:claw-string)
                   (:pointer claw-utils:claw-pointer)))

cmake_minimum_required(VERSION 3.15)
project(physx.clawed)

set(LIBRARY_DIR ${CMAKE_CURRENT_SOURCE_DIR}/physx)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)
set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreadedDLL")

# setup rpath
set(CMAKE_SKIP_BUILD_RPATH FALSE)
set(CMAKE_BUILD_WITH_INSTALL_RPATH TRUE)
set(CMAKE_INSTALL_RPATH "$\{ORIGIN\}")

if (MSVC)
    set(TARGET_TRIPLE x86_64-w64-mingw32)
    if (NOT ANDROID_ABI)
      add_compile_options(/arch:AVX)
    endif()
else()
    add_compile_options(-O2)
    set(CLAWED_COMPILE_OPTIONS "-Wno-narrowing")
    if (NOT ANDROID_ABI)
      set(TARGET_TRIPLE x86_64-pc-linux-gnu)
      add_compile_options(-mavx)
    else()
      set(TARGET_TRIPLE aarch64-linux-android)
      add_link_options(-Wl,--strip-debug)
    endif()
endif()

#
# MAIN
#
set(TARGET physx.clawed)
set(ADAPTER "adapter.${TARGET_TRIPLE}.cxx")

add_library(${TARGET} SHARED ${ADAPTER})
set_source_files_properties(${ADAPTER} PROPERTIES LANGUAGE CXX)

target_include_directories(${TARGET}
  PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/../"
  PRIVATE "${LIBRARY_DIR}/"
  PRIVATE "${LIBRARY_DIR}/pxshared/include/"
  PRIVATE "${LIBRARY_DIR}/physx/include/")

target_compile_options(${TARGET} PUBLIC "${CLAWED_COMPILE_OPTIONS}")

target_link_directories(${TARGET}
  PRIVATE "${LIBRARY_DIR}/physx/bin/linux.clang/release/"
  PRIVATE "${LIBRARY_DIR}/physx/bin/linux.clang/debug/"
  PRIVATE "${LIBRARY_DIR}/physx/bin/android.arm64-v8a.fp-soft/release/"
  PRIVATE "${LIBRARY_DIR}/physx/bin/android.arm64-v8a.fp-soft/debug/"
  PRIVATE "${LIBRARY_DIR}/physx/bin/win.x86_64.vc142.md/release/"
  PRIVATE "${LIBRARY_DIR}/physx/bin/win.x86_64.vc142.md/debug/")


if(NOT ANDROID_ABI)
  target_link_libraries(${TARGET}
    PRIVATE PhysXExtensions_static_64
    PRIVATE PhysX_static_64
    PRIVATE PhysXPvdSDK_static_64
    PRIVATE PhysXVehicle_static_64
    PRIVATE PhysXCharacterKinematic_static_64
    PRIVATE PhysXCooking_static_64
    PRIVATE PhysXCommon_static_64
    PRIVATE PhysXFoundation_static_64)
else()
  find_library (ANDROID_LOG_LIB log)

  target_link_libraries(${TARGET}
    PRIVATE PhysXExtensions_static
    PRIVATE PhysX_static
    PRIVATE PhysXPvdSDK_static
    PRIVATE PhysXVehicle_static
    PRIVATE PhysXCharacterKinematic_static
    PRIVATE PhysXCooking_static
    PRIVATE PhysXCommon_static
    PRIVATE PhysXFoundation_static
    PRIVATE ${ANDROID_LOG_LIB})
endif()

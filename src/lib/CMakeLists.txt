cmake_minimum_required(VERSION 3.10)
project(physx.clawed)

set(LIBRARY_DIR ${CMAKE_CURRENT_SOURCE_DIR}/physx)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# setup rpath
set(CMAKE_SKIP_BUILD_RPATH FALSE)
set(CMAKE_BUILD_WITH_INSTALL_RPATH TRUE)
set(CMAKE_INSTALL_RPATH "$\{ORIGIN\}")

if (MSVC)
    add_compile_options(/O2)
    if (NOT ANDROID_ABI)
      add_compile_options(/arch:AVX)
    endif()
else()
    add_compile_options(-O2)
    if (NOT ANDROID_ABI)
      set(TARGET_TRIPLE x86_64-pc-linux-gnu)
      add_compile_options(-mavx)
    else()
      set(TARGET_TRIPLE aarch64-linux-android)
      add_compile_options(-mfpu=neon)
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

target_compile_options(${TARGET} PUBLIC -Wno-narrowing)

target_link_directories(${TARGET}
  PRIVATE ${LIBRARY_DIR}/physx/bin/linux.clang/release/
  PRIVATE ${LIBRARY_DIR}/physx/bin/linux.clang/debug/)

target_link_libraries(${TARGET}
  PRIVATE PhysXExtensions_static_64
          PhysX_static_64
          PhysXPvdSDK_static_64
          PhysXVehicle_static_64
          PhysXCharacterKinematic_static_64
          PhysXCooking_static_64
          PhysXCommon_static_64
          PhysXFoundation_static_64)

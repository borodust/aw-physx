#!/bin/bash

CC=clang
CXX=clang++

WORK_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
LIBRARY_DIR=$WORK_DIR/physx/
BUILD_TYPE=MinSizeRel

REST_ARGS=
while [[ $# -gt 0 ]]
do
key="$1"

case $key in
    --arch)
        TARGET_ARCH="$2"
        shift
        shift
        ;;
    --ndk)
        NDK="$2"
        shift
        shift
        ;;
    --physx-android-ndk)
        PHYSX_ANDROID_NDK="$2"
        shift
        shift
        ;;
    *)
        REST_ARGS+="$1"
        shift
        ;;
esac
done

BUILD_DIR="$WORK_DIR/build/$REST_ARGS/"

function build_android {
#
# Due to weirdest possible PhysX requirements this runs with MinGW on WIndows
#
    if [[ -z "$NDK" ]]; then
        echo "Path to Android NDK must be provided via --ndk"
        exit 1
    fi

    if [[ -z "$PHYSX_ANDROID_NDK" ]]; then
        echo "Path to Android NDK for PhysX must be provided via --physx-android-ndk"
        exit 1
    fi

    ANDROID_ABI=arm64-v8a
    case "$TARGET_ARCH" in
        aarch64)
            ANDROID_ABI=arm64-v8a
            ;;
        armv7a)
            ANDROID_ABI=armeabi-v7a
            ;;
        *)
            echo "Using ABI $ANDROID_ABI"
            ;;
    esac

    export PM_AndroidNDK_PATH="$PHYSX_ANDROID_NDK"
    cd $LIBRARY_DIR/physx/ && ./generate_projects.sh android-arm64-v8a \
        && cd $LIBRARY_DIR/physx/compiler/android-arm64-v8a-release/ \
        && cmake -DPX_BUILDSNIPPETS=OFF -DPX_BUILDPUBLICSAMPLES=OFF . \
        && mingw32-make

    mkdir -p $BUILD_DIR && cd $BUILD_DIR
    cmake -G "MinGW Makefiles" \
          -DCMAKE_BUILD_TYPE="$BUILD_TYPE" \
          -DANDROID_ABI=$ANDROID_ABI \
          -DANDROID_PLATFORM=23 \
          -DANDROID_ARM_NEON=ON \
          -DCMAKE_TOOLCHAIN_FILE="$NDK/build/cmake/android.toolchain.cmake" \
          $WORK_DIR
    cmake --build . --config "$BUILD_TYPE"
}

function build_desktop {
    cd $LIBRARY_DIR/physx/ && ./generate_projects.sh linux && \
        cd $LIBRARY_DIR/physx/compiler/linux-release/ && cmake --build .

    mkdir -p $BUILD_DIR && cd $BUILD_DIR
    cmake -DCMAKE_C_COMPILER=clang \
          -DCMAKE_CXX_COMPILER=clang++ \
          -DCMAKE_SHARED_LINKER_FLAGS="-stdlib=libc++ -lc++abi" \
          $WORK_DIR
    cmake --build . --config "$BUILD_TYPE"
}

export CMAKE_PX_BUILDPUBLICSAMPLES=OFF
export CMAKE_DPX_BUILDSNIPPETS=OFF

case "$REST_ARGS" in
    desktop)
        build_desktop
        ;;
    android)
        build_android
        ;;
    *)
        echo "Unrecognized platform $REST_ARGS"
        exit -1
        ;;
esac

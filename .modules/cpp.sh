#!/bin/bash
#
# C/C++ programming tools.
#
# Author: Danylo Malyuta, 2022.

# ..:: CMake ::..
# C++ build system

CMAKE_VERSION="3.23.1"
if [[ $(not_installed cmake) || \
       "$(cmake --version | head -n 1 | cut -d ' ' -f 3)" != \
       "$CMAKE_VERSION" ]]; then
    # Dependencies
    sudo apt-get -y install libssl-dev

    wget -4 https://github.com/Kitware/CMake/releases/download/v"$CMAKE_VERSION"/cmake-"$CMAKE_VERSION".tar.gz -P /tmp/
    tar -xvf /tmp/cmake-"$CMAKE_VERSION".tar.gz -C /tmp
    ( cd /tmp/cmake-"$CMAKE_VERSION"/ && ./bootstrap && make && sudo make install )
fi

# ..:: Bazel ::..
# C++ build system

if not_installed bazel; then
    # Instructions: https://docs.bazel.build/versions/main/install-ubuntu.html
    sudo apt-get -y install apt-transport-https curl gnupg
    curl -fsSL https://bazel.build/bazel-release.pub.gpg | gpg --dearmor > bazel.gpg
    sudo mv bazel.gpg /etc/apt/trusted.gpg.d/
    echo "deb [arch=amd64] https://storage.googleapis.com/bazel-apt stable jdk1.8" | sudo tee \
        /etc/apt/sources.list.d/bazel.list

    sudo apt-get update
    sudo apt-get -y install bazel
fi

# ..:: LLVM / Clang ::..

CLANG_VERSION=14

if not_installed clang-$CLANG_VERSION; then
    wget -4 https://apt.llvm.org/llvm.sh -P /tmp/
    chmod +x /tmp/llvm.sh
    sudo /tmp/llvm.sh $CLANG_VERSION

    sudo ln -sf /usr/lib/llvm-14/bin/clang /usr/bin/clang
    sudo update-alternatives --install /usr/bin/clang clang /usr/lib/llvm-14/bin/clang 100
    sudo update-alternatives --set clang /usr/lib/llvm-14/bin/clang
fi

# ..:: Debugging ::..

sudo apt-get -y install ddd

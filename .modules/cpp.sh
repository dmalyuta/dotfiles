#!/bin/bash
#
# C/C++ programming tools.
#
# Author: Danylo Malyuta, 2022.

# ..:: CMake ::..
# C++ build system

if not_installed cmake; then
    # Dependencies
    sudo apt-get -y install libssl-dev

    wget -4 https://github.com/Kitware/CMake/releases/download/v3.18.4/cmake-3.18.4.tar.gz -P /tmp/
    tar -xvf /tmp/cmake-3.18.4.tar.gz -C /tmp
    ( cd /tmp/cmake-3.18.4/ && ./bootstrap && make && sudo make install )
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

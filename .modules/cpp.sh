#!/bin/bash
#
# C/C++ programming tools.
#
# Author: Danylo Malyuta, 2022.

# ..:: CMake ::..
# C++ build system

CMAKE_VERSION="3.23.1"
if not_installed cmake || [[ "$(cmake --version | head -n 1 | cut -d ' ' -f 3)" != "$CMAKE_VERSION" ]]; then
    # Dependencies
    sudo apt-get -y install libssl-dev

    wget -4 https://github.com/Kitware/CMake/releases/download/v"$CMAKE_VERSION"/cmake-"$CMAKE_VERSION".tar.gz -P /tmp/
    tar -xvf /tmp/cmake-"$CMAKE_VERSION".tar.gz -C /tmp
    ( cd /tmp/cmake-"$CMAKE_VERSION"/ && ./bootstrap && make -j2 && sudo make install )
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

    sudo apt-get install clang-tidy-14 clang-format-14 clang-tools-14 llvm-14-dev lld-14 lldb-14 \
        llvm-14-tools libomp-14-dev libc++-14-dev libc++abi-14-dev libclang-common-14-dev \
        libclang-14-dev libclang-cpp14-dev libunwind-14-dev

    sudo ln -sf /usr/lib/llvm-14/bin/clang /usr/bin/clang
    sudo update-alternatives --install /usr/bin/clang clang /usr/lib/llvm-14/bin/clang 100
    sudo update-alternatives --set clang /usr/lib/llvm-14/bin/clang
fi

# ..:: Custom sysroot ::..

if [ ! -d /usr/local/custom_sysroot ]; then

sudo -- -sh -c <<EOF
mkdir /usr/local/custom_sysroot
cd /usr/local/custom_sysroot
mkdir usr
cd usr/
mkdir include
mkdir lib
mkdir libexec
cd include/
###########################
### Set up /usr/include ###
cp -Rs /usr/include/* .
###########################
cd ../lib/
#######################
### Set up /usr/lib ###
cp -s /usr/lib/x86_64-linux-gnu/*.a .
cp -s /usr/lib/x86_64-linux-gnu/*.o .
cp -s /usr/lib/x86_64-linux-gnu/*.so* .
mkdir x86_64-linux-gnu
ln -s /usr/lib/x86_64-linux-gnu/libc_nonshared.a x86_64-linux-gnu/
#######################
cd ..
#########################
### Set up /usr/lib64 ###
ln -s lib lib64
#########################
cd ..
mkdir lib
cd lib
###################
### Set up /lib ###
ln -s /lib/x86_64-linux-gnu .
mkdir -p gcc/x86_64-linux-gnu/12
ln -s /lib/gcc/x86_64-linux-gnu/12/* gcc/x86_64-linux-gnu/12/
###################
cd ..
mkdir lib64
cd lib64
#####################
### Set up /lib64 ###
ln -s /lib64/ld-linux-x86-64.so.2 .
#####################
EOF

fi

# ..:: Debugging ::..

sudo apt-get -y install ddd

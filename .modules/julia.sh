#!/bin/bash
#
# Julia programming tools.
#
# Author: Danylo Malyuta, 2020.

if not_installed julia; then
    JULIA_VERSION=1.7.1
    BASE_JULIA_VERSION="$(echo $JULIA_VERSION | cut -d '.' -f 1-2)"
    JULIA_URL=https://julialang-s3.julialang.org/bin/linux/x64/$BASE_JULIA_VERSION/julia-$JULIA_VERSION-linux-x86_64.tar.gz
    wget -4 $JULIA_URL -P /tmp/
    tar -xvzf /tmp/julia-$JULIA_VERSION-linux-x86_64.tar.gz -C /tmp
    sudo cp -r /tmp/julia-$JULIA_VERSION /opt/
    sudo ln -sf /opt/julia-$JULIA_VERSION/bin/julia /usr/local/bin/julia

    # This makes PackageCompiler.jl work
    sudo ln -f /opt/julia-$JULIA_VERSION/lib/libjulia.so.$BASE_JULIA_VERSION \
	 /usr/lib/x86_64-linux-gnu/libjulia.so
fi

# Install Jupyter notebook module

echo 'using Pkg; Pkg.status()' | julia --startup-file=no | \
    grep IJulia > /dev/null 2>&1
if [ $? -ne 0 ]; then

    cat << EOF | julia --startup-file=no
using Pkg
Pkg.add("IJulia")
Pkg.add("OhMyREPL")
Pkg.add("BenchmarkTools")

Pkg.add("LanguageServer")

Pkg.add("LinearAlgebra")
Pkg.add("JuMP")
Pkg.add("ECOS")

Pkg.add("PyPlot")
Pkg.add("Colors")
Pkg.add("Printf")

Pkg.add("Test")
EOF

fi

#!/bin/bash
#
# Julia programming tools.
#
# Author: Danylo Malyuta, 2020.

if not_installed julia; then
    wget -4 https://julialang-s3.julialang.org/bin/linux/x64/1.5/julia-1.5.3-linux-x86_64.tar.gz \
	 -P /tmp/
    tar -xvzf /tmp/julia-1.5.3-linux-x86_64.tar.gz -C /tmp
    sudo cp -r /tmp/julia-1.5.3 /opt/
    sudo ln -sf /opt/julia-1.5.3/bin/julia /usr/local/bin/julia

    # This makes PackageCompiler.jl work
    sudo ln -f /opt/julia-1.5.3/lib/libjulia.so.1.5 \
	 /usr/lib/x86_64-linux-gnu/libjulia.so
fi

# Install Jupyter notebook module

echo 'using Pkg; Pkg.status()' | julia | grep IJulia > /dev/null 2>&1
if [ $? -ne 0 ]; then
    echo 'using Pkg; Pkg.add("IJulia")' | julia
fi

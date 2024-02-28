#!/bin/bash

set -exo pipefail

git clone https://github.com/openfheorg/openfhe-development.git OpenFHE
echo "Build OpenFHE v1.1.2"
if [ ! -d "OpenFHE/build" ] ; then
    cd ./OpenFHE
    git reset --hard b2869ae
    mkdir -p build && cd build
    cmake -DBUILD_UNITTESTS=OFF -DBUILD_EXAMPLES=OFF -DBUILD_BENCHMARKS=OFF ..
    make -j 10
    sudo make install
    sudo ln -s /usr/local/lib/libOpenFHEcore.so.1 /usr/lib/libOpenFHEcore.so.1
    sudo ln -s /usr/local/lib/libOPENFHEbinfhe.so.1 /usr/lib/libOPENFHEbinfhe.so.1
    sudo ln -s /usr/local/lib/libOPENFHEpke.so.1 /usr/lib/libOPENFHEpke.so.1
    cd ../..
else
    echo "Found in cache"
fi

git clone https://github.com/microsoft/SEAL.git
echo "Build SEAL v4.1.1"
if [ ! -d "SEAL/build" ] ; then
    cd ./SEAL
    git reset --hard 206648d0e4634e5c61dcf9370676630268290b59
    cmake -S . -B build -DSEAL_BUILD_BENCH=OFF -DSEAL_BUILD_EXAMPLES=OFF -DSEAL_BUILD_TESTS=OFF
    cmake --build build
    sudo cmake --install build
    cd ..
else
    echo "Found in cache"
fi

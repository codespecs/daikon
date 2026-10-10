#!/bin/bash

cd source || exit 2
./configure --disable-lib-checks --disable-optimiz --enable-debug --prefix=$(pwd) COMPILED_BY="KVASIR TEST SUITE"
make && make install
cp bin/povray ../

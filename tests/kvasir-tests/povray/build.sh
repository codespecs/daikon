#!/bin/bash
cd source
./configure --disable-lib-checks --disable-optimiz --enable-debug --prefix=`pwd` COMPILED_BY="KVASIR TEST SUITE"
make && make install
cp bin/povray ../

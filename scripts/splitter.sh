#!/bin/sh

cp -p $1 $1.bak
rmcomm <$1.bak >$1
extractcond.pl $1
gensplitter.pl $1 $2
rm $1.conds
mv $1.bak $1

#!/bin/sh
perl -pi -e 's/^package .+$/package MapQuick1;/;' *.java 

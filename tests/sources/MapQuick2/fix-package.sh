#!/bin/sh
perl -pi -e 's/^package .+$/package MapQuick2;/;' *.java 

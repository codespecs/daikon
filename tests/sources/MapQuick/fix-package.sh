#!/bin/sh
perl -pi -e 's/^package .+$/package MapQuick;/;' *.java 
#perl -pi.bak -e 's/^package .+\n$/package MapQuick;\n\nimport MapQuick2.*\n\n/;' *.java 
#perl -pi -e 's/^import MapQuick2.*\n$/import MapQuick2.*;\n/;' *.java 

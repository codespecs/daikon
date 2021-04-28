#!/bin/sh

# Fail the whole script if any command fails
set -e
# Echo commands before executing them
set -x

java -cp "$HOME"/java/jarjar-pantsbuild/dist/jarjar.jar org.pantsbuild.jarjar.Main process daikon-plumelib-rules.txt plume-util-*.jar daikon-util.jar
java -cp "$HOME"/java/jarjar-pantsbuild/dist/jarjar.jar org.pantsbuild.jarjar.Main process daikon-plumelib-rules.txt bcel-util-*.jar daikon-bcelutil.jar
java -cp "$HOME"/java/jarjar-pantsbuild/dist/jarjar.jar org.pantsbuild.jarjar.Main process daikon-plumelib-rules.txt options-*.jar daikon-options.jar
java -cp "$HOME"/java/jarjar-pantsbuild/dist/jarjar.jar org.pantsbuild.jarjar.Main process daikon-plumelib-rules.txt reflection-util-*.jar daikon-reflection.jar
mkdir -p daikonplume
cd daikonplume
jar -xf ../daikon-bcelutil.jar
jar -xf ../daikon-options.jar
jar -xf ../daikon-reflection.jar
jar -xf ../daikon-util.jar
jar cf daikon-plumelib.jar ./*
cd ..
rm daikon*.jar
mv daikonplume/*.jar .
rm -rf daikonplume

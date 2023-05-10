#!/bin/sh

# Fail the whole script if any command fails
set -e
# Echo commands before executing them
set -x

java -cp "$HOME"/java/jarjar-pantsbuild/dist/jarjar.jar org.pantsbuild.jarjar.Main process daikon-plumelib-rules.txt bcel-util-*.jar daikon-bcel-util.jar
java -cp "$HOME"/java/jarjar-pantsbuild/dist/jarjar.jar org.pantsbuild.jarjar.Main process daikon-plumelib-rules.txt hashmap-util-*.jar daikon-hashmap-util.jar
java -cp "$HOME"/java/jarjar-pantsbuild/dist/jarjar.jar org.pantsbuild.jarjar.Main process daikon-plumelib-rules.txt options-*.jar daikon-options.jar
java -cp "$HOME"/java/jarjar-pantsbuild/dist/jarjar.jar org.pantsbuild.jarjar.Main process daikon-plumelib-rules.txt plume-util-*.jar daikon-plume-util.jar
java -cp "$HOME"/java/jarjar-pantsbuild/dist/jarjar.jar org.pantsbuild.jarjar.Main process daikon-plumelib-rules.txt reflection-util-*.jar daikon-reflection-util.jar
mkdir -p daikonplume
cd daikonplume
jar -xf ../daikon-bcel-util.jar
jar -xf ../daikon-hashmap-util.jar
jar -xf ../daikon-options.jar
jar -xf ../daikon-plume-util.jar
jar -xf ../daikon-reflection-util.jar
jar cf daikon-plumelib.jar ./*
cd ..
rm daikon*.jar
mv daikonplume/*.jar .
rm -rf daikonplume

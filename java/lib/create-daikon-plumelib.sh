#!/bin/sh

# Fail the whole script if any command fails
set -e
# Echo commands before executing them
set -x

# JARJAR_JARFILE="$HOME"/java/jarjar-pantsbuild/dist/jarjar.jar
# JARJAR_MAINCLASS=org.pantsbuild.jarjar.Main
JARJAR_JARFILE="$HOME"/java/jarjar-shevek/jarjar-command-all.jar
JARJAR_MAINCLASS=com.tonicsystems.jarjar.Main

java -cp "$JARJAR_JARFILE" "$JARJAR_MAINCLASS" --mode process --rules daikon-plumelib-rules.txt bcel-util-*.jar --output daikon-bcel-util.jar
java -cp "$JARJAR_JARFILE" "$JARJAR_MAINCLASS" --mode process --rules daikon-plumelib-rules.txt hashmap-util-*.jar --output daikon-hashmap-util.jar
java -cp "$JARJAR_JARFILE" "$JARJAR_MAINCLASS" --mode process --rules daikon-plumelib-rules.txt options-*.jar --output daikon-options.jar
java -cp "$JARJAR_JARFILE" "$JARJAR_MAINCLASS" --mode process --rules daikon-plumelib-rules.txt plume-util-*.jar --output daikon-plume-util.jar
java -cp "$JARJAR_JARFILE" "$JARJAR_MAINCLASS" --mode process --rules daikon-plumelib-rules.txt reflection-util-*.jar --output daikon-reflection-util.jar
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

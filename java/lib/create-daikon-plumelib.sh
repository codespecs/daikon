#!/bin/sh

# Fail the whole script if any command fails
set -e
# Echo commands before executing them
set -x

JARJAR_JAR_FILE="$HOME"/java/jarjar-fork-shevek/jarjar-command/build/libs/jarjar-command-1.1.1.jar

java -jar "$JARJAR_JAR_FILE" --mode process --rules daikon-plumelib-rules.txt --output daikon-bcel-util.jar bcel-util-*.jar
java -jar "$JARJAR_JAR_FILE" --mode process --rules daikon-plumelib-rules.txt --output daikon-hashmap-util.jar hashmap-util-*.jar
java -jar "$JARJAR_JAR_FILE" --mode process --rules daikon-plumelib-rules.txt --output daikon-options.jar options-*.jar
java -jar "$JARJAR_JAR_FILE" --mode process --rules daikon-plumelib-rules.txt --output daikon-plume-util.jar plume-util-*.jar
java -jar "$JARJAR_JAR_FILE" --mode process --rules daikon-plumelib-rules.txt --output daikon-reflection-util.jar reflection-util-*.jar
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

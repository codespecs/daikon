###########################################################################

# Read ../Makefile-fora-project for what these declarations mean

PROJECT:=WeightedNodePath
TRACE_NAME=WeightedNodePath.dtrace

PACKAGE:=MapQuick

SOURCE_UNCHANGED:=${PACKAGE}/PS3PrivateTest.java ${PACKAGE}/PS3TestSuite.java ${PACKAGE}/PS3TestDriver.java ${PACKAGE}/WeightedNode.java ${PACKAGE}/Path.java ${PACKAGE}/PathFinder.java ${PACKAGE}/Graph.java ${PACKAGE}/PriorityQueue.java
SOURCE_TO_DFEJ:=${PACKAGE}/WeightedNodePath.java

MAIN_RUN:=-Dtest.dir=${HOME}/research/invariants/esc-experiments/MapQuick/ps3-tests junit.textui.TestRunner MapQuick.PS3PrivateTest

include ../Makefile-fora-project

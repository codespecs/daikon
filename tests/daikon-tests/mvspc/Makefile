###########################################################################

# Read Makefile.common for what all of this means

MAIN_CLASS:=who.knows
PROJECT_FILE:=mvspc

include ../../Makefile.common

#instrument-files-revise:
#	rm -f ${INST_LIST_FILE}
#	echo "who.knows" > ${INST_LIST_FILE}


# ###########################################################################
# ### mvspc (Melissa Hao)
# ###

# # Melissa Hao's version:  /g1/users/mhao/Daikon/tests/mvspc

# mvspc: mvspc-batch

# mvspc-interactive: dfej-mvspc javac-mvspc run-mvspc-interactive daikon-mvspc

# mvspc-batch: dfej-mvspc javac-mvspc run-mvspc-batch daikon-mvspc

# # Run this just once (ever).
# mvsps-setup:
# 	# packages/mvspc/daikon-java/daikon is a symlink to the main
# 	# daikon directory, because appletviewer expects its classes to be 
# 	# in the same directory as the applet.
# 	ln -s ../daikon packages/mvspc/daikon-java/daikon
# 	cd packages/mvspc/daikon-java; ln -s ../VisualNumerics ../*.html ../*.dat ../*.htl ../*.gif .

# dfej-mvspc:
# 	cd packages/mvspc && ${DFEJ} *.java

# javac-mvspc:
# 	cp -Rpf packages/mvspc/VisualNumerics packages/mvspc/daikon-java/VisualNumerics
# 	cd packages/mvspc/daikon-java && ${JAVAC} *.java

# # Melissa says:
# # Applet status: If I quit the applet right away, when I run daikon it
# # works.  If I actually use the applet for a while, the weird thing is
# # that Daikon crashed once (with a different error meesage from before),
# # but Daikon has worked since then so I guess I won't worry about that
# # crash.
# run-mvspc-interactive:
# 	@echo "From the first drop-down menu, select the second item (sort2.dat)."
# 	@echo "When the chart at the bottom appears, move the mouse around the stars,"
# 	@echo "and click-and-drag some stars with the left or right mouse button."
# 	cd packages/mvspc/daikon-java && appletviewer -J-Djava.security.policy=javaPolicy index.html
# 	cd packages/mvspc/daikon-java/daikon-output && mv mvspc.dtrace mvspc-interactive.dtrace
# 	${MODBIT_MUNGE} packages/mvspc/daikon-java/daikon-output/mvspc-interactive.dtrace

# run-mvspc-batch:
# 	-cd packages/mvspc/daikon-java && (CLASSPATH=${TESTS}/packages/mvspc/daikon-java:${CLASSPATH}; exec_cpu_limited 5 appletviewer -J-Djava.security.policy=javaPolicy index.html)
# 	cd packages/mvspc/daikon-java/daikon-output && mv mvspc.dtrace mvspc-batch.dtrace
# 	${MODBIT_MUNGE} packages/mvspc/daikon-java/daikon-output/mvspc-batch.dtrace

# daikon-mvspc-interactive:
# 	${DAIKON} packages/mvspc/daikon-output/*.decls packages/mvspc/daikon-java/daikon-output/mvspc-interactive.dtrace >| packages/mvspc/mvspc-interactive.result

# daikon-mvspc-batch:
# 	${DAIKON} packages/mvspc/daikon-output/*.decls packages/mvspc/daikon-java/daikon-output/mvspc-batch.dtrace >| packages/mvspc/mvspc-batch.result

# daikon-mvspc: daikon-mvspc-batch


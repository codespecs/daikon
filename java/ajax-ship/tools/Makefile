JAVAC = javac -g
CLASSPATH = /usr0/roc/dev/ajax
JAVA_FILES := $(shell find . -follow -name \*.java -print)

all: .build_java

.build_java : $(JAVA_FILES)
	$(JAVAC) -classpath $(CLASSPATH) $?
	@touch $@

# This target creates daikon-plumelib.jar, whether or not it already exists.
all:
	$(MAKE) build-jarjar
	\rm -f daikon-plumelib.jar
	./create-daikon-plumelib.sh

~/java/jarjar-fork-shevek:
	(cd ~/java && git clone https://github.com/shevek/jarjar jarjar-fork-shevek)

build-jarjar: ~/java/jarjar-fork-shevek
	@git -C ~/java/jarjar-fork-shevek pull
	(cd ~/java/jarjar-fork-shevek && ./gradlew build)

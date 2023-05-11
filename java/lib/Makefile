# This target creates daikon-plumelib.jar, whether or not it already exists.
all:
	$(MAKE) build-jarjar
	\rm -f daikon-plumelib.jar
	./create-daikon-plumelib.sh

~/java/jarjar-pantsbuild:
	(cd ~/java && git clone https://github.com/pantsbuild/jarjar jarjar-pantsbuild)

build-jarjar: ~/java/jarjar-pantsbuild
	@(cd ~/java/jarjar-pantsbuild && git pull)
#	(cd ~/java/jarjar-pantsbuild && ./pants doc test ::)
	(cd ~/java/jarjar-pantsbuild && ~/bin/pants test ::)

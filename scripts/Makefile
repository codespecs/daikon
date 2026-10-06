HIERDIR = ../tools/hierarchical
KMDIR = ../tools/kmeans

# These are executables and/or downloaded from the Internet.
all: difftbl cluster clgroup kmeans trace-untruncate-fast checkargs.pm dockerfiles
.PHONY: all clean tags dockerfiles

clean:
	rm -f difftbl cluster clgroup kmeans trace-untruncate-fast
	${MAKE} -C ${HIERDIR} clean
	${MAKE} -C ${KMDIR} clean

TAGS: tags
tags:
	etags `find . | grep -v '~\|^\.$$\|^\./[kx]means$$'`

difftbl: ${HIERDIR}/difftbl
	cp -p $^ $@

cluster: ${HIERDIR}/cluster
	cp -p $^ $@

clgroup: ${HIERDIR}/clgroup
	cp -p $^ $@

kmeans: ${KMDIR}/kmeans
	cp -p $^ $@

trace-untruncate-fast: trace-untruncate-fast.c
	gcc -Wall -g $< -o $@

## Dockerfiles

DOCKERFILE_OSES := ubuntu rockylinux
DOCKERFILE_JDKS := 11 17 21 25 27
DOCKERFILES := $(foreach os,${DOCKERFILE_OSES},$(foreach jdk,${DOCKERFILE_JDKS},Dockerfile-${os}-jdk${jdk} Dockerfile-${os}-jdk${jdk}-plus))
# Remove this line when DOCKERFILE_JDKS no longer contains 11 or 17.
DOCKERFILES := $(filter-out Dockerfile-rockylinux-jdk11% Dockerfile-rockylinux-jdk17%,${DOCKERFILES})

dockerfiles: ${DOCKERFILES}

# The stem $* is, for example, "ubuntu-jdk21" or "ubuntu-jdk21-plus".
${DOCKERFILES}: Dockerfile-%: Dockerfile-defs.m4 $(addsuffix .m4,$(addprefix Dockerfile-,${DOCKERFILE_OSES}))
	m4 -P -DJDKVER=$(patsubst jdk%,%,$(word 2,$(subst -, ,$*))) \
	  -DPLUS=$(if $(filter %-plus,$*),1,0) \
	  Dockerfile-defs.m4 Dockerfile-$(word 1,$(subst -, ,$*)).m4 > $@.tmp
	cat -s $@.tmp > $@.tmp2
	mv -f $@.tmp2 $@
	rm -f $@.tmp

## Docker images

# Build each Docker image and upload it to Docker Hub.
# To build and upload just one image, run, for example:
#   make docker-image-ubuntu-jdk21-plus
# To build images named "...-testing", run:
#   make docker-images DOCKERTESTING=-testing
# and also update the value of `docker_testing` in file ../.azure/defs-common.m4
# and regenerate the CI configuration files (see Dockerfile-README.md).
DOCKERTESTING ?=
# Must match `docker_userid` in file ../.azure/defs-common.m4 .
DOCKER_USERID := mdernst
# The stem $* is, for example, "ubuntu-jdk21" or "ubuntu-jdk21-plus".
DOCKER_IMAGE = ${DOCKER_USERID}/daikon-$*${DOCKERTESTING}
DOCKER_IMAGES := $(patsubst Dockerfile-%,docker-image-%,${DOCKERFILES})
.PHONY: docker-images ${DOCKER_IMAGES}
docker-images: ${DOCKER_IMAGES}

# The Dockerfiles copy no files into the image, so build with no context.
${DOCKER_IMAGES}: docker-image-%: Dockerfile-%
	@echo "***** Starting ${DOCKER_IMAGE} *****"
	docker -l warn build --rm=false --progress=plain -t ${DOCKER_IMAGE} - < $< \
	  && docker -l warn push --quiet ${DOCKER_IMAGE} \
	  || { echo "***** FAILURE in ${DOCKER_IMAGE} *****"; exit 1; }
	@echo "***** Success for ${DOCKER_IMAGE} *****"

## Delegation to other Makefiles (gross)

${HIERDIR}/difftbl: ${HIERDIR}/difftbl.c
	${MAKE} -C ${HIERDIR} difftbl

${HIERDIR}/cluster: ${HIERDIR}/cluster.c
	${MAKE} -C ${HIERDIR} cluster

${HIERDIR}/clgroup: ${HIERDIR}/clgroup.c
	${MAKE} -C ${HIERDIR} clgroup

# This rule lists no dependencies, because doing so would just duplicate
# what is already in that Makefile.
${KMDIR}/kmeans:
	${MAKE} -C ${KMDIR} kmeans

checkargs.pm:
	wget https://raw.githubusercontent.com/plume-lib/html-tools/master/checkargs.pm

.PHONY: test
test: all

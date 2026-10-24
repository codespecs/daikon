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
# Rocky Linux 10 does not package JDK 11 or 17.  This special case is
# temporary:  remove this line when DOCKERFILE_JDKS no longer contains 11 or 17.
DOCKERFILES := $(filter-out Dockerfile-rockylinux-jdk11% Dockerfile-rockylinux-jdk17%,${DOCKERFILES})

dockerfiles: ${DOCKERFILES}

# The stem $* is, for example, "ubuntu-jdk21" or "ubuntu-jdk21-plus".
${DOCKERFILES}: Dockerfile-%: Dockerfile-defs.m4 $(addsuffix .m4,$(addprefix Dockerfile-,${DOCKERFILE_OSES}))
	m4 -P -DJDKVER=$(patsubst jdk%,%,$(word 2,$(subst -, ,$*))) \
	  -DPLUS=$(if $(filter %-plus,$*),1,0) \
	  Dockerfile-defs.m4 Dockerfile-$(word 1,$(subst -, ,$*)).m4 > $@.tmp \
	  || { rm -f $@.tmp; exit 1; }
	cat -s $@.tmp > $@.tmp2
	mv -f $@.tmp2 $@
	rm -f $@.tmp

## Docker images

# Build each Docker image and upload it to Docker Hub.
# To build and upload just one image, run, for example:
#   make docker-image-ubuntu-jdk21-plus
# The Docker Hub user and the "-testing" suffix come from file
# ../.azure/defs-common.m4, which also determines the images that CI uses.
# DOCKER_M4_VALUE fails if m4 fails, rather than silently yielding a wrong
# image name such as one that lacks the "-testing" suffix.  The "@@" markers
# distinguish the value (which may be empty) from any other output of
# defs-common.m4.
DOCKER_M4_VALUE = $(shell out=$$(printf '@@%s@@\n' '$(1)' | m4 ../.azure/defs-common.m4 -) && line=$$(printf '%s\n' "$$out" | grep -x '@@.*@@') && line=$${line#@@} && printf '%s\n' "$${line%@@}")$(if $(filter 0,${.SHELLSTATUS}),,$(error m4 failed to compute $(1)))
# In m4, "[]" separates "$*" from the macro name "docker_testing".
DOCKER_IMAGE = $(call DOCKER_M4_VALUE,docker_userid/daikon-$*[]docker_testing)
DOCKER_IMAGES := $(patsubst Dockerfile-%,docker-image-%,${DOCKERFILES})
.PHONY: docker-images ${DOCKER_IMAGES}
docker-images: ${DOCKER_IMAGES}
# Building the images in parallel can exhaust memory, disk space, or Docker
# Hub rate limits, so build them one at a time even if make is run with -j.
.NOTPARALLEL: docker-images

# The Dockerfiles copy no files into the image, so build with no context.
# --pull ensures that the image uses the latest base image.  Docker reuses
# cached layers, so re-running is fast when nothing has changed.
# The image must match the committed Dockerfile, which make might have just
# regenerated from the .m4 files.  "git diff" does not report an untracked
# file, such as a Dockerfile for a JDK version just added to DOCKERFILE_JDKS,
# so first check that the Dockerfile is tracked.
${DOCKER_IMAGES}: docker-image-%: Dockerfile-%
	git ls-files --error-unmatch -- $< > /dev/null 2>&1 || { echo "$< is not committed; commit it first" >&2; exit 1; }
	git diff --quiet HEAD -- $< || { echo "$< differs from the committed version; commit it first" >&2; exit 1; }
	docker -l warn build --pull --progress=plain -t ${DOCKER_IMAGE} - < $<
	docker -l warn push --quiet ${DOCKER_IMAGE}

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

# Create a Docker image that is ready to run the Daikon tests,
# using JDK 8.

FROM fedora
MAINTAINER Michael Ernst <mernst@cs.washington.edu>

# According to
# https://docs.docker.com/engine/userguide/eng-image/dockerfile_best-practices/:
#  * Put "apt-get update" and "apt-get install" in the same RUN command.
#  * Do not run "apt-get upgrade"; instead get upstream to update.
RUN dnf -qy upgrade && dnf -qy install \
  autoconf \
  automake \
  bc \
  binutils-devel \
  gcc \
  git \
  m4 \
  make \
  redhat-lsb \
  rsync \
  tar \
  unzip \
  which \
  java-1.8.0-openjdk \
  java-1.8.0-openjdk-devel \
&& dnf -q clean all

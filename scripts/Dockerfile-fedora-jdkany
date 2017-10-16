# Create a Docker image that is ready to run the full Daikon tests,
# including building the manual and Javadoc.
# But it's used primarily for running miscellaneous tests such as the manual
# and Javadoc.

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
&& dnf -qy install \
  curl \
  ctags \
  gcc-c++ \
  graphviz \
  netpbm \
  python \
  texi2html \
  texinfo \
  texinfo-tex \
  texlive \
  wget \
&& dnf -q clean all

dockerfile_header(Dockerfile-rockylinux.m4)

FROM rockylinux:9
LABEL org.opencontainers.image.authors="Michael Ernst <mernst@cs.washington.edu>"

# According to
# https://docs.docker.com/engine/userguide/eng-image/dockerfile_best-practices/:
#  * Put "apt-get update" and "apt-get install" in the same RUN command.
#  * Do not run "apt-get upgrade"; instead get upstream to update.

# The EPEL repository contains certain packages.
RUN dnf -q -y upgrade && dnf -q -y install \
  epel-release

# curl is installed by default on Rocky Linux.
RUN dnf -q -y upgrade && dnf -q -y install \
  autoconf \
  automake \
  bc \
  binutils-devel \
  diffutils \
  findutils \
  gcc \
  git \
  jq \
  libxcrypt-devel \
  m4 \
  make \
  perl-English \
  perl-filetest \
  rsync \
  tar \
  unzip \
  wget \
  which

# Install the JDK.
m4_ifelse(jdk_packaged, 1, [[RUN dnf -q -y upgrade && dnf -q -y install \
  java-JDKVER-openjdk \
  java-JDKVER-openjdk-devel
m4_ifelse(jdk_at_least(25), 1, [[ENV JAVA[[]]JDKVER[[]]_HOME=/usr/lib/jvm/java-JDKVER-openjdk
]])]], [[# The java-JDKVER-openjdk* packages do not yet exist, so download the JDK.
# RUN curl --silent -o jdk-JDKVER[[]]_linux-x64_bin.tar.gz https://download.oracle.com/java/JDKVER/latest/jdk-JDKVER[[]]_linux-x64_bin.tar.gz \
RUN curl --silent -o jdk-JDKVER[[]]_linux-x64_bin.tar.gz jdk_download_url \
&& tar xzf jdk-JDKVER[[]]_linux-x64_bin.tar.gz \
&& rm jdk-JDKVER[[]]_linux-x64_bin.tar.gz
ENV PATH="/jdk-JDKVER/bin:/root/.local/bin:/root/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
ENV JAVA[[]]JDKVER[[]]_HOME=/jdk-JDKVER
RUN chmod og+rx /root \
&& chmod og+r /root/*
]])m4_dnl
if_plus([[
RUN dnf -q -y upgrade && dnf -q -y install \
  ctags \
  devscripts-checkbashisms \
  gcc-c++ \
  graphviz \
  netpbm \
  netpbm-progs \
  python3 \
  python3-distutils-extra \
  ShellCheck \
  texlive \
  yamllint

## Other ways to install gradle:
##  * The `dnf` package repository has a very old version of gradle.
##  * Need zip to install SDKMAN, need SDKMAN to install gradle.
##    However, SDKMAN requires bash, which would prevent the Docker container
##    from testing that Daikon runs under POSIX sh.
# Install gradle (needed for building daikon-plumelib.jar).
RUN wget -q https://services.gradle.org/distributions/gradle-9.8.0-bin.zip \
&& unzip -q -d /opt/gradle gradle-9.8.0-bin.zip \
&& rm gradle-9.8.0-bin.zip
ENV PATH=$PATH:/opt/gradle/gradle-9.8.0/bin

# Alternately, run: dnf --enablerepo=crb install PACKAGENAME
RUN dnf config-manager --set-enabled crb \
&& dnf -y install \
  dnf-plugins-core \
  texinfo \
  texinfo-tex

# Install shfmt.
RUN dnf -q -y upgrade && dnf -q -y install \
  golang \
&& go install mvdan.cc/sh/v3/cmd/shfmt@latest
ENV PATH=/root/go/bin:$PATH

# Install Python dependencies (obsolete, to be removed).
# `pipx ensurepath` only adds to the path in newly-started shells.
# BUT, setting the path for the current user is not enough.
# Azure creates a new user and runs jobs as it.
# So, install into /usr/local/bin which is already on every user's path.
RUN dnf -q -y install \
  pipx \
&& PIPX_HOME=/opt/pipx PIPX_BIN_DIR=/usr/local/bin pipx install mypy \
&& PIPX_HOME=/opt/pipx PIPX_BIN_DIR=/usr/local/bin pipx install ruff

# Install uv (manages Python dependencies).
RUN echo "installing uv" \
&& wget -qO- https://astral.sh/uv/install.sh | sh \
&& find /root -exec chmod +r {} \; \
&& find /root -type d -exec chmod +x {} \; \
&& find /root/.local/bin -type f -exec chmod +x {} \;
ENV PATH=/root/.local/bin:$PATH
]])m4_dnl

# Clean up.
RUN dnf -q clean all

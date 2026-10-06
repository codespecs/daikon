dockerfile_header(Dockerfile-ubuntu.m4)

# "ubuntu" is the latest LTS release.  "ubuntu:rolling" is the latest release.
# Either might lag behind; as of 2024-11-16, ubuntu:rolling was still 24.04 rather than 24.10.
FROM ubuntu[[]]m4_ifelse(jdk_at_least(25), 1, [[:rolling]])
LABEL org.opencontainers.image.authors="Michael Ernst <mernst@cs.washington.edu>"

# According to
# https://docs.docker.com/engine/userguide/eng-image/dockerfile_best-practices/:
#  * Put "apt-get update" and "apt-get install" in the same RUN command.
#  * Do not run "apt-get upgrade"; instead get upstream to update.

RUN export DEBIAN_FRONTEND=noninteractive \
&& apt-get -qqy update \
&& apt-get -qqy install locales \
&& rm -rf /var/lib/apt/lists/* \
&& locale-gen "en_US.UTF-8"
ENV LANG=en_US.UTF-8 \
    LANGUAGE=en_US:en \
    LC_ALL=en_US.UTF-8

RUN export DEBIAN_FRONTEND=noninteractive \
&& apt-get -qqy update \
&& apt-get -qqy install \
  apt-utils

RUN export DEBIAN_FRONTEND=noninteractive \
&& apt-get -qqy update \
&& apt-get -qqy install \
  autoconf \
  automake \
  bc \
  binutils-dev \
  curl \
  gcc \
  git \
  jq \
  libcrypt-dev \
  lsb-release \
  m4 \
  make \
  rsync \
  unzip \
  wget

# Install the JDK.
m4_ifelse(jdk_packaged_ubuntu, 1, [[RUN export DEBIAN_FRONTEND=noninteractive \
&& apt-get -qqy update \
&& apt-get -qqy install \
  openjdk-JDKVER-jdk \
&& update-java-alternatives --set java-1.JDKVER.0-openjdk-amd64
m4_ifelse(jdk_at_least(25), 1, [[ENV JAVA[[]]JDKVER[[]]_HOME=/usr/lib/jvm/java-JDKVER-openjdk-amd64
]])]], [[# JDK JDKVER is newer than ubuntu[[]]_newest_packaged_jdk in Dockerfile-defs.m4, so
# download the JDK rather than installing openjdk-JDKVER-jdk with apt-get.
RUN curl --silent -o jdk-JDKVER[[]]_linux-x64_bin.tar.gz jdk_download_url \
&& tar xzf jdk-JDKVER[[]]_linux-x64_bin.tar.gz \
&& rm jdk-JDKVER[[]]_linux-x64_bin.tar.gz
ENV PATH="/jdk-JDKVER/bin:$PATH"
ENV JAVA[[]]JDKVER[[]]_HOME=/jdk-JDKVER
]])m4_dnl
if_plus([[
# These are needed to build the Checker Framework, used by the "typecheck" job in CI.
RUN export DEBIAN_FRONTEND=noninteractive \
&& apt-get -qqy update \
&& apt-get -qqy install \
  ant \
  maven \
  python3

RUN export DEBIAN_FRONTEND=noninteractive \
&& apt-get -qqy update \
&& apt-get -qqy install \
  devscripts \
  g++ \
  graphviz \
  libc6-dbg \
  netpbm \
  python3-setuptools \
  shellcheck \
  shfmt \
  texinfo \
  texlive \
  texlive-latex-extra \
  universal-ctags \
  yamllint \
  zlib1g-dev

# Install Python dependencies (obsolete, to be removed).
# `pipx ensurepath` only adds to the path in newly-started shells.
# BUT, setting the path for the current user is not enough.
# Azure creates a new user and runs jobs as it.
# So, install into /usr/local/bin which is already on every user's path.
RUN export DEBIAN_FRONTEND=noninteractive \
&& apt-get -qqy update \
&& apt-get -qqy install \
  pipx \
&& PIPX_HOME=/opt/pipx PIPX_BIN_DIR=/usr/local/bin pipx install mypy \
&& PIPX_HOME=/opt/pipx PIPX_BIN_DIR=/usr/local/bin pipx install ruff

# Install uv (manages Python dependencies).
RUN export DEBIAN_FRONTEND=noninteractive \
&& echo "installing uv" \
&& wget -qO- https://astral.sh/uv/install.sh | sh \
&& find /root -exec chmod +r {} \; \
&& find /root -type d -exec chmod +x {} \; \
&& find /root/.local/bin -type f -exec chmod +x {} \;
ENV PATH=/root/.local/bin:$PATH

## `apt` installs a very old version of gradle (see `apt-cache policy gradle`).
## `snap` does not work under docker.
# Need zip to install SDKMAN, need SDKMAN to install gradle.
# However, SDKMAN requires bash, which would prevent the Docker container from
# testing that Daikon runs under POSIX sh.
RUN export DEBIAN_FRONTEND=noninteractive \
&& wget -q https://services.gradle.org/distributions/gradle-9.8.0-bin.zip \
&& unzip -q -d /opt/gradle gradle-9.8.0-bin.zip \
&& rm gradle-9.8.0-bin.zip
ENV PATH=$PATH:/opt/gradle/gradle-9.8.0/bin
]])m4_dnl

# Clean up.
RUN export DEBIAN_FRONTEND=noninteractive \
&& apt-get -qqy autoremove \
&& apt-get clean \
&& rm -rf /var/lib/apt/lists/*

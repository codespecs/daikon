changequote
changequote(`[',`]')dnl
include([defs.m4])dnl
trigger:
  branches:
    include:
    - '*'
pr:
  branches:
    include:
    - '*'

# variables:
# - name: testingSuffix
#   value: -testing
variables:
- name: testingSuffix
  value:

jobs:

# The dependsOn clauses (in this file and in .circleci/config.yml) are:
#  * Everything not *_ubuntu_jdk17 or typecheck_*_jdk17 depends on jdk17_jobs.
#  * Anything *_jdk8 or *_jdk11 or *_jdk17 or _jdk23 depends on *_jdk21.
#  * Anything *_rockylinux_* depends on *_ubuntu_*.

# The remainder of jobs are run only if the canary_jobs pass.
- job: canary_jobs
  dependsOn:
    - quick_ubuntu_jdk[]canary_version
    - nonquick_ubuntu_jdk[]canary_version
    - nontxt_ubuntu_jdk[]canary_version
    - misc_ubuntu_jdk[]canary_version
    - kvasir_ubuntu_jdk[]canary_version
    - typecheck_latest_ubuntu_jdk[]canary_version
    - typecheck_bundled_ubuntu_jdk[]canary_version
  pool:
    vmImage: 'ubuntu-latest'
  steps:
  - checkout: none
  - bash: true

quick_job(ubuntu, 8)
quick_job(ubuntu, 11)
quick_job(ubuntu, 17)
quick_job(ubuntu, 21)
quick_job(ubuntu, 23)

nonquick_job(ubuntu, 8)
nonquick_job(ubuntu, 11)
nonquick_job(ubuntu, 17)
nonquick_job(ubuntu, 21)
nonquick_job(ubuntu, 23)

nontxt_job(ubuntu, 8)
nontxt_job(ubuntu, 11)
nontxt_job(ubuntu, 17)
nontxt_job(ubuntu, 21)
nontxt_job(ubuntu, 23)

misc_job(ubuntu, 8)
misc_job(ubuntu, 11)
misc_job(ubuntu, 17)
misc_job(ubuntu, 21)
misc_job(ubuntu, 23)

kvasir_job(ubuntu, 8)
kvasir_job(ubuntu, 11)
kvasir_job(ubuntu, 17)
kvasir_job(ubuntu, 21)
kvasir_job(ubuntu, 23)

## The Checker Framework cannot be built, or run, under Java 8.
## Thus, there is no typecheck_*_ubuntu_jdk8 job.
typecheck_latest_job(ubuntu, 11)
typecheck_bundled_job(ubuntu, 11)
typecheck_latest_job(ubuntu, 17)
typecheck_bundled_job(ubuntu, 17)
typecheck_latest_job(ubuntu, 21)
typecheck_bundled_job(ubuntu, 21)
typecheck_latest_job(ubuntu, 23)
typecheck_bundled_job(ubuntu, 23)

quick_job(rockylinux, 8)
quick_job(rockylinux, 11)
quick_job(rockylinux, 17)
quick_job(rockylinux, 21)

nonquick_job(rockylinux, 8)
nonquick_job(rockylinux, 11)
nonquick_job(rockylinux, 17)
nonquick_job(rockylinux, 21)

nontxt_job(rockylinux, 8)
nontxt_job(rockylinux, 11)
nontxt_job(rockylinux, 17)
nontxt_job(rockylinux, 21)

misc_job(rockylinux, 8)
misc_job(rockylinux, 11)
misc_job(rockylinux, 17)
misc_job(rockylinux, 21)

kvasir_job(rockylinux, 8)
kvasir_job(rockylinux, 11)
kvasir_job(rockylinux, 17)
kvasir_job(rockylinux, 21)

## Enable this job to prioritize typechecking a single file, without waiting for other tests.
# - job: typecheck_onefile_jdk21
#   pool:
#     vmImage: 'ubuntu-latest'
#   container: mdernst/daikon-ubuntu-jdk21-plus${{ variables.testingSuffix }}:latest
#   timeoutInMinutes: 70
#   steps:
#   - checkout: self
#     fetchDepth: 1
#   - bash: |
#       java -version
#       javac -version
#     displayName: show Java version
#   - bash: ./scripts/test-typecheck-onefile.sh
#     displayName: test-typecheck-onefile.sh

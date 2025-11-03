changequote
changequote(`[',`]')dnl
changecom([], [disable comments, that is, expand within them])dnl
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
  #  * Everything not *_ubuntu_jdk[]canary_version or typecheck_*_jdk[]canary_version depends on canary_jobs.
  #  * Anything *_jdk8 or *_jdk11 or *_jdk17 or *_jdk21 depends on *_jdk25.
  #  * Anything *_rockylinux_* depends on *_ubuntu_*.
  # The remainder of jobs are run only if the canary_jobs pass.
  - job: canary_jobs
    dependsOn:
      - quick_[]canary_os[]_jdk[]canary_version
      - nonquick_[]canary_os[]_jdk[]canary_version
      - nontxt_[]canary_os[]_jdk[]canary_version
      - misc_[]canary_os[]_jdk[]canary_version
      - kvasir_[]canary_os[]_jdk[]canary_version
      - typecheck_latest_[]canary_os[]_jdk[]canary_version
      - typecheck_bundled_[]canary_os[]_jdk[]canary_version
    pool:
      vmImage: 'ubuntu-latest'
    steps:
      - checkout: none
      - bash: true

include([jobs.m4])dnl

#   ## Enable this job to prioritize typechecking a single file, without waiting for other tests.
#   - job: typecheck_onefile_jdk21
#     pool:
#       vmImage: 'ubuntu-latest'
#     container: mdernst/daikon-ubuntu-jdk21-plus${{ variables.testingSuffix }}:latest
#     timeoutInMinutes: 70
#     steps:
#       - checkout: self
#         fetchDepth: 1
#       - bash: |
#           java -version
#           javac -version
#         displayName: show Java version
#       - bash: ./scripts/test-typecheck-onefile.sh
#         displayName: test-typecheck-onefile.sh

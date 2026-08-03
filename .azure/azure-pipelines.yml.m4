changequote
changequote(`[',`]')dnl
changecom([], [disable comments, that is, expand within them])dnl
include([defs-common.m4])dnl
include([defs.m4])dnl
trigger:
  batch: true
  branches:
    include:
      - '*'
pr:
  branches:
    include:
      - '*'

jobs:

  # The dependsOn clauses (in this file and in .circleci/config.yml) are:
  #  * Everything not *_ubuntu_jdk[]canary_jdk or typecheck_*_jdk[]canary_jdk depends on canary_jobs.
  #  * Anything *_jdk8 or *_jdk11 or *_jdk17 or *_jdk21 or *_jdk26 depends on *_jdk25.
  #  * Anything *_rockylinux_* depends on *_ubuntu_*.
  # The remainder of jobs are run only if the canary_jobs pass.
  - job: canary_jobs
    dependsOn:
      - quick_[]canary_os[]_jdk[]canary_jdk
      - nonquick_[]canary_os[]_jdk[]canary_jdk
      - nontxt_[]canary_os[]_jdk[]canary_jdk
      - misc_[]canary_os[]_jdk[]canary_jdk
      - kvasir_[]canary_os[]_jdk[]canary_jdk
      - typecheck_latest_part1_[]canary_os[]_jdk[]canary_jdk
      - typecheck_latest_part2_[]canary_os[]_jdk[]canary_jdk
      - typecheck_latest_part3_[]canary_os[]_jdk[]canary_jdk
      - typecheck_bundled_part1_[]canary_os[]_jdk[]canary_jdk
      - typecheck_bundled_part2_[]canary_os[]_jdk[]canary_jdk
      - typecheck_bundled_part3_[]canary_os[]_jdk[]canary_jdk
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
#     container: mdernst/daikon-ubuntu-jdk21-plus:latest
#     timeoutInMinutes: 70
#     steps:
#       - checkout: self
#         fetchDepth: 1
#       - bash: |
#           java -version
#           javac -version
#         displayName: show Java version
#       - bash: scripts/test-typecheck-onefile.sh
#         displayName: test-typecheck-onefile.sh

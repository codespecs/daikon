# DO NOT EDIT azure-pipelines.yml.  Edit azure-pipelines.yml.m4 and defs.m4 instead.
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

  # The dependsOn clauses are:
  #  * Everything depends on the canary jobs (the main jdk25 jobs), except those jobs themselves.
  #  * Any *_jdkNN job (NN != 25) depends on the corresponding *_jdk25 job.
  #  * Anything *_rockylinux_* depends on *_ubuntu_*.
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
        displayName: canary_jobs

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

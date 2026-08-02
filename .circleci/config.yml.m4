changequote
changequote(`[',`]')dnl
changecom([], [disable comments, that is, expand within them])dnl
include([../.azure/defs-common.m4])dnl
include([defs.m4])dnl
version: 2.1

jobs:

  # Only proceed to other jobs if canary_jobs passes.
  canary_jobs:
    docker:
      - image: 'cimg/base:2025.09'
    resource_class: small
    environment:
      TERM: dumb
    steps:
      - run: /bin/true

include([../.azure/jobs.m4])dnl

# For an explanation of the dependence logic, see ../.azure/azure-pipelines.yml .

workflows:
  build:
    jobs:
      - canary_jobs:
          requires:
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
job_dependences(ubuntu, 11, quick)
job_dependences(ubuntu, 17, quick)
job_dependences(ubuntu, 21, quick)
job_dependences(ubuntu, 25, quick)
job_dependences(ubuntu, 26, quick)
job_dependences(ubuntu, 11, nonquick)
job_dependences(ubuntu, 17, nonquick)
job_dependences(ubuntu, 21, nonquick)
job_dependences(ubuntu, 25, nonquick)
job_dependences(ubuntu, 26, nonquick)
job_dependences(ubuntu, 11, nontxt)
job_dependences(ubuntu, 17, nontxt)
job_dependences(ubuntu, 21, nontxt)
job_dependences(ubuntu, 25, nontxt)
job_dependences(ubuntu, 26, nontxt)
job_dependences(ubuntu, 11, misc)
job_dependences(ubuntu, 17, misc)
job_dependences(ubuntu, 21, misc)
job_dependences(ubuntu, 25, misc)
job_dependences(ubuntu, 26, misc)
job_dependences(ubuntu, 11, kvasir)
job_dependences(ubuntu, 25, kvasir)
job_dependences(ubuntu, 26, kvasir)
      ## The Checker Framework cannot be built under Java 8, 11 or 17.
      ## Thus, there is no typecheck_latest_ubuntu_jdk{8,11,17} job.
      ## The Checker Framework cannot be run under Java 8 or 11.
      ## Thus, there is no typecheck_bundled_ubuntu_jdk{8,11} job.
job_dependences_part(ubuntu, 17, typecheck_bundled, part1)
job_dependences_part(ubuntu, 17, typecheck_bundled, part2)
job_dependences_part(ubuntu, 17, typecheck_bundled, part3)
job_dependences_part(ubuntu, 21, typecheck_latest, part1)
job_dependences_part(ubuntu, 21, typecheck_latest, part2)
job_dependences_part(ubuntu, 21, typecheck_latest, part3)
job_dependences_part(ubuntu, 21, typecheck_bundled, part1)
job_dependences_part(ubuntu, 21, typecheck_bundled, part2)
job_dependences_part(ubuntu, 21, typecheck_bundled, part3)
job_dependences_part(ubuntu, 25, typecheck_latest, part1)
job_dependences_part(ubuntu, 25, typecheck_latest, part2)
job_dependences_part(ubuntu, 25, typecheck_latest, part3)
job_dependences_part(ubuntu, 25, typecheck_bundled, part1)
job_dependences_part(ubuntu, 25, typecheck_bundled, part2)
job_dependences_part(ubuntu, 25, typecheck_bundled, part3)
job_dependences_part(ubuntu, 26, typecheck_latest, part1)
job_dependences_part(ubuntu, 26, typecheck_latest, part2)
job_dependences_part(ubuntu, 26, typecheck_latest, part3)
job_dependences_part(ubuntu, 26, typecheck_bundled, part1)
job_dependences_part(ubuntu, 26, typecheck_bundled, part2)
job_dependences_part(ubuntu, 26, typecheck_bundled, part3)
job_dependences(rockylinux, 11, quick)
job_dependences(rockylinux, 25, quick)
job_dependences(rockylinux, 26, quick)
job_dependences(rockylinux, 11, nonquick)
job_dependences(rockylinux, 25, nonquick)
job_dependences(rockylinux, 26, nonquick)
job_dependences(rockylinux, 11, nontxt)
job_dependences(rockylinux, 25, nontxt)
job_dependences(rockylinux, 26, nontxt)
job_dependences(rockylinux, 11, misc)
job_dependences(rockylinux, 25, misc)
job_dependences(rockylinux, 26, misc)
job_dependences(rockylinux, 11, kvasir)
job_dependences(rockylinux, 25, kvasir)
job_dependences(rockylinux, 26, kvasir)

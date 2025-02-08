changequote
changequote(`[',`]')dnl
include([defs.m4])dnl
version: 2.1

jobs:

  # Only proceed to other jobs if canary-jobs passes.
  canary-jobs:
    docker:
      - image: cimg/base:2022.06
    environment:
      CIRCLE_COMPARE_URL: << pipeline.project.git_url >>/compare/<< pipeline.git.base_revision >>..<<pipeline.git.revision>>
    steps:
      - run: /bin/true

quick_job(ubuntu, 8)
quick_job(ubuntu, 11)
quick_job(ubuntu, 17)
quick_job(ubuntu, 21)
quick_job(ubuntu, 24)

nonquick_job(ubuntu, 8)
nonquick_job(ubuntu, 11)
nonquick_job(ubuntu, 17)
nonquick_job(ubuntu, 21)
nonquick_job(ubuntu, 24)

nontxt_job(ubuntu, 8)
nontxt_job(ubuntu, 11)
nontxt_job(ubuntu, 17)
nontxt_job(ubuntu, 21)
nontxt_job(ubuntu, 24)

misc_job(ubuntu, 8)
misc_job(ubuntu, 11)
misc_job(ubuntu, 17)
misc_job(ubuntu, 21)
misc_job(ubuntu, 24)

kvasir_job(ubuntu, 8)
kvasir_job(ubuntu, 11)
kvasir_job(ubuntu, 17)
kvasir_job(ubuntu, 21)
kvasir_job(ubuntu, 24)

## The Checker Framework cannot be built, or run, under Java 8.
## Thus, there is no typecheck_*_ubuntu_jdk8 job.
typecheck_latest_job(ubuntu, 11)
typecheck_bundled_job(ubuntu, 11)
typecheck_latest_job(ubuntu, 17)
typecheck_bundled_job(ubuntu, 17)
typecheck_latest_job(ubuntu, 21)
typecheck_bundled_job(ubuntu, 21)
## The Checker Framework does not yet build under Java 24.
# typecheck_latest_job(ubuntu, 24)
typecheck_bundled_job(ubuntu, 24)

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


# For an explanation of the dependence logic, see ../.azure/azure-pipelines.yml .

# TODO: Automate this part of the file with m4, too.

workflows:
  version: 2
  build:
    jobs:
      - canary-jobs:
          requires:
            - quick-txt-diff-ubuntu-jdk21
            - nonquick-txt-diff-ubuntu-jdk21
            - non-txt-diff-ubuntu-jdk21
            - misc-ubuntu-jdk21
            - kvasir-ubuntu-jdk21
            - typecheck-bundled-ubuntu-jdk21
      - quick-txt-diff-ubuntu-jdk8:
          requires:
            - canary-jobs
            - quick-txt-diff-ubuntu-jdk21
      - quick-txt-diff-ubuntu-jdk11:
          requires:
            - canary-jobs
            - quick-txt-diff-ubuntu-jdk21
      - quick-txt-diff-ubuntu-jdk17:
          requires:
            - canary-jobs
            - quick-txt-diff-ubuntu-jdk21
      - quick-txt-diff-ubuntu-jdk21
      - nonquick-txt-diff-ubuntu-jdk8:
          requires:
            - canary-jobs
            - nonquick-txt-diff-ubuntu-jdk21
      - nonquick-txt-diff-ubuntu-jdk11:
          requires:
            - canary-jobs
            - nonquick-txt-diff-ubuntu-jdk21
      - nonquick-txt-diff-ubuntu-jdk17:
          requires:
            - canary-jobs
            - nonquick-txt-diff-ubuntu-jdk21
      - nonquick-txt-diff-ubuntu-jdk21
      - non-txt-diff-ubuntu-jdk8:
          requires:
            - canary-jobs
            - non-txt-diff-ubuntu-jdk21
      - non-txt-diff-ubuntu-jdk11:
          requires:
            - canary-jobs
            - non-txt-diff-ubuntu-jdk21
      - non-txt-diff-ubuntu-jdk17:
          requires:
            - canary-jobs
            - non-txt-diff-ubuntu-jdk21
      - non-txt-diff-ubuntu-jdk21
      - misc-ubuntu-jdk8:
          requires:
            - canary-jobs
            - misc-ubuntu-jdk21
      - misc-ubuntu-jdk11:
          requires:
            - canary-jobs
            - misc-ubuntu-jdk21
      - misc-ubuntu-jdk17:
          requires:
            - canary-jobs
            - misc-ubuntu-jdk21
      - misc-ubuntu-jdk21
      - kvasir-ubuntu-jdk8:
          requires:
            - canary-jobs
            - kvasir-ubuntu-jdk21
      - kvasir-ubuntu-jdk11:
          requires:
            - canary-jobs
            - kvasir-ubuntu-jdk21
      - kvasir-ubuntu-jdk17:
          requires:
            - canary-jobs
            - kvasir-ubuntu-jdk21
      - kvasir-ubuntu-jdk21
      - kvasir-rockylinux-jdk8:
          requires:
            - canary-jobs
            - kvasir-rockylinux-jdk21
            - kvasir-ubuntu-jdk8
      - kvasir-rockylinux-jdk11:
          requires:
            - canary-jobs
            - kvasir-rockylinux-jdk21
            - kvasir-ubuntu-jdk11
      - kvasir-rockylinux-jdk17:
          requires:
            - canary-jobs
            - kvasir-rockylinux-jdk21
            - kvasir-ubuntu-jdk17
      - kvasir-rockylinux-jdk21:
          requires:
            - canary-jobs
            - kvasir-ubuntu-jdk21
      - quick-txt-diff-rockylinux-jdk8:
          requires:
            - canary-jobs
            - quick-txt-diff-rockylinux-jdk21
            - quick-txt-diff-ubuntu-jdk8
      - quick-txt-diff-rockylinux-jdk11:
          requires:
            - canary-jobs
            - quick-txt-diff-rockylinux-jdk21
            - quick-txt-diff-ubuntu-jdk11
      - quick-txt-diff-rockylinux-jdk17:
          requires:
            - canary-jobs
            - quick-txt-diff-rockylinux-jdk21
            - quick-txt-diff-ubuntu-jdk17
      - quick-txt-diff-rockylinux-jdk21:
          requires:
            - canary-jobs
            - quick-txt-diff-ubuntu-jdk21
      - nonquick-txt-diff-rockylinux-jdk8:
          requires:
            - canary-jobs
            - nonquick-txt-diff-rockylinux-jdk21
            - nonquick-txt-diff-ubuntu-jdk8
      - nonquick-txt-diff-rockylinux-jdk11:
          requires:
            - canary-jobs
            - nonquick-txt-diff-rockylinux-jdk21
            - nonquick-txt-diff-ubuntu-jdk11
      - nonquick-txt-diff-rockylinux-jdk17:
          requires:
            - canary-jobs
            - nonquick-txt-diff-rockylinux-jdk21
            - nonquick-txt-diff-ubuntu-jdk17
      - nonquick-txt-diff-rockylinux-jdk21:
          requires:
            - canary-jobs
            - nonquick-txt-diff-ubuntu-jdk21
      - non-txt-diff-rockylinux-jdk8:
          requires:
            - canary-jobs
            - non-txt-diff-rockylinux-jdk21
            - non-txt-diff-ubuntu-jdk8
      - non-txt-diff-rockylinux-jdk11:
          requires:
            - canary-jobs
            - non-txt-diff-rockylinux-jdk21
            - non-txt-diff-ubuntu-jdk11
      - non-txt-diff-rockylinux-jdk17:
          requires:
            - canary-jobs
            - non-txt-diff-rockylinux-jdk21
            - non-txt-diff-ubuntu-jdk17
      - non-txt-diff-rockylinux-jdk21:
          requires:
            - canary-jobs
            - non-txt-diff-ubuntu-jdk21
      - misc-rockylinux-jdk8:
          requires:
            - canary-jobs
            - misc-rockylinux-jdk21
            - misc-ubuntu-jdk8
      - misc-rockylinux-jdk11:
          requires:
            - canary-jobs
            - misc-rockylinux-jdk21
            - misc-ubuntu-jdk11
      - misc-rockylinux-jdk17:
          requires:
            - canary-jobs
            - misc-rockylinux-jdk21
            - misc-ubuntu-jdk17
      - misc-rockylinux-jdk21:
          requires:
            - canary-jobs
            - misc-ubuntu-jdk21
      - typecheck-bundled-ubuntu-jdk21
# Seems to run out of memory on CircleCI.  It's being checked on Azure Pipelines.
#      -  typecheck-latest-ubuntu-jdk8:
#      -  typecheck-latest-ubuntu-jdk11:
#      -  typecheck-latest-ubuntu-jdk17:

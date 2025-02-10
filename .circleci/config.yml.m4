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

include([../.azure/jobs.m4])dnl


# For an explanation of the dependence logic, see ../.azure/azure-pipelines.yml .

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
job_dependences(ubuntu, 8, quick-txt-diff)
job_dependences(ubuntu, 11, quick-txt-diff)
job_dependences(ubuntu, 17, quick-txt-diff)
job_dependences(ubuntu, 21, quick-txt-diff)
job_dependences(ubuntu, 24, quick-txt-diff)
job_dependences(ubuntu, 8, nonquick-txt-diff)
job_dependences(ubuntu, 11, nonquick-txt-diff)
job_dependences(ubuntu, 17, nonquick-txt-diff)
job_dependences(ubuntu, 21, nonquick-txt-diff)
# job_dependences(ubuntu, 24, nonquick-txt-diff)
job_dependences(ubuntu, 8, non-txt-diff)
job_dependences(ubuntu, 11, non-txt-diff)
job_dependences(ubuntu, 17, non-txt-diff)
job_dependences(ubuntu, 21, non-txt-diff)
job_dependences(ubuntu, 24, non-txt-diff)
job_dependences(ubuntu, 8, misc)
job_dependences(ubuntu, 11, misc)
job_dependences(ubuntu, 17, misc)
job_dependences(ubuntu, 21, misc)
job_dependences(ubuntu, 24, misc)
job_dependences(ubuntu, 8, kvasir)
job_dependences(ubuntu, 11, kvasir)
job_dependences(ubuntu, 17, kvasir)
job_dependences(ubuntu, 21, kvasir)
job_dependences(ubuntu, 24, kvasir)
## The Checker Framework cannot be built, or run, under Java 8.
## Thus, there is no typecheck_*_ubuntu_jdk8 job.
job_dependences(ubuntu, 11, typecheck-latest)
job_dependences(ubuntu, 11, typecheck-bundled)
job_dependences(ubuntu, 17, typecheck-latest)
job_dependences(ubuntu, 17, typecheck-bundled)
job_dependences(ubuntu, 21, typecheck-latest)
job_dependences(ubuntu, 21, typecheck-bundled)
## The Checker Framework does not yet build under Java 24.
# job_dependences(ubuntu, 24, typecheck-latest)
job_dependences(ubuntu, 24, typecheck-bundled)
job_dependences(rockylinux, 8, quick-txt-diff)
job_dependences(rockylinux, 11, quick-txt-diff)
job_dependences(rockylinux, 17, quick-txt-diff)
job_dependences(rockylinux, 21, quick-txt-diff)
job_dependences(rockylinux, 24, quick-txt-diff)
job_dependences(rockylinux, 8, nonquick-txt-diff)
job_dependences(rockylinux, 11, nonquick-txt-diff)
job_dependences(rockylinux, 17, nonquick-txt-diff)
job_dependences(rockylinux, 21, nonquick-txt-diff)
# job_dependences(rockylinux, 24, nonquick-txt-diff)
job_dependences(rockylinux, 8, non-txt-diff)
job_dependences(rockylinux, 11, non-txt-diff)
job_dependences(rockylinux, 17, non-txt-diff)
job_dependences(rockylinux, 21, non-txt-diff)
job_dependences(rockylinux, 24, non-txt-diff)
job_dependences(rockylinux, 8, misc)
job_dependences(rockylinux, 11, misc)
job_dependences(rockylinux, 17, misc)
job_dependences(rockylinux, 21, misc)
job_dependences(rockylinux, 24, misc)
job_dependences(rockylinux, 8, kvasir)
job_dependences(rockylinux, 11, kvasir)
job_dependences(rockylinux, 17, kvasir)
job_dependences(rockylinux, 21, kvasir)
job_dependences(rockylinux, 24, kvasir)

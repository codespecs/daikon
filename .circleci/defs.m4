changequote
changequote(`[',`]')dnl
changecom([], [disable comments, that is, expand within them])dnl
ifelse([the built-in "dnl" macro means "discard to next line",])dnl
define([canary_os], [ubuntu])dnl
define([canary_version], [25])dnl
define([canary_test], [canary_os[]canary_version])dnl
dnl
ifelse([If the first argument is "full", do a full checkout.])dnl
define([circleci_boilerplate], [dnl
    resource_class: large
    environment:
      CIRCLE_COMPARE_URL: << pipeline.project.git_url >>/compare/<< pipeline.git.base_revision >>..<<pipeline.git.revision>>
      TERM: dumb
    steps:
      - restore_cache:
          keys:
            - &source$1-cache source$1-v1$1-{{ .Branch }}-{{ .Revision }}
            - source$1-v1$1-{{ .Branch }}-
            - source$1-v1$1-
      - checkout[]ifelse($1,full,[:
          method: full])
      - save_cache:
          key: *source$1-cache
          paths:
            - ".git"])dnl
dnl
ifelse([Each macro takes two arguments, the OS name and the JDK version.])dnl
dnl
define([quick_job], [dnl
  quick-txt-diff-$1-jdk$2:
    docker:
      - image: mdernst/daikon-$1-jdk$2
circleci_boilerplate
      - run: ./scripts/test-quick-txt-diff.sh
])dnl
dnl
define([nonquick_job], [dnl
  nonquick-txt-diff-$1-jdk$2:
    docker:
      - image: mdernst/daikon-$1-jdk$2
circleci_boilerplate
      - run: ./scripts/test-nonquick-txt-diff.sh
])dnl
dnl
define([nontxt_job], [dnl
  non-txt-diff-$1-jdk$2:
    docker:
      - image: mdernst/daikon-$1-jdk$2
circleci_boilerplate
      - run: ./scripts/test-non-txt-diff.sh])dnl
dnl
define([misc_job], [dnl
  misc-$1-jdk$2:
    docker:
      - image: mdernst/daikon-$1-jdk$2-plus
circleci_boilerplate(full)
      - run:
          command: ./scripts/test-misc.sh
          no_output_timeout: 20m])dnl
dnl
define([kvasir_job], [dnl
  kvasir-$1-jdk$2:
    docker:
      - image: mdernst/daikon-$1-jdk$2-plus
circleci_boilerplate
      - run:
          name: Test Kvasir
          command: ./scripts/test-kvasir.sh
          no_output_timeout: 20m])dnl
dnl
define([typecheck_latest_job], [dnl
  typecheck-latest-$1-jdk$2:
    docker:
      - image: mdernst/daikon-$1-jdk$2
circleci_boilerplate
      - run: env
      - run:
          command: scripts/test-typecheck-with-latest-cf.sh
          no_output_timeout: 30m])dnl
define([typecheck_bundled_job], [dnl
  typecheck-bundled-$1-jdk$2:
    docker:
      - image: mdernst/daikon-$1-jdk$2
circleci_boilerplate
      - run: |
          make showvars
          make compile daikon.jar
      - run:
          command: scripts/test-typecheck-with-bundled-cf.sh
          no_output_timeout: 30m
])dnl
dnl
define([job_dependences], [dnl
      - $3-$1-jdk$2[]dnl
ifelse($1$2,canary_test,,[:
          requires:
            - canary-jobs
ifelse($2,canary_version,,[dnl
            - $3-$1-jdk[]canary_version
])dnl
ifelse($1,canary_os,,[dnl
            - $3-canary_os[]-jdk$2
])dnl
])dnl
])dnl
dnl
ifelse([
Local Variables:
eval: (add-hook 'after-save-hook '(lambda () (run-command nil "make")) nil 'local)
end:
])dnl

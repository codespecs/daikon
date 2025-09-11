changequote
changequote(`[',`]')dnl
ifelse([the built-in "dnl" macro means "discard to next line",])dnl
define([canary_os], [ubuntu])dnl
define([canary_version], [25])dnl
define([canary_test], [canary_os[]canary_version])dnl
ifelse([each macro takes two arguments, the OS name and the JDK version])dnl
dnl
define([quick_job], [dnl
  quick-txt-diff-$1-jdk$2:
    docker:
      - image: mdernst/daikon-$1-jdk$2
    resource_class: large
    environment:
      CIRCLE_COMPARE_URL: << pipeline.project.git_url >>/compare/<< pipeline.git.base_revision >>..<<pipeline.git.revision>>
    steps:
      - restore_cache:
          keys:
            - source-v1-{{ .Branch }}-{{ .Revision }}
            - source-v1-{{ .Branch }}-
            - source-v1-
      - checkout
      - save_cache:
          key: source-v1-{{ .Branch }}-{{ .Revision }}
          paths:
            - ".git"
      - run: ./scripts/test-quick-txt-diff.sh
])dnl
dnl
define([nonquick_job], [dnl
  nonquick-txt-diff-$1-jdk$2:
    docker:
      - image: mdernst/daikon-$1-jdk$2
    resource_class: large
    environment:
      CIRCLE_COMPARE_URL: << pipeline.project.git_url >>/compare/<< pipeline.git.base_revision >>..<<pipeline.git.revision>>
    steps:
      - restore_cache:
          keys:
            - source-v1-{{ .Branch }}-{{ .Revision }}
            - source-v1-{{ .Branch }}-
            - source-v1-
      - checkout
      - save_cache:
          key: source-v1-{{ .Branch }}-{{ .Revision }}
          paths:
            - ".git"
      - run: ./scripts/test-nonquick-txt-diff.sh
])dnl
dnl
define([nontxt_job], [dnl
  non-txt-diff-$1-jdk$2:
    docker:
      - image: mdernst/daikon-$1-jdk$2
    resource_class: large
    environment:
      CIRCLE_COMPARE_URL: << pipeline.project.git_url >>/compare/<< pipeline.git.base_revision >>..<<pipeline.git.revision>>
    steps:
      - restore_cache:
          keys:
            - source-v1-{{ .Branch }}-{{ .Revision }}
            - source-v1-{{ .Branch }}-
            - source-v1-
      - checkout
      - save_cache:
          key: source-v1-{{ .Branch }}-{{ .Revision }}
          paths:
            - ".git"
      - run: ./scripts/test-non-txt-diff.sh])dnl
dnl
define([misc_job], [dnl
  misc-$1-jdk$2:
    docker:
      - image: mdernst/daikon-$1-jdk$2-plus
    resource_class: large
    environment:
      CIRCLE_COMPARE_URL: << pipeline.project.git_url >>/compare/<< pipeline.git.base_revision >>..<<pipeline.git.revision>>
    steps:
      - restore_cache:
          keys:
            - source-v1-{{ .Branch }}-{{ .Revision }}
            - source-v1-{{ .Branch }}-
            - source-v1-
      - checkout
      - save_cache:
          key: source-v1-{{ .Branch }}-{{ .Revision }}
          paths:
            - ".git"
      - run:
          command: ./scripts/test-misc.sh
          no_output_timeout: 20m])dnl
dnl
define([kvasir_job], [dnl
  kvasir-$1-jdk$2:
    docker:
      - image: mdernst/daikon-$1-jdk$2-plus
    resource_class: large
    environment:
      CIRCLE_COMPARE_URL: << pipeline.project.git_url >>/compare/<< pipeline.git.base_revision >>..<<pipeline.git.revision>>
    steps:
      - restore_cache:
          keys:
            - source-v1-{{ .Branch }}-{{ .Revision }}
            - source-v1-{{ .Branch }}-
            - source-v1-
      - checkout
      - save_cache:
          key: source-v1-{{ .Branch }}-{{ .Revision }}
          paths:
            - ".git"
      - run:
          name: Test Kvasir
          command: ./scripts/test-kvasir.sh
          no_output_timeout: 20m])dnl
dnl
define([typecheck_latest_job], [dnl
  typecheck-latest-$1-jdk$2:
    docker:
      - image: mdernst/daikon-$1-jdk$2
    resource_class: large
    environment:
      CIRCLE_COMPARE_URL: << pipeline.project.git_url >>/compare/<< pipeline.git.base_revision >>..<<pipeline.git.revision>>
    steps:
      - restore_cache:
          keys:
            - source-v1-{{ .Branch }}-{{ .Revision }}
            - source-v1-{{ .Branch }}-
            - source-v1-
      - checkout
      - save_cache:
          key: source-v1-{{ .Branch }}-{{ .Revision }}
          paths:
            - ".git"
      - run: env
      - run:
          command: scripts/test-typecheck-with-latest-cf.sh
          no_output_timeout: 30m])dnl
define([typecheck_bundled_job], [dnl
  typecheck-bundled-$1-jdk$2:
    docker:
      - image: mdernst/daikon-$1-jdk$2
    resource_class: large
    environment:
      CIRCLE_COMPARE_URL: << pipeline.project.git_url >>/compare/<< pipeline.git.base_revision >>..<<pipeline.git.revision>>
    steps:
      - restore_cache:
          keys:
            - source-v1-{{ .Branch }}-{{ .Revision }}
            - source-v1-{{ .Branch }}-
            - source-v1-
      - checkout
      - save_cache:
          key: source-v1-{{ .Branch }}-{{ .Revision }}
          paths:
            - ".git"
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
ifelse($2,canary_version,,[            - $3-$1-jdk[]canary_version
])dnl
ifelse($1,canary_os,,[            - $3-canary_os[]-jdk$2
])dnl
])dnl
])dnl
dnl
define([nonquick_job_dependences], [dnl
  nonquick-txt-diff-$1-jdk$2:
    docker:
      - image: mdernst/daikon-$1-jdk$2
    resource_class: large
    environment:
      CIRCLE_COMPARE_URL: << pipeline.project.git_url >>/compare/<< pipeline.git.base_revision >>..<<pipeline.git.revision>>
    steps:
      - restore_cache:
          keys:
            - source-v1-{{ .Branch }}-{{ .Revision }}
            - source-v1-{{ .Branch }}-
            - source-v1-
      - checkout
      - save_cache:
          key: source-v1-{{ .Branch }}-{{ .Revision }}
          paths:
            - ".git"
      - run: ./scripts/test-nonquick-txt-diff.sh
])dnl
dnl
define([nontxt_job_dependences], [dnl
  non-txt-diff-$1-jdk$2:
    docker:
      - image: mdernst/daikon-$1-jdk$2
    resource_class: large
    environment:
      CIRCLE_COMPARE_URL: << pipeline.project.git_url >>/compare/<< pipeline.git.base_revision >>..<<pipeline.git.revision>>
    steps:
      - restore_cache:
          keys:
            - source-v1-{{ .Branch }}-{{ .Revision }}
            - source-v1-{{ .Branch }}-
            - source-v1-
      - checkout
      - save_cache:
          key: source-v1-{{ .Branch }}-{{ .Revision }}
          paths:
            - ".git"
      - run: ./scripts/test-non-txt-diff.sh])dnl
dnl
define([misc_job_dependences], [dnl
  misc-$1-jdk$2:
    docker:
      - image: mdernst/daikon-$1-jdk$2-plus
    resource_class: large
    environment:
      CIRCLE_COMPARE_URL: << pipeline.project.git_url >>/compare/<< pipeline.git.base_revision >>..<<pipeline.git.revision>>
    steps:
      - restore_cache:
          keys:
            - source-v1-{{ .Branch }}-{{ .Revision }}
            - source-v1-{{ .Branch }}-
            - source-v1-
      - checkout
      - save_cache:
          key: source-v1-{{ .Branch }}-{{ .Revision }}
          paths:
            - ".git"
      - run:
          command: ./scripts/test-misc.sh
          no_output_timeout: 20m])dnl
dnl
define([kvasir_job_dependences], [dnl
  kvasir-$1-jdk$2:
    docker:
      - image: mdernst/daikon-$1-jdk$2-plus
    resource_class: large
    environment:
      CIRCLE_COMPARE_URL: << pipeline.project.git_url >>/compare/<< pipeline.git.base_revision >>..<<pipeline.git.revision>>
    steps:
      - restore_cache:
          keys:
            - source-v1-{{ .Branch }}-{{ .Revision }}
            - source-v1-{{ .Branch }}-
            - source-v1-
      - checkout
      - save_cache:
          key: source-v1-{{ .Branch }}-{{ .Revision }}
          paths:
            - ".git"
      - run:
          name: Test Kvasir
          command: ./scripts/test-kvasir.sh
          no_output_timeout: 20m])dnl
dnl
define([typecheck_latest_job_dependences], [dnl
  typecheck-latest-$1-jdk$2:
    docker:
      - image: mdernst/daikon-$1-jdk$2
    resource_class: large
    environment:
      CIRCLE_COMPARE_URL: << pipeline.project.git_url >>/compare/<< pipeline.git.base_revision >>..<<pipeline.git.revision>>
    steps:
      - restore_cache:
          keys:
            - source-v1-{{ .Branch }}-{{ .Revision }}
            - source-v1-{{ .Branch }}-
            - source-v1-
      - checkout
      - save_cache:
          key: source-v1-{{ .Branch }}-{{ .Revision }}
          paths:
            - ".git"
      - run: env
      - run:
          command: scripts/test-typecheck-with-latest-cf.sh
          no_output_timeout: 30m])dnl
define([typecheck_bundled_job_dependences], [dnl
  typecheck-bundled-$1-jdk$2:
    docker:
      - image: mdernst/daikon-$1-jdk$2
    resource_class: large
    environment:
      CIRCLE_COMPARE_URL: << pipeline.project.git_url >>/compare/<< pipeline.git.base_revision >>..<<pipeline.git.revision>>
    steps:
      - restore_cache:
          keys:
            - source-v1-{{ .Branch }}-{{ .Revision }}
            - source-v1-{{ .Branch }}-
            - source-v1-
      - checkout
      - save_cache:
          key: source-v1-{{ .Branch }}-{{ .Revision }}
          paths:
            - ".git"
      - run: |
          make showvars
          make compile daikon.jar
      - run:
          command: scripts/test-typecheck-with-bundled-cf.sh
          no_output_timeout: 30m
])dnl
ifelse([
Local Variables:
eval: (make-local-variable 'after-save-hook)
eval: (add-hook 'after-save-hook '(lambda () (compile "make")))
end:
])dnl

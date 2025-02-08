changequote
changequote(`[',`]')dnl
ifelse([the built-in "dnl" macro means "discard to next line",])dnl
define([canary_os], [ubuntu])dnl
define([canary_version], [21])dnl
define([latest_version], [24])dnl
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
          command: scripts/test-typecheck-latest-cf.sh
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
          command: scripts/test-typecheck-bundled-cf.sh
          no_output_timeout: 30m
])dnl
ifelse([
Local Variables:
eval: (make-local-variable 'after-save-hook)
eval: (add-hook 'after-save-hook '(lambda () (compile "make")))
end:
])dnl

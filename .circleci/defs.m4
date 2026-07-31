changequote
changequote(`[',`]')dnl
changecom([], [disable comments, that is, expand within them])dnl
ifelse([the built-in "dnl" macro means "discard to next line",])dnl
define([canary_os], [ubuntu])dnl
define([canary_version], [25])dnl
define([canary_test], [canary_os[]canary_version])dnl
define([docker_userid], [mdernst])
dnl
ifelse([Three arguments: Docker image, name, command line.])dnl
define([boilerplate], [dnl
    docker:
      - image: $1
    resource_class: large
    environment:
      TERM: dumb
    steps:
      - restore_cache:
          keys:
            - &source-cache source-v1-{{ .Branch }}-{{ .Revision }}
            - source-v1-{{ .Branch }}-
            - source-v1-
      - checkout[]ifelse($2,test-misc,[:
          method: full])
      - save_cache:
          key: *source-cache
          paths:
            - ".git"
      - run:
          name: $2
          command: $3[]ifelse($2,test-misc,[
          no_output_timeout: 20m],$2,Test Kvasir,[
          no_output_timeout: 20m],$2,typecheck bundled,[
          no_output_timeout: 30m],$2,typecheck latest,[
          no_output_timeout: 30m],[])
])dnl
dnl
ifelse([Each macro takes two arguments, the OS name and the JDK version.])dnl
dnl
define([quick_job], [dnl
  quick-txt-diff-$1-jdk$2:
boilerplate(docker_userid/daikon-$1-jdk$2<< pipeline.parameters.testing-suffix >>, test-quick, ./scripts/test-quick-txt-diff.sh)
])dnl
dnl
define([nonquick_job], [dnl
  nonquick-txt-diff-$1-jdk$2:
boilerplate(docker_userid/daikon-$1-jdk$2<< pipeline.parameters.testing-suffix >>, test-nonquick, ./scripts/test-nonquick-txt-diff.sh)
])dnl
dnl
define([nontxt_job], [dnl
  non-txt-diff-$1-jdk$2:
boilerplate(docker_userid/daikon-$1-jdk$2<< pipeline.parameters.testing-suffix >>, test-nontxt, ./scripts/test-non-txt-diff.sh)
])dnl
dnl
define([misc_job], [dnl
  misc-$1-jdk$2:
boilerplate(docker_userid/daikon-$1-jdk$2-plus<< pipeline.parameters.testing-suffix >>, test-misc, ./scripts/test-misc.sh)
])dnl
dnl
define([kvasir_job], [dnl
  kvasir-$1-jdk$2:
boilerplate(docker_userid/daikon-$1-jdk$2-plus<< pipeline.parameters.testing-suffix >>, Test Kvasir, ./scripts/test-kvasir.sh)
])dnl
dnl
ifelse([argument 3 is "latest" or "bundled"])dnl
define([typecheck_job], [dnl
  typecheck-$3-$1-jdk$2:
    docker:
      - image: docker_userid/daikon-$1-jdk$2<< pipeline.parameters.testing-suffix >>
boilerplate(docker_userid/daikon-$1-jdk$2<< pipeline.parameters.testing-suffix >>, typecheck $3, scripts/test-typecheck-with-$3-cf.sh)
])dnl
dnl
define([typecheck_job_parts], [dnl
typecheck_job_part($1, $2, $3, part1)
typecheck_job_part($1, $2, $3, part2)
typecheck_job_part($1, $2, $3, part3)])dnl
ifelse([argument 3 is "latest" or "bundled", argument 4 is "part1", "part2", or "part3"])dnl
define([typecheck_job_part], [dnl
  typecheck-$3-$4-$1-jdk$2:
boilerplate(docker_userid/daikon-$1-jdk$2<< pipeline.parameters.testing-suffix >>, typecheck $3, scripts/test-typecheck-with-$3-cf.sh $4)
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
define([job_dependences_part], [dnl
      - $3-$4-$1-jdk$2[]dnl
ifelse($1$2,canary_test,,[:
          requires:
            - canary-jobs
ifelse($2,canary_version,,[dnl
            - $3-$4-$1-jdk[]canary_version
])dnl
ifelse($1,canary_os,,[dnl
            - $3-$4-canary_os[]-jdk$2
])dnl
])dnl
])dnl
dnl
ifelse([
Local Variables:
eval: (add-hook 'after-save-hook '(lambda () (run-command nil "make")) nil 'local)
end:
])dnl

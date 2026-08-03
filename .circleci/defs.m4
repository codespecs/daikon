changequote
changequote(`[',`]')dnl
changecom([], [disable comments, that is, expand within them])dnl
ifelse([The built-in "dnl" m4 macro means "discard to next line".])dnl
dnl
define(job_name, [$1:])
ifelse([Arguments are OS, JDK version number, name, command line.])dnl
define([boilerplate], [dnl
    docker:
      - image: docker_image($1, $2, $3)
    resource_class: large
    environment:
      TERM: dumb
    steps:
      - restore_cache:
          keys:
            - &source-cache source-v1-{{ .Branch }}-{{ .Revision }}
            - source-v1-{{ .Branch }}-
            - source-v1-
      - checkout[]ifelse($3,test-misc.sh,[:
          method: full])
      - save_cache:
          key: *source-cache
          paths:
            - ".git"
      - run:
          name: $3
          command: $4[]ifelse($3,test-misc.sh,[
          no_output_timeout: 20m],$3,test-kvasir.sh,[
          no_output_timeout: 20m],$3,typecheck bundled,[
          no_output_timeout: 20m],$3,typecheck bundled part1,[
          no_output_timeout: 20m],$3,typecheck bundled part2,[
          no_output_timeout: 20m],$3,typecheck bundled part3,[
          no_output_timeout: 30m],$3,typecheck latest,[
          no_output_timeout: 30m],$3,typecheck latest part1,[
          no_output_timeout: 30m],$3,typecheck latest part2,[
          no_output_timeout: 30m],$3,typecheck latest part3,[
          no_output_timeout: 30m],[])
])dnl
dnl
ifelse([Each macro takes two arguments, the OS name and the JDK version.])dnl
dnl
define([quick_job], [dnl
  job_name(quick_$1_jdk$2)
boilerplate($1, $2, test-quick, scripts/test-quick-txt-diff.sh)[]dnl
])dnl
dnl
define([nonquick_job], [dnl
  job_name(nonquick_$1_jdk$2)
boilerplate($1, $2, test-nonquick, scripts/test-nonquick-txt-diff.sh)[]dnl
])dnl
dnl
define([nontxt_job], [dnl
  job_name(nontxt_$1_jdk$2)
boilerplate($1, $2, test-nontxt, scripts/test-non-txt-diff.sh)[]dnl
])dnl
dnl
define([misc_job], [dnl
  job_name(misc_$1_jdk$2)
boilerplate($1, $2, test-misc.sh, make showvars && scripts/test-misc.sh)[]dnl
])dnl
dnl
define([kvasir_job], [dnl
  job_name(kvasir_$1_jdk$2)
boilerplate($1, $2, test-kvasir.sh, scripts/test-kvasir.sh)[]dnl
])dnl
dnl
ifelse([argument 3 is "latest" or "bundled"])dnl
define([typecheck_job], [dnl
  job_name(typecheck_$3_$4_$1_jdk$2)
boilerplate($1, $2, typecheck $3, scripts/test-typecheck-with-$3-cf.sh)[]dnl
])dnl
dnl
define([typecheck_job_parts], [dnl
typecheck_job_part($1, $2, $3, part1)
typecheck_job_part($1, $2, $3, part2)
typecheck_job_part($1, $2, $3, part3)])dnl
ifelse([argument 3 is "latest" or "bundled", argument 4 is "part1", "part2", or "part3"])dnl
define([typecheck_job_part], [dnl
  job_name(typecheck_$3_$4_$1_jdk$2)
boilerplate($1, $2, typecheck $3 $4, scripts/test-typecheck-with-$3-cf.sh $4)[]dnl
])dnl
dnl
define([job_dependences], [dnl
      - $3_$1_jdk$2[]dnl
ifelse($1_jdk$2,canary_version,,[:
          requires:
            - canary_jobs
ifelse($2,canary_jdk,,[dnl
            - $3_$1_jdk[]canary_jdk
])dnl
ifelse($1,canary_os,,[dnl
            - $3_[]canary_os[]_jdk$2
])dnl
])dnl
])dnl
define([job_dependences_part], [dnl
      - $3_$4_$1_jdk$2[]dnl
ifelse($1_jdk$2,canary_version,,[:
          requires:
            - canary_jobs
ifelse($2,canary_jdk,,[dnl
            - $3_$4_$1_jdk[]canary_jdk
])dnl
ifelse($1,canary_os,,[dnl
            - $3_$4_[]canary_os[]_jdk$2
])dnl
])dnl
])dnl
dnl
ifelse([
Local Variables:
eval: (add-hook 'after-save-hook '(lambda () (run-command nil "make")) nil 'local)
end:
])dnl

changequote
changequote(`[',`]')dnl
changecom([], [disable comments, that is, expand within them])dnl
ifelse([The built-in "dnl" m4 macro means "discard to next line".])dnl
dnl
define(job_name, [$1:])
define(dependsOn, needs)
ifelse([Arguments are OS, JDK version number, name, command line.])dnl
define([boilerplate], [dnl
    runs-on: ubuntu-latest
    container:
      image: docker_userid/daikon-$1-jdk$2[]ifelse(test-misc.sh,$3,-plus,test-kvasir.sh,$3,-plus,typecheck bundled,$3,-plus,typecheck bundled part1,$3,-plus,typecheck bundled part2,$3,-plus,typecheck bundled part3,$3,-plus,typecheck latest,$3,-plus,typecheck latest part1,$3,-plus,typecheck latest part2,$3,-plus,typecheck latest part3,$3,-plus)[]docker_testing:latest
    timeout-minutes: 70
    steps:
      - uses: actions/checkout@v7
        with:
          set-safe-directory: true
          fetch-depth: ifelse($3,test-misc.sh,0,25)
      - name: $3
        run: $4
])dnl
dnl
ifelse([Each macro takes two arguments, the OS name and the JDK version.])dnl
dnl
define([quick_job], [dnl
  job_name(quick_$1_jdk$2)
ifelse($1_jdk$2,canary_version,,[dnl
    dependsOn:
      - canary_jobs
ifelse($2,canary_jdk,,[      - quick_$1_jdk[]canary_jdk
])dnl
ifelse($1,canary_os,,[      - quick_[]canary_os[]_jdk$2
])dnl
])dnl
boilerplate($1, $2, test-quick, scripts/test-quick-txt-diff.sh)[]dnl
])dnl
dnl
define([nonquick_job], [dnl
  job_name(nonquick_$1_jdk$2)
ifelse($1_jdk$2,canary_version,,[dnl
    dependsOn:
      - canary_jobs
ifelse($2,canary_jdk,,[      - nonquick_$1_jdk[]canary_jdk
])dnl
ifelse($1,canary_os,,[      - nonquick_[]canary_os[]_jdk$2
])dnl
])dnl
boilerplate($1, $2, test-nonquick, scripts/test-nonquick-txt-diff.sh)[]dnl
])dnl
dnl
define([nontxt_job], [dnl
  job_name(nontxt_$1_jdk$2)
ifelse($1_jdk$2,canary_version,,[dnl
    dependsOn:
      - canary_jobs
ifelse($2,canary_jdk,,[      - nontxt_$1_jdk[]canary_jdk
])dnl
ifelse($1,canary_os,,[      - nontxt_[]canary_os[]_jdk$2
])dnl
])dnl
boilerplate($1, $2, test-nontxt, scripts/test-non-txt-diff.sh)[]dnl
])dnl
dnl
define([misc_job], [dnl
  job_name(misc_$1_jdk$2)
ifelse($1_jdk$2,canary_version,,[dnl
    dependsOn:
      - canary_jobs
ifelse($2,canary_jdk,,[      - misc_$1_jdk[]canary_jdk
])dnl
ifelse($1,canary_os,,[      - misc_[]canary_os[]_jdk$2
])dnl
])dnl
boilerplate($1, $2, test-misc.sh, make showvars && scripts/test-misc.sh)[]dnl
])dnl
dnl
define([kvasir_job], [dnl
  job_name(kvasir_$1_jdk$2)
ifelse($1_jdk$2,canary_version,,[dnl
    dependsOn:
      - canary_jobs
ifelse($2,canary_jdk,,[      - kvasir_$1_jdk[]canary_jdk
])dnl
ifelse($1,canary_os,,[      - kvasir_[]canary_os[]_jdk$2
])dnl
])dnl
boilerplate($1, $2, test-kvasir.sh, scripts/test-kvasir.sh)[]dnl
])dnl
dnl
ifelse([argument 3 is "latest" or "bundled"])dnl
define([typecheck_job], [dnl
  job_name(typecheck_$3_$4_$1_jdk$2)
ifelse($1_jdk$2,canary_version,,[dnl
    dependsOn:
      - canary_jobs
ifelse($2,canary_jdk,,[      - typecheck_$3_$1_jdk[]canary_jdk
])dnl
ifelse($1,canary_os,,[      - typecheck_$3_[]canary_os[]_jdk$2
])dnl
])dnl
boilerplate($1, $2, typecheck $3, scripts/test-typecheck-with-$3-cf.sh)[]dnl
])dnl
define([typecheck_job_parts], [dnl
typecheck_job_part($1, $2, $3, part1)
typecheck_job_part($1, $2, $3, part2)
typecheck_job_part($1, $2, $3, part3)])dnl
ifelse([argument 3 is "latest" or "bundled", argument 4 is "part1", "part2", or "part3"])dnl
define([typecheck_job_part], [dnl
  job_name(typecheck_$3_$4_$1_jdk$2)
ifelse($1_jdk$2,canary_version,,[dnl
    dependsOn:
      - canary_jobs
ifelse($2,canary_jdk,,[      - typecheck_$3_$4_$1_jdk[]canary_jdk[]
])dnl
ifelse($1,canary_os,,[      - typecheck_$3_$4_[]canary_os[]_jdk$2
])dnl
])dnl
boilerplate($1, $2, typecheck $3 $4, scripts/test-typecheck-with-$3-cf.sh $4)[]dnl
])dnl
dnl
dnl
ifelse([
Local Variables:
eval: (add-hook 'after-save-hook '(lambda () (run-command nil "make")) nil 'local)
end:
])dnl

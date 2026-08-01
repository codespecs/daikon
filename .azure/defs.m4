changequote
changequote(`[',`]')dnl
changecom([], [disable comments, that is, expand within them])dnl
ifelse([The built-in "dnl" m4 macro means "discard to next line".])dnl
dnl
ifelse([Arguments are OS, JDK, name, command line.])dnl
define([boilerplate], [dnl
    pool:
      vmImage: 'ubuntu-latest'
    container: docker_userid/daikon-$1-jdk$2[]ifelse(test-misc.sh,$3,-plus,Test Kvasir,$3,-plus,typecheck bundled,$3,-plus,typecheck bundled part1,$3,-plus,typecheck bundled part2,$3,-plus,typecheck bundled part3,$3,-plus,typecheck latest,$3,-plus,typecheck latest part1,$3,-plus,typecheck latest part2,$3,-plus,typecheck latest part3,$3,-plus)[]docker_testing:latest
ifelse(typecheck bundled,$3,[    timeoutInMinutes: 40
],typecheck bundled part1,$3,[    timeoutInMinutes: 40
],typecheck bundled part2,$3,[    timeoutInMinutes: 40
],typecheck bundled part3,$3,[    timeoutInMinutes: 40
],typecheck latest,$3,[    timeoutInMinutes: 40
],typecheck latest part1,$3,[    timeoutInMinutes: 40
],typecheck latest part2,$3,[    timeoutInMinutes: 40
],typecheck latest part3,$3,[    timeoutInMinutes: 40
])dnl
    steps:
      - checkout: self
        fetchDepth: 25
      - bash: $4
        displayName: $3])dnl
dnl
ifelse([Each macro takes two arguments, the OS name and the JDK version.])dnl
dnl
define([quick_job], [dnl
  - job: quick_$1_jdk$2
ifelse($1$2,canary_test,,[dnl
    dependsOn:
      - canary_jobs
ifelse($2,canary_version,,[dnl
      - quick_$1_jdk[]canary_version
])dnl
ifelse($1,canary_os,,[      - quick_[]canary_os[]_jdk$2
])dnl
])dnl
boilerplate($1, $2, test-quick, ./scripts/test-quick-txt-diff.sh)[]dnl
])dnl
dnl
define([nonquick_job], [dnl
  - job: nonquick_$1_jdk$2
ifelse($1$2,canary_test,,[dnl
    dependsOn:
      - canary_jobs
ifelse($2,canary_version,,[      - nonquick_$1_jdk[]canary_version
])dnl
ifelse($1,canary_os,,[      - nonquick_[]canary_os[]_jdk$2
])dnl
])dnl
boilerplate($1, $2, test-nonquick, ./scripts/test-nonquick-txt-diff.sh)[]dnl
])dnl
dnl
define([nontxt_job], [dnl
  - job: nontxt_$1_jdk$2
ifelse($1$2,canary_test,,[dnl
    dependsOn:
      - canary_jobs
ifelse($2,canary_version,,[
      - nontxt_$1_jdk[]canary_version
])dnl
ifelse($1,canary_os,,[      - nontxt_[]canary_os[]_jdk$2
])dnl
])dnl
boilerplate($1, $2, test-nontxt, ./scripts/test-non-txt-diff.sh)[]dnl
])dnl
dnl
define([misc_job], [dnl
  - job: misc_$1_jdk$2
ifelse($1$2,canary_test,,[dnl
    dependsOn:
      - canary_jobs
ifelse($2,canary_version,,[      - misc_$1_jdk[]canary_version
])dnl
ifelse($1,canary_os,,[      - misc_[]canary_os[]_jdk$2
])dnl
])dnl
boilerplate($1, $2, test-misc.sh, make showvars && ./scripts/test-misc.sh)[]dnl
])dnl
dnl
define([kvasir_job], [dnl
  - job: kvasir_$1_jdk$2
ifelse($1$2,canary_test,,[dnl
    dependsOn:
      - canary_jobs
ifelse($2,canary_version,,[dnl
      - kvasir_$1_jdk[]canary_version
])dnl
ifelse($1,canary_os,,[dnl
      - kvasir_[]canary_os[]_jdk$2
])dnl
])dnl
boilerplate($1, $2, Test Kvasir, ./scripts/test-kvasir.sh)[]dnl
])dnl
dnl
ifelse([argument 3 is "latest" or "bundled"])dnl
define([typecheck_job], [dnl
  - job: typecheck_$3_$4_$1_jdk$2
ifelse($1$2,canary_test,,[dnl
    dependsOn:
      - canary_jobs
ifelse($2,canary_version,,[      - typecheck_$3_$1_jdk[]canary_version
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
  - job: typecheck_$3_$4_$1_jdk$2
ifelse($1$2,canary_test,,[dnl
    dependsOn:
      - canary_jobs
ifelse($2,canary_version,,[      - typecheck_$3_$4_$1_jdk[]canary_version[]
])dnl
ifelse($1,canary_os,,[      - typecheck_$3_$4_[]canary_os[]_jdk$2
])dnl
])dnl
boilerplate($1, $2, typecheck $3 $4, ./scripts/test-typecheck-with-$3-cf.sh $4)[]dnl
])dnl
ifelse([
Local Variables:
eval: (add-hook 'after-save-hook '(lambda () (run-command nil "make")) nil 'local)
end:
])dnl

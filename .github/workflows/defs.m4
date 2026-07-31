changequote
changequote(`[',`]')dnl
changecom([], [disable comments, that is, expand within them])dnl
ifelse([The built-in "dnl" m4 macro means "discard to next line".])dnl
dnl
ifelse([Arguments are container, name, command line.])dnl
define([boilerplate], [dnl
    runs-on: ubuntu-latest
    container:
      image: $1
    timeout-minutes: 70
    steps:
      - uses: actions/checkout@v7
        with:
          set-safe-directory: true
          fetch-depth: ifelse($2,test-misc,0,25)
      - name: $2
        run: $3
])dnl
dnl
ifelse([Each macro takes two arguments, the OS name and the JDK version.])dnl
dnl
define([quick_job], [dnl
  quick_$1_jdk$2:
ifelse($1,canary_version,,[    needs:
      - canary_jobs
      - junit_part1_jdk[]canary_version
])dnl
boilerplate(docker_userid/daikon-jdk$2[]docker_testing:latest, test-quick, ./scripts/test-quick-txt-diff.sh)
])dnl
dnl
define([nonquick_job], [dnl
  nonquick_$1_jdk$2:
ifelse($1,canary_version,,[    needs:
      - canary_jobs
      - junit_part1_jdk[]canary_version
])dnl
boilerplate(docker_userid/daikon-jdk$2[]docker_testing:latest, test-nonquick, ./scripts/test-nonquick-txt-diff.sh)
])dnl
dnl
define([nontxt_job], [dnl
  nontxt_$1_jdk$2:
ifelse($1,canary_version,,[    needs:
      - canary_jobs
      - junit_part1_jdk[]canary_version
])dnl
boilerplate(docker_userid/daikon-jdk$2[]docker_testing:latest, test-nontxt, ./scripts/test-non-txt-diff.sh)
])dnl
dnl
define([misc_job], [dnl
  misc_$1_jdk$2:
ifelse($1,canary_version,,[    needs:
      - canary_jobs
      - misc_jdk[]canary_version
])dnl
boilerplate(docker_userid/daikon-jdk$2[]docker_testing:latest, test-misc, ./scripts/test-misc.sh)
])dnl
dnl
define([kvasir_job], [dnl
  kvasir_$1_jdk$2:
ifelse($1,canary_version,,[    needs:
      - canary_jobs
      - kvasir_jdk[]canary_version
])dnl
boilerplate(docker_userid/daikon-jdk$2[]docker_testing:latest, Test Kvasir, ./scripts/test-kvasir.sh)
])dnl
dnl
ifelse([argument 3 is "latest" or "bundled"])dnl
define([typecheck_job], [dnl
ifelse($1,canary_version,[dnl
  typecheck_part1_$1_jdk$2:
boilerplate(docker_userid/daikon-jdk$2[]docker_testing:latest, typecheck-$3, scripts/test-typecheck-with-$3-cf.sh)
])
define([typecheck_job_parts], [dnl
typecheck_job_part($1, $2, $3, part1)
typecheck_job_part($1, $2, $3, part2)
typecheck_job_part($1, $2, $3, part3)])dnl
ifelse([argument 3 is "latest" or "bundled", argument 4 is "part1", "part2", or "part3"])dnl
define([typecheck_job_part], [dnl
  typecheck_part2_$1_jdk$2:
boilerplate(docker_userid/daikon-jdk$2[]docker_testing:latest, test-cftests-typecheck-part2.sh, ./checker/bin-devel/test-cftests-typecheck-part2.sh)
],[dnl
  typecheck_$1_jdk$2:
    needs:
      - canary_jobs
      - typecheck_part1_jdk[]canary_version
      - typecheck_part2_jdk[]canary_version
boilerplate(docker_userid/daikon-jdk$2[]docker_testing:latest, test-cftests-typecheck.sh, ./checker/bin-devel/test-cftests-typecheck.sh)
])dnl
])dnl
dnl
dnl
ifelse([
Local Variables:
eval: (add-hook 'after-save-hook '(lambda () (run-command nil "make")) nil 'local)
end:
])dnl

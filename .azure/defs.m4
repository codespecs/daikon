changequote
changequote(`[',`]')dnl
changecom([], [disable comments, that is, expand within them])dnl
ifelse([the built-in "dnl" macro means "discard to next line"])dnl
define([canary_os], [ubuntu])dnl
define([canary_version], [25])dnl
define([canary_test], [canary_os[]canary_version])dnl
ifelse([each macro takes two arguments, the OS name and the JDK version])dnl
dnl
define([quick_job], [dnl
  - job: quick_$1_jdk$2
ifelse($1$2,canary_test,,[    dependsOn:
      - canary_jobs
ifelse($2,canary_version,,[      - quick_$1_jdk[]canary_version
])dnl
ifelse($1,canary_os,,[      - quick_[]canary_os[]_jdk$2
])dnl
])dnl
    pool:
      vmImage: 'ubuntu-latest'
    container: mdernst/daikon-$1-jdk$2${{ variables.testingSuffix }}:latest
    steps:
      - checkout: self
        fetchDepth: 25
      - bash: ./scripts/test-quick-txt-diff.sh
        displayName: test-quick-txt-diff.sh])dnl
dnl
define([nonquick_job], [dnl
  - job: nonquick_$1_jdk$2
ifelse($1$2,canary_test,,[    dependsOn:
      - canary_jobs
ifelse($2,canary_version,,[      - nonquick_$1_jdk[]canary_version
])dnl
ifelse($1,canary_os,,[      - nonquick_[]canary_os[]_jdk$2
])dnl
])dnl
    pool:
      vmImage: 'ubuntu-latest'
    container: mdernst/daikon-$1-jdk$2${{ variables.testingSuffix }}:latest
    steps:
      - checkout: self
        fetchDepth: 25
      - bash: ./scripts/test-nonquick-txt-diff.sh
        displayName: test-nonquick-txt-diff.sh])dnl
dnl
define([nontxt_job], [dnl
  - job: nontxt_$1_jdk$2
ifelse($1$2,canary_test,,[    dependsOn:
      - canary_jobs
ifelse($2,canary_version,,[      - nontxt_$1_jdk[]canary_version
])dnl
ifelse($1,canary_os,,[      - nontxt_[]canary_os[]_jdk$2
])dnl
])dnl
    pool:
      vmImage: 'ubuntu-latest'
    container: mdernst/daikon-$1-jdk$2${{ variables.testingSuffix }}:latest
    steps:
      - checkout: self
        fetchDepth: 25
      - bash: ./scripts/test-non-txt-diff.sh
        displayName: test-non-txt-diff.sh])dnl
dnl
define([misc_job], [dnl
  - job: misc_$1_jdk$2
ifelse($1$2,canary_test,,[    dependsOn:
      - canary_jobs
ifelse($2,canary_version,,[      - misc_$1_jdk[]canary_version
])dnl
ifelse($1,canary_os,,[      - misc_[]canary_os[]_jdk$2
])dnl
])dnl
    pool:
      vmImage: 'ubuntu-latest'
    container: mdernst/daikon-$1-jdk$2-plus${{ variables.testingSuffix }}:latest
    steps:
      - checkout: self
        fetchDepth: 25
      - bash: make showvars
        displayName: make showvars
      - bash: ./scripts/test-misc.sh
        displayName: test-misc.sh])dnl
dnl
define([kvasir_job], [dnl
  - job: kvasir_$1_jdk$2
ifelse($1$2,canary_test,,[    dependsOn:
      - canary_jobs
ifelse($2,canary_version,,[      - kvasir_$1_jdk[]canary_version
])dnl
ifelse($1,canary_os,,[      - kvasir_[]canary_os[]_jdk$2
])dnl
])dnl
    pool:
      vmImage: 'ubuntu-latest'
    container: mdernst/daikon-$1-jdk$2-plus${{ variables.testingSuffix }}:latest
    steps:
      - checkout: self
        fetchDepth: 25
      - bash: ./scripts/test-kvasir.sh
        displayName: test-kvasir.sh])dnl
dnl
ifelse([argument 3 is "latest" or "bundled"])dnl
define([typecheck_job], [dnl
  - job: typecheck_$3_$4_$1_jdk$2
ifelse($1$2,canary_test,,[    dependsOn:
      - canary_jobs
ifelse($2,canary_version,,[      - typecheck_$3_$1_jdk[]canary_version
])dnl
ifelse($1,canary_os,,[      - typecheck_$3_[]canary_os[]_jdk$2
])dnl
])dnl
    pool:
      vmImage: 'ubuntu-latest'
    container: mdernst/daikon-$1-jdk$2-plus${{ variables.testingSuffix }}:latest
    timeoutInMinutes: 80
    steps:
      - checkout: self
        fetchDepth: 25
      - bash: |
          java -version
          javac -version
        displayName: show Java version
      - bash: ./scripts/test-typecheck-with-$3-cf.sh
        displayName: test-typecheck-with-$3-cf.sh])dnl
define([typecheck_job_parts], [dnl
typecheck_job_part($1, $2, $3, part1)
typecheck_job_part($1, $2, $3, part2)
typecheck_job_part($1, $2, $3, part3)])dnl
ifelse([argument 3 is "latest" or "bundled", argument 4 is "part1", "part2", or "part3"])dnl
define([typecheck_job_part], [dnl
  - job: typecheck_$3_$4_$1_jdk$2
ifelse($1$2,canary_test,,[    dependsOn:
      - canary_jobs
ifelse($2,canary_version,,[      - typecheck_$3_$4_$1_jdk[]canary_version[]
])dnl
ifelse($1,canary_os,,[      - typecheck_$3_$4_[]canary_os[]_jdk$2
])dnl
])dnl
    pool:
      vmImage: 'ubuntu-latest'
    container: mdernst/daikon-$1-jdk$2-plus${{ variables.testingSuffix }}:latest
    timeoutInMinutes: 40
    steps:
      - checkout: self
        fetchDepth: 25
      - bash: |
          java -version
          javac -version
        displayName: show Java version
      - bash: ./scripts/test-typecheck-with-$3-cf.sh $4
        displayName: test-typecheck-with-$3-cf.sh $4])dnl
ifelse([
Local Variables:
eval: (add-hook 'after-save-hook '(lambda () (run-command nil "make")) nil 'local)
end:
])dnl

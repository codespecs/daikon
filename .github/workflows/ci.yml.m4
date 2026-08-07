# DO NOT EDIT ci.yml. Edit ci.yml.m4 and defs.m4 instead.

changequote
changequote(`[',`]')dnl
include([../../.azure/defs-common.m4])dnl
include([defs.m4])dnl
name: CI

"on":
  push:
    branches:
      - "**"
  pull_request:
    branches:
      - "**"

# concurrency:
#   group: ${{ github.workflow }}-${{ github.event.pull_request.head.repo.full_name || github.repository }}-${{ github.head_ref || github.ref_name }}
#   cancel-in-progress: ${{ github.ref != 'refs/heads/master' }}

# Cancel in-progress jobs.
# For a pull request, "github.ref" is "refs/pull/NUMBER/merge", which is unique
# per pull request even when the pull request originates from a fork.
concurrency:
  group: ${{ github.workflow }}-${{ github.ref }}
  cancel-in-progress: true

permissions:
  contents: read

env:
  GIT_CONFIG_COUNT: "1"
  GIT_CONFIG_KEY_0: safe.directory
  GIT_CONFIG_VALUE_0: ${{ github.workspace }}

jobs:

  # The needs clauses are:
  #  * Everything depends on the canary jobs (the main jdk25 jobs), except those jobs themselves.
  #  * Any other *_jdkNN job depends on the corresponding *_jdk25 job.

  canary_jobs:
    needs:
      - quick_[]canary_os[]_jdk[]canary_jdk
      - nonquick_[]canary_os[]_jdk[]canary_jdk
      - nontxt_[]canary_os[]_jdk[]canary_jdk
      - misc_[]canary_os[]_jdk[]canary_jdk
      - kvasir_[]canary_os[]_jdk[]canary_jdk
      - typecheck_latest_part1_[]canary_os[]_jdk[]canary_jdk
      - typecheck_latest_part2_[]canary_os[]_jdk[]canary_jdk
      - typecheck_latest_part3_[]canary_os[]_jdk[]canary_jdk
      - typecheck_bundled_part1_[]canary_os[]_jdk[]canary_jdk
      - typecheck_bundled_part2_[]canary_os[]_jdk[]canary_jdk
      - typecheck_bundled_part3_[]canary_os[]_jdk[]canary_jdk
    runs-on: ubuntu-latest
    steps:
      - name: canary_jobs
        run: "true"
  ci_info:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v7
        with:
          set-safe-directory: true
          fetch-depth: 0
      - name: clone_plume_scripts
        run: git clone https://github.com/plume-lib/plume-scripts.git /tmp/plume-scripts
      - name: ci_info
        run: /tmp/plume-scripts/ci-info --debug

include([../../.azure/jobs.m4])dnl

ifelse([
Local Variables:
eval: (add-hook 'after-save-hook '(lambda () (run-command nil "make")) nil 'local)
end:
])dnl

quick_job(ubuntu, 8)
quick_job(ubuntu, 11)
quick_job(ubuntu, 17)
quick_job(ubuntu, 21)
quick_job(ubuntu, 25)

nonquick_job(ubuntu, 8)
nonquick_job(ubuntu, 11)
nonquick_job(ubuntu, 17)
nonquick_job(ubuntu, 21)
nonquick_job(ubuntu, 25)

nontxt_job(ubuntu, 8)
nontxt_job(ubuntu, 11)
nontxt_job(ubuntu, 17)
nontxt_job(ubuntu, 21)
nontxt_job(ubuntu, 25)

misc_job(ubuntu, 8)
misc_job(ubuntu, 11)
misc_job(ubuntu, 17)
misc_job(ubuntu, 21)
misc_job(ubuntu, 25)

kvasir_job(ubuntu, 8)
kvasir_job(ubuntu, 11)
kvasir_job(ubuntu, 17)
kvasir_job(ubuntu, 21)
kvasir_job(ubuntu, 25)

  ## The Checker Framework cannot be built under Java 8, 11 or 17.
  ## Thus, there is no typecheck_latest_ubuntu_jdk{8,11,17} job.
  ## The Checker Framework cannot be run under Java 8 or 11.
  ## Thus, there is no typecheck_bundled_ubuntu_jdk_{8,11} job.
typecheck_bundled_job(ubuntu, 17)
typecheck_latest_job(ubuntu, 21)
typecheck_bundled_job(ubuntu, 21)
typecheck_latest_job(ubuntu, 25)
typecheck_bundled_job(ubuntu, 25)

quick_job(rockylinux, 8)
quick_job(rockylinux, 11)
quick_job(rockylinux, 17)
quick_job(rockylinux, 21)
quick_job(rockylinux, 25)

nonquick_job(rockylinux, 8)
nonquick_job(rockylinux, 11)
nonquick_job(rockylinux, 17)
nonquick_job(rockylinux, 21)
nonquick_job(rockylinux, 25)

nontxt_job(rockylinux, 8)
nontxt_job(rockylinux, 11)
nontxt_job(rockylinux, 17)
nontxt_job(rockylinux, 21)
nontxt_job(rockylinux, 25)

misc_job(rockylinux, 8)
misc_job(rockylinux, 11)
misc_job(rockylinux, 17)
misc_job(rockylinux, 21)
misc_job(rockylinux, 25)

kvasir_job(rockylinux, 8)
kvasir_job(rockylinux, 11)
kvasir_job(rockylinux, 17)
kvasir_job(rockylinux, 21)
kvasir_job(rockylinux, 25)

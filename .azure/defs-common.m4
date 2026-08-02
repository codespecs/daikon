changequote
changequote(`[',`]')dnl
changecom([], [disable comments, that is, expand within them])dnl
ifelse([the built-in "dnl" macro means "discard to next line"])dnl
define([canary_os], [ubuntu])dnl
define([canary_jdk], [25])dnl
define([canary_version], [canary_os[]_jdk[]canary_jdk])dnl
define([docker_userid], [mdernst])
define([docker_testing], [])

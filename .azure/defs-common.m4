changequote
changequote(`[',`]')dnl
changecom([], [disable comments, that is, expand within them])dnl
ifelse([the built-in "dnl" macro means "discard to next line"])dnl
define([canary_os], [ubuntu])dnl
define([canary_version], [25])dnl
define([canary_test], [canary_os[]canary_version])dnl
define([docker_userid], [mdernst])

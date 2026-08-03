changequote
changequote(`[',`]')dnl
changecom([], [disable comments, that is, expand within them])dnl
ifelse([the built-in "dnl" macro means "discard to next line"])dnl
define([canary_os], [ubuntu])dnl
define([canary_jdk], [25])dnl
define([canary_version], [canary_os[]_jdk[]canary_jdk])dnl
define([docker_userid], [mdernst])
define([docker_testing], [])
define([docker_image], [docker_userid/daikon-$1-jdk$2[]ifelse(test-misc.sh,$3,-plus,test-kvasir.sh,$3,-plus,typecheck bundled,$3,-plus,typecheck bundled part1,$3,-plus,typecheck bundled part2,$3,-plus,typecheck bundled part3,$3,-plus,typecheck latest,$3,-plus,typecheck latest part1,$3,-plus,typecheck latest part2,$3,-plus,typecheck latest part3,$3,-plus)[]docker_testing:latest])

# pag-daikon.cshrc
# This file should be kept in sync with pag-daikon.bashrc.

setenv DAIKONPARENT ${HOME}/research
setenv DAIKONDIR ${DAIKONPARENT}/invariants
setenv INV ${DAIKONPARENT}/invariants
setenv inv ${INV}
setenv DAIKONCLASS_SOURCES 1

source ${INV}/scripts/daikon.cshrc

# Should also remove "invariants/bin" from PATH.
setenv PATH $DAIKONPARENT/scripts:${PATH}:/home/httpd/html/pag/daikon/download/binaries/mit

setenv LD_LIBRARY_PATH /usr/X11R6/lib:/usr/local/lib:/usr/lib:/lib

setenv DAIKON_LIBS `/usr/bin/perl -e 'print join(":", @ARGV);' ${INV}/java/lib/*.jar`
setenv CLASSPATH .:${CLASSPATH}:${DAIKON_LIBS}

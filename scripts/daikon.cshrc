## daikon.cshrc
## Daikon initialization file for C shell (csh and tcsh) users

## Set this directory to the directory containing "daikon".
setenv DAIKONPARENT /path/to/parent/of/daikon

## You should uncomment exactly one of the following lines, depending on
## want to run Daikon from the precompiled bytecode files in daikon.jar, or
## you want to run Daikon from .class files that you compile yourself.  The
## former is easier, but the latter permits you to modify Daikon (most
## users will not need to do so).  The latter may be used only if you
## downloaded the source distribution.
setenv CLASSPATH $DAIKONPARENT/daikon/daikon.jar:${CLASSPATH}
# setenv CLASSPATH $DAIKONPARENT/daikon/java:${CLASSPATH}

## Add the Daikon binaries to your path
set path = ($DAIKONPARENT/daikon/bin $path)

## If you wish to use dfej, the Daikon front end for Java, you need to have
## rt.jar on your classpath.  Set the directory appropriately.  If you do
## not wish to use dfej, you may comment out this line.
setenv CLASSPATH ${CLASSPATH}:/g2/jdk1.3.1/jre/lib/rt.jar

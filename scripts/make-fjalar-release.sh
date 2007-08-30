#!/bin/sh

# - Copy over the entire valgrind-3 directory and all sub-directories
# into a folder and cd there.
# - do a 'make clean' and 'make distclean'

# mkdir /tmp/fjalar-release
# cp -rv $DAIKONDIR/valgrind-3 /tmp/fjalar-release
# cd /tmp/fjalar-release/valgrind-3/valgrind
# make clean
# make distclean
# cd ..

# Alternate approach: check out from CVS instead
mkdir /tmp/fjalar-release
cd /tmp/fjalar-release
cvs -d /afs/csail.mit.edu/group/pag/projects/invariants/.CVS co -P valgrind-3
cd valgrind-3

# - Remove all CVS/ directories:

find . -type d -name CVS | xargs rm -rf

# - Remove all DynComp-specific things from Valgrind and VEX files, in
# addition to Memcheck files:

# (grep for ' PG' or 'pgbovine' to see which files I modified)

# VEX/priv/host-generic/reg_alloc2.c
# coregrind/m_scheduler/scheduler.c
# coregrind/pub_core_threadstate.h
# coregrind/m_machine.c (only remove DynComp-specific things like the tag stuff)
# include/pub_tool_machine.h (only remove DynComp-specific things like the tag stuff)
# - Cut out all dyncomp-related stuff from mc_main.c and mc_translate.c
# (grep for 'PG', 'pgbovine', or 'dyncomp' - this may not be a trivial
# task)
# - Change mc_pre_clo_init() in mc_main.c to not print out the heading
# that says Kvasir/DynComp but instead print out something
# Fjalar-related.  And also change the contact info. and version number.
# - Change fjalar/Makefile.am to build basic-tool instead of kvasir by
# removing kvasir/ source files and adding 'basic-tool/basic-tool.c' to
# FJALAR_SOURCES_COMMON

# Uncomment the following if you need to regenerate the patch;

#cp -rv valgrind valgrind-kvasir
#exit;

# apply the parts of the old patch you want to keep,
# make any futher changes to valgrind/, and then do 
# diff -ur valgrind-kvasir valgrind \ 
#   >$DAIKONDIR/kvasir/fjalar/notes/fjalar-release.patch

patch -p0 <$DAIKONDIR/kvasir/fjalar/notes/fjalar-release.patch

# It would actually be nice to just do the autogen.sh again, since
# that's all that really affects the release. But this is more
# future-proof, and it has the side benefit of checking that the
# release actually builds!
./auto-everything.sh
cd valgrind
make distclean
cd ..

# - Remove notes subdirectory and other misc. subdirectories within fjalar/

rm -rv valgrind/fjalar/notes

# - Remove kvasir/ sub-directory within fjalar/, leaving only basic-tool

rm -rv valgrind/fjalar/kvasir

# - ln -s valgrind-3/valgrind/fjalar/ fjalar-source to create a
# convenient link from the top level to the Fjalar source code directory

cd ..
ln -s valgrind-3/valgrind/fjalar fjalar-source

# - Replace the valgrind-3/valgrind/fjalar/README file with the
# following contents (below the line)

cp $DAIKONDIR/kvasir/fjalar/notes/fjalar-release-README valgrind-3/valgrind/fjalar/README

# - ln -s valgrind-3/valgrind/fjalar/README README to create a
# convenient link from the top level to the Fjalar README file

ln -s valgrind-3/valgrind/fjalar/README README

# - Remove all temporary ~ files created by emacs

find . -name '*~' -exec rm {} \;

# - Remove other crappy temp files like .# files "

find . -name '.#*' -exec rm {} \;

# - grep again for kvasir, dyncomp, etc... to make sure we don't have
# any strays

# - rm -rf valgrind-3/valgrind/inst to not leave any binaries lying
# around which take up dead space

rm -rf valgrind-3/valgrind/inst

# Clean space-wasting parts of the VEX SVN tree:

rm -rf valgrind-3/valgrind/VEX/orig_*

# - Go to valgrind-3/valgrind/fjalar/documentation/fjalar-www and clear
# out old versions of Fjalar

rm -f valgrind-3/valgrind/fjalar/documentation/fjalar-www/fjalar-*.tar.gz

# - Put updated copies of fjalar_include.h and fjalar_tool.h on the main
# Fjalar website, because those serve as the public interfaces

# - Make a copy of the Fjalar home page and programmer's manual into the
# valgrind-3/valgrind/fjalar/documentation directory.

# - tar it up and put in on the web!

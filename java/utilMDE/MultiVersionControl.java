package utilMDE;

import java.io.*;

// TO DO:  Make this search for all CVS, .hg, and .svn files in the
// filesystem, and report about them all.  In fact, that may be preferable.
// The list is for making sure that a subset exist and are up to date.

/**
 * This program lets you run a version control command, such as "update" or
 * "diff", on a set of CVS/SVN/Hg checkouts rather than just one.
 * <p>
 *
 * This can simplify the process of managing many checkouts.  It is not
 * uncommon for a developer to have multiple checkouts at once.  You might
 * want to know whether you have uncommitted changes in any of them, or you
 * might want to update all of them.  Or, when setting up a new account,
 * you might want to check them all out.  This program does any of those
 * tasks.  In particular, it accepts these arguments:
 * <pre>
 *   checkout         -- checks out all repositories
 *   mdecvs update    -- update all checked out repositories
 *   mdecvs diff      -- list files that are not committed
 * </pre><p>
 *
 * The "repositories" file contains a list of sections.  Each section names
 * one repository, and a list of directories in which a checkout of a
 * different module appears.  Each repository name is prefixed
 * by the type of the repository.  Examples include:
 * <pre>
 * CVSROOT: :ext:login.csail.mit.edu:/afs/csail.mit.edu/u/m/mernst/.CVS/.CVS-mernst
 * SVNROOT: svn+ssh://tricycle.cs.washington.edu/cse/courses/cse403/09sp/
 * HGROOT: https://jsr308-langtools.googlecode.com/hg/
 * </pre><p>
 *
 * Each repository begins a section of the file.  A section consists of a
 * list of checkout directories.  For example, a section might be
 * <pre>
 * SVNROOT: https://crashma.googlecode.com/svn/trunk/
 * ~/research/crashma
 *
 * HGROOT: https://jsr308-langtools.googlecode.com/hg/
 * cd ~/research/types/jsr308-langtools
 * CVSROOT: :ext:login.csail.mit.edu:/afs/csail.mit.edu/group/pag/projects/classify-tests/.CVS
 * ~/research/testing/symstra-eclat-paper
 * ~/research/testing/symstra-eclat-code
 * ## This is last because it's slow.
 * ~/research/testing/eclat
 * </pre><p>
 *
 * The basename of the directory is assumed to be the same as the name of
 * the module in the repository.  If this is not the case, then you can
 * override it.  [SAY HOW.]
 *
 * When performing a checkout, the parent directiories are created if
 * needed.
 */
public class MultiVersionControl {

  @Option("File with list of repositories")
  public String repositories = "$ENV{HOME}/.mvcrepos";

  @Option("Display commands as they are executed")
  public boolean show;

  @Option("Show the directories in which commands will be executed")
  public boolean show_directory;

  @Option("Do not execute commands; just show them.  Implies --show --redo-existing")
  public boolean dry_run;

  /**  Default is for checkout command to skip existing directories. */
  @Option("Redo existing checkouts; relevant only to checkout command")
  public boolean redo_existing;

  @Option("Name of the executable for this program")
  public String program_name;

  @Option("Print debugging output")
  public boolean debug;

  /** "checkout", "update", or "diff" */
  private String action;



  public static void main (String[] args) {
    MultiVersionControl mvc = new MultiVersionControl(args);
    mvc.process();
  }

  public MultiVersionControl(String[] args) {
    parseArgs(args);
  }

  public void parseArgs(String[] args) /*@Raw*/ {
    Options options = new Options ("mvc [options] {checkout,update,diff}", this);
    String[] remaining_args = options.parse_or_usage (args);
    if (remaining_args.length != 1) {
      options.print_usage("Please supply exactly one argument; found %d: %s", remaining_args.length, UtilMDE.join(remaining_args, " "));
    }
    action = remaining_args[0].intern();
    if ("checkout".startsWith(action)) {
      action = "checkout";
    } else if ("diff".startsWith(action)) {
      action = "diff";
    } else if ("update".startsWith(action)) {
      action = "update";
    } else {
      options.print_usage("Unrecognized action \"%s\" should be one of \"checkout\", \"diff\", or \"commit\"", action);
    }

    // clean up options
    if (dry_run) {
      show = true;
      redo_existing = true;
    }

    if (action == "checkout") { // interned
      show = true;
    }
    if (action == "update") {   // interned
      show_directory = true;
    }

    if (debug) {
      show = true;
    }

  }

  enum RepoType { cvs, hg, svn };

  public void process() {
    String repo;

  }

  /** Find all checkouts under the given directory. */
  static void findCheckouts(File f) {
  }


}



// # At least one of these is always undefined, and we are using the other one.
// my $cvsroot;
// my $svnroot;
//
// my $dirfile = "$ENV{HOME}/bin/share/mdecvsdirs";
// open(DIRS, $dirfile) or die "Can't open '$dirfile: $!";
// my @dirs = <DIRS>;
// close(DIRS);
//
// for my $dir (@dirs) {
//   # print "line: $dir\n";
//   chomp($dir);
//   # Skip comments and blank lines.
//   if (($dir eq "") || ($dir =~ /^\#/)) {
//     next;
//   }
//   if ($dir =~ /^CVSROOT: (.*)$/) {
//     $cvsroot = $1;
//     $svnroot = undef;
//     # If the CVSROOT is remote, try to make it local.
//     if ($cvsroot =~ /^:ext:.*:(.*)$/) {
//       if (-e $1) {
//         $cvsroot = $1;
//       }
//     }
//     next;
//   }
//   if ($dir =~ /^SVNROOT: (.*)$/) {
//     $svnroot = $1;
//     $cvsroot = undef;
//     next;
//   }
//   if ($dir =~ /^(~\/.*?)([^\/]*)$/) {
//     my $base = $1;
//     my $module = $2;
//     # Replace "~" by "$HOME", because -d (and Athena's "cd" command) does not
//     # understand ~, but it does understand $HOME.
//     my $dir_expanded = $dir;
//     $dir_expanded =~ s/^~\//$ENV{HOME}\//;
//     $base =~ s/^~\//$ENV{HOME}\//;
//
//     # Compute the command
//     my $command_cwd = $dir_expanded;            # working directory for command
//     my $command;
//     my $filter;
//     if ($action eq "checkout") {
//       $command_cwd = $base;
//       if (defined($cvsroot)) {
//         # Need a way to specify "-ko" option after "checkout".
//         # "-P" means prune empty directories.
//         $command = "$cvs -d $cvsroot checkout -P $module";
//       } else {
//         # To do:  can specify the local name, which might differ from the
//         # repository directory (such as "trunk"), by specifying it as the
//         # last argument to "svn checkout".  Support this.
//         $command = "svn checkout ${svnroot}$module";
//       }
//     } elsif ($action eq "update") {
//       if (defined($cvsroot)) {
//         $command = "$cvs -d $cvsroot -Q update -d";
//         $filter = "grep -v \"config: unrecognized keyword 'UseNewInfoFmtStrings'\"";
//       } else {
//         $command = "svn -q update";
//         $filter = "grep -v \"Killed by signal 15.\"";
//       }
//     } elsif ($action eq "diff") {
//       if (defined($cvsroot)) {
//         $command = "$cvs -d $cvsroot -Q diff -b --brief -N";
//         # For the last perl command, this also works:
//         #   perl -p -e 'chomp(\$cwd = `pwd`); s/^Index: /\$cwd\\//'";
//         # but the one we use is briefer and uses the abbreviated directory name.
//         $filter = "grep -v \"unrecognized keyword 'UseNewInfoFmtStrings'\" | grep \"^Index:\" | perl -p -e 's|^Index: |$dir\\/|'";
//       } else {
//         # Both of these work.  The second one prints the whole diff, then just greps out the "Index" line, whereas the first one doesn't compute the whole diff.
//         $command = "svn diff --diff-cmd diff -x -q -x -r -x -N";
//         $filter = "grep \"^Index:\" | perl -p -e 's|^Index: |$dir\\/|'";
//         # $command = "svn diff";
//         # $filter = "grep \"^Index:\" | perl -p -e 's|^Index: |$dir\\/|'";
//       }
//     } else {
//       die "bad action $action";
//     }
//     # $command = "$command 2>&1";
//
//     # Check that the directory exists (OK if it doesn't for checkout).
//     if ($debug) {
//       print "$dir:\n";
//     }
//     if (! -e $dir_expanded) {
//       # Directory was not found
//       if (-e $dir) { die "Found $dir but not $dir_expanded"; } # sanity check
//       if ($action eq "checkout") {
//         if ($show_commands) {
//           print "Does not exist"
//             . ($perform_commands ? " (creating)" : "")
//             . ": $dir\n";
//         }
//         if ($perform_commands) {
//           maybe_mkdir($base);
//         }
//       } else {
//         print "Did not find directory $dir = $dir_expanded\n";
//         next;
//       }
//     } else {
//       # Directory was found
//       if ($action eq "checkout" && ! $redo_existing) {
//         print "Skipping checkout (dir already exists): $dir\n";
//         next;
//       }
//     }
//     if (chdir($command_cwd)) {
//       if ($debug) { print "changed to $command_cwd\n"; }
//     } else {
//       print "couldn't chdir to $command_cwd: $!\n";
//       if ($debug) {
//         printf "does it exist? %d\n", (-e $command_cwd);
//         printf "does it exist (2)? %d\n", (-e "$command_cwd/");
//       }
//       if (! $show_commands) {
//         next;
//       }
//     }
//     if ($debug) {
//       print("pwd: ");
//       system("pwd");
//     }
//
//     # Show the command.
//     if ($show_commands) {
//       if (($action eq "checkout")
//           # Better would be to change the printed (but not executed) command
//           # || (($action eq "update") && defined($svnroot))
//           || ($action eq "update")) {
//         print "cd $command_cwd\n";
//       }
//       print "command: $command\n";
//     }
//
//     # Perform the command
//     if ($debug) { "perform_commands = $perform_commands\n"; }
//     if ($perform_commands) {
//       my $tmpfile = "/tmp/cmd-output-$$";
//       # For debugging
//       # my $command_cwd_sanitized = $command_cwd;
//       # $command_cwd_sanitized =~ s/\//_/g;
//       # my $tmpfile = "/tmp/cmd-output-$$-$command_cwd_sanitized";
//       my $command_redirected = "$command > $tmpfile 2>&1";
//       if ($debug) { print "About to execute: $command_redirected\n"; }
//       my $result = system("$command_redirected");
//       if ($debug) { print "Executed: $command_redirected\n"; }
//       if ($debug) { print "raw result = $result\n"; }
//       if ($result == -1) {
//         print "failed to execute: $command_redirected: $!\n";
//       } elsif ($result & 127) {
//         printf "child died with signal %d, %s coredump\n",
//         ($result & 127),  ($result & 128) ? 'with' : 'without';
//       } else {
//         # Problem:  diff returns failure status if there were differences
//         # or if there was an error, so ther's no good way to detect errors.
//         $result = $result >> 8;
//         if ($debug) { print "shifted result = $result\n"; }
//         if ((($action eq "diff") && ($result != 0) && ($result != 1))
//             || (($action ne "diff") && ($result != 0))) {
//           print "exit status $result for:\n  cd $command_cwd;\n  $command_redirected\n";
//           system("cat $tmpfile");
//         }
//       }
//       # Filter the output
//       if (defined($filter)) {
//         system("cat $tmpfile | $filter > $tmpfile-2");
//         rename("$tmpfile-2", "$tmpfile");
//       }
//       if ($debug && $show_directory) {
//         print "show-directory: $dir:\n";
//         printf "tmpfile size: %d, zeroness: %d, non-zeroness %d\n", (-s $tmpfile), (-z $tmpfile), (! -z $tmpfile);
//       }
//       if ((! -z $tmpfile) && $show_directory) {
//         print "$dir:\n";
//       }
//       system("cat $tmpfile");
//       unlink($tmpfile);
//     }
//     next;
//   }
//   print "IGNORED LINE: $dir\n";
// }
//
// sub maybe_mkdir ( $ ) {
//   my ($base) = @_;
//   my $base_expanded = $base;
//   $base_expanded =~ s/^~/$ENV{HOME}/;
//   # print "base_expanded: $base_expanded\n";
//   if (! -e $base_expanded) {
//     my $command = "mkdir -p $base";
//     print "COMMAND: $command\n";
//     my $result = system($command);
//     if ($result != 0) {
//       die "system failed: $?";
//     }
//   }
// }

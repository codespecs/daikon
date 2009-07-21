package utilMDE;

import org.tmatesoft.svn.core.wc.*;
import org.tmatesoft.svn.core.internal.io.dav.DAVRepositoryFactory;
import org.tmatesoft.svn.core.internal.io.fs.FSRepositoryFactory;
import org.tmatesoft.svn.core.internal.io.svn.SVNRepositoryFactoryImpl;
import org.tmatesoft.svn.core.auth.ISVNAuthenticationManager;
import org.tmatesoft.svn.core.*;
import org.ini4j.Ini;

import java.io.*;
import java.util.*;
import java.net.URL;

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
 * different module appears.  Each repository name is prefixed by the type
 * of the repository.  Each directory may be suffixed by the name of the
 * module being checked out (relevant only to CVS and SVN); it defaults to
 * the last component of the directory.  (Note:  This prohibits file names
 * with spaces!  Bad!)
 *
 * Examples include:
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

// TODO:  There is a problem when two modules from the same SVN repository
// are checked out, with one checkout inside the other.  It doesn't always
// manifest, and I am ignoring it for now.

public class MultiVersionControl {

  @Option("File with list of checkouts.  Set it to /dev/null to suppress reading.")
  public String repositories = new File(userHome, ".mvc-checkouts").getPath();

  @Option("Directory under which to search for checkouts; default=home dir")
  public List<String> dir = new ArrayList<String>();

  @Option("Searches for all checkouts, not just those listed in a file; default=true")
  public boolean search = true;

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
  public static boolean debug;

  /** "checkout", "update", or "diff" */
  private String action;


  @SuppressWarnings("nullness") // user.home property always exists
  static final /*@NonNull*/ String userHome = System.getProperty ("user.home");

  public static void main (String[] args) {
    setupSVNKIT();
    MultiVersionControl mvc = new MultiVersionControl(args);

    Set<Checkout> checkouts = new LinkedHashSet<Checkout>();

    try {
      readCheckouts(new File(mvc.repositories), checkouts);
    } catch (IOException e) {
      throw new Error("Problem reading file " + mvc.repositories, e);
    }

    if (mvc.search) {
      for (String adir : mvc.dir) {
        if (debug) {
          System.out.println("Searching for checkouts under " + adir);
        }
        findCheckouts(new File(adir), checkouts);
      }
    }

    mvc.process(checkouts);
  }

  private static void setupSVNKIT() {
    DAVRepositoryFactory.setup();
    SVNRepositoryFactoryImpl.setup();
    FSRepositoryFactory.setup();
  }

  public MultiVersionControl(String[] args) {
    parseArgs(args);
  }

  public void parseArgs(String[] args) /*@Raw*/ {
    Options options = new Options ("mvc [options] {checkout,update,diff}", this);
    String[] remaining_args = options.parse_or_usage (args);
    if (remaining_args.length != 1) {
      options.print_usage("Please supply exactly one argument (found %d)%n%s", remaining_args.length, UtilMDE.join(remaining_args, " "));
      System.exit(1);
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
    if (dir.isEmpty()) {
      dir.add(userHome);
    }

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

  enum RepoType {
    BZR,
      CVS,
      HG,
      SVN };

  /**
   */
  // TODO: have subclasses of Checkout
  static class Checkout {
    RepoType repoType;
    /** Local directory */
    // actually the parent directory?
    File directory;
    /**
     * Null for distributed version control systems (Bzr, Hg).
     */
    // (Most operations don't need this.  Useful for checkout, though.)
    /*@Nullable*/ String repositoryRoot;
    /**
     * Null if no module, just whole thing.
     * Non-null for CVS and, optionally, for SVN.
     */
    /*@Nullable*/ String repositoryModule;


    Checkout(RepoType repoType, File directory) {
      this(repoType, directory, null, null);
    }

    Checkout(RepoType repoType, File directory, /*@Nullable*/ String repositoryRoot, /*@Nullable*/ String repositoryModule) {
      assert directory.isDirectory() : "Not a directory: " + directory;
      this.repoType = repoType;
      this.directory = directory;
      this.repositoryRoot = repositoryRoot;
      this.repositoryModule = repositoryModule;
      // These asserts come at the end so that the error message can be better.
      switch (repoType) {
      case BZR:
        assert new File(directory, ".bzr").isDirectory();
        assert repositoryModule == null;
        break;
      case CVS:
        assert new File(directory, "CVS").isDirectory() : "Bad CVS: " + this;
        assert repositoryModule != null : "Bad CVS: " + this;
        break;
      case HG:
        assert new File(directory, ".hg").isDirectory();
        assert repositoryModule == null;
        break;
      case SVN:
        assert new File(directory, ".svn").isDirectory() : "Bad SVN: " + this;
        assert repositoryModule == null : "Bad SVN: " + this;
        break;
      }
    }

    @Override
    public boolean equals(/*@Nullable*/ Object other) {
      if (! (other instanceof Checkout))
        return false;
      Checkout c2 = (Checkout) other;
      return ((repoType == c2.repoType)
              && directory.equals(c2.directory)
              && ((repositoryRoot == null)
                  ? (repositoryRoot == c2.repositoryRoot)
                  : repositoryRoot.equals(repositoryRoot))
              && ((repositoryModule == null)
                  ? (repositoryModule == c2.repositoryModule)
                  : repositoryModule.equals(repositoryModule)));
    }

    @Override
    public int hashCode() {
      return (repoType.hashCode()
              + directory.hashCode()
              + (repositoryRoot == null ? 0 : repositoryRoot.hashCode())
              + (repositoryModule == null ? 0 : repositoryModule.hashCode()));
    }

    @Override
      public String toString() {
      return repoType
        + " " + directory
        + " " + repositoryRoot
        + " " + repositoryModule;
    }

  }


  ///////////////////////////////////////////////////////////////////////////
  /// Read checkouts from a directory
  ///

  static void readCheckouts(File file, Set<Checkout> checkouts) throws IOException {
    RepoType currentType = RepoType.BZR; // arbitrary choice
    String currentRoot = null;

    for (String line : new EntryReader(file)) {
      if (debug) {
        System.out.println("line: " + line);
      }
      line = line.trim();
      // Skip comments and blank lines
      if (line.equals("") || line.startsWith("#")) {
        continue;
      }

      String[] splitTwo = line.split("[ \t]+");
      if (debug) {
        System.out.println("split length: " + splitTwo.length);
      }
      if (splitTwo.length == 2) {
        String word1 = splitTwo[0];
        String word2 = splitTwo[1];
        if (word1.equals("BZRROOT:")) {
          throw new Error("bzr not yet supported");
          // continue;
        } else if (word1.equals("CVSROOT:")) {
          currentType = RepoType.CVS;
          currentRoot = word2;
          // If the CVSROOT is remote, try to make it local.
          if (currentRoot.startsWith(":ext:")) {
            String[] rootWords = currentRoot.split(":");
            String possibleRoot = rootWords[rootWords.length-1];
            if (new File(possibleRoot).isDirectory()) {
              currentRoot = possibleRoot;
            }
          }
          continue;
        } else if (word1.equals("HGROOT:")) {
          currentType = RepoType.HG;
          currentRoot = word2;
          continue;
        } else if (word1.equals("SVNROOT:")) {
          currentType = RepoType.SVN;
          currentRoot = word2;
          continue;
        }
      }

      if (currentRoot == null) {
        throw new Error("need root before directory: " + line);
      }

      // Replace "~" by "$HOME", because -d (and Athena's "cd" command) does not
      // understand ~, but it does understand $HOME.
      File dir = new File(line.replaceFirst("^~", userHome));
      if (! dir.exists()) {
        System.out.println("Cannot find directory: " + dir);
        continue;
      }

      String module = null;
      if (currentType == RepoType.CVS) {
        module = dir.getName();
      }

      checkouts.add(new Checkout(currentType, dir, currentRoot, module));
    }
  }


  ///////////////////////////////////////////////////////////////////////////
  /// Find checkouts
  ///

  /// Note:  this can be slow, because it examines every directory in your
  /// entire home directory.

  /**
   * Find checkouts.  These are indicated by directories named .bzr, CVS,
   * .hg, or .svn.
   * <p>
   *
   * With some version control systems, this task is easy:  there is
   * exactly one .bzr or .hg directory per checkout.  With CVS and SVN,
   * there is one CVS/.svn directory per directory of the checkout.  It is
   * permitted for one checkout to be made inside another one (though that
   * is bad style), so we must examine every CVS/.svn directory to find all
   * the distinct checkouts.
   */

  // An alternative implementation would use Files.walkFileTree, but that
  // is available only in Java 7.



  /** Accept only directories that are not symbolic links. */
  static class IsDirectoryFilter implements FileFilter {
    public boolean accept(File pathname) {
      try {
      return pathname.isDirectory()
        && pathname.getPath().equals(pathname.getCanonicalPath());
      } catch (IOException e) {
        throw new Error(e);
        // return false;
      }
    }
  }

  static IsDirectoryFilter idf = new IsDirectoryFilter();

  /** Find all checkouts under the given directory. */
  static Set<Checkout> findCheckouts(File dir) {
    assert dir.isDirectory();

    Set<Checkout> checkouts = new LinkedHashSet<Checkout>();

    findCheckouts(dir, checkouts);

    return checkouts;
  }


  /**
   * Find all checkouts at or under the given directory (or, as a special
   * case, also its parent -- could rewrite to avoid that case), and adds
   * them to checkouts.  Works by checking whether dir or any of its
   * descendants is a version control directory.
   */
  private static void findCheckouts(File dir, Set<Checkout> checkouts) {
    assert dir.isDirectory();

    String dirName = dir.getName().toString();
    File parent = dir.getParentFile();
    if (parent != null) {
      if (dirName.equals(".bzr")) {
        checkouts.add(new Checkout(RepoType.BZR, parent, null, null));
      } else if (dirName.equals("CVS")) {
        addCheckoutCvs(dir, parent, checkouts);
      } else if (dirName.equals(".hg")) {
        checkouts.add(dirToCheckoutHg(dir, parent));
      } else if (dirName.equals(".svn")) {
        checkouts.add(dirToCheckoutSvn(parent));
      }
    }

    @SuppressWarnings("nullness") // listFiles => non-null because dir is a directory
    File /*@NonNull*/ [] childdirs = dir.listFiles(idf);
    for (File childdir : childdirs) {
      findCheckouts(childdir, checkouts);
    }
  }


  /**
   * Given a directory named "CVS" , create a corresponding Checkout object
   * for its parent.  Returns null if this directory is named "CVS" but is
   * not a version control directory.  (Google Web Toolkit does that, for
   * example.)
   */
  static void addCheckoutCvs(File cvsDir, File dir, Set<Checkout> checkouts) {
    assert cvsDir.getName().toString().equals("CVS") : cvsDir.getName();
    // relative path within repository
    File repositoryFile = new File(cvsDir, "Repository");
    File rootFile = new File(cvsDir, "Root");
    if (! (repositoryFile.exists() && rootFile.exists())) {
      // apparently it wasn't a version control directory
      return;
    }
    String pathInRepo = UtilMDE.readFile(repositoryFile).trim();
    String repoRoot = UtilMDE.readFile(rootFile).trim();
    /*@NonNull*/ File repoFileRoot = new File(pathInRepo);
    while (repoFileRoot.getParentFile() != null) {
      @SuppressWarnings("nullness") // just checed that parent is non-null
      /*@NonNull*/ File newRepoFileRoot = repoFileRoot.getParentFile();
      repoFileRoot = newRepoFileRoot;
    }

    // strip common suffix off of local dir and repo url
    Pair<File,File> stripped = removeCommonSuffixDirs(dir,
                                                      new File(pathInRepo),
                                                      repoFileRoot,
                                                      "CVS");
    dir = stripped.a;
    String pathInRepoAtCheckout;
    if (stripped.b != null) {
      pathInRepoAtCheckout = stripped.b.toString();
    } else {
      pathInRepoAtCheckout = dir.getName();
    }

    checkouts.add(new Checkout(RepoType.CVS, dir, repoRoot, pathInRepoAtCheckout));
  }

  /**
   * Given a directory named ".hg" , create a corresponding Checkout object
   * for its parent.
   */
  static Checkout dirToCheckoutHg(File hgDir, File dir) {
    String repositoryRoot = null;

    File hgrcFile = new File(hgDir, "hgrc");
    Ini ini;
    try {
      ini = new Ini(new FileReader(hgrcFile));
    } catch (IOException e) {
      throw new Error("Problem reading file " + hgrcFile);
    }

    Ini.Section pathsSection = ini.get("paths");
    if (pathsSection != null) {
      repositoryRoot = pathsSection.get("default");
    }

    return new Checkout(RepoType.HG, dir, repositoryRoot, null);
  }


  /**
   * Given a directory that contains a .svn subdirectory, create a
   * corresponding Checkout object.
   */
  static Checkout dirToCheckoutSvn(File dir) {

    // For SVN, do
    //   svn info
    // and grep out these lines:
    //   URL: svn+ssh://login.csail.mit.edu/afs/csail/group/pag/projects/reCrash/repository/trunk/www
    //   Repository Root: svn+ssh://login.csail.mit.edu/afs/csail/group/pag/projects/reCrash/repository

    // Use SVNKit?
    // Con: introduces dependency on external library.
    // Pro: no need to re-implement or to call external process (which
    //   might be slow for large checkouts).

    @SuppressWarnings("nullness") // SVNKit is not yet annotated
    SVNWCClient wcClient = new SVNWCClient((/*@Nullable*/ ISVNAuthenticationManager) null, null);
    SVNInfo info;
    try {
      info = wcClient.doInfo(new File(dir.toString()), SVNRevision.WORKING);
    } catch (SVNException e) {
      throw new Error(e);
    }
    // getFile is null when operating on a working copy, as I am
    // String relativeFile = info.getPath(); // relative to repository root -- can use to determine root of checkout
    // getFile is just the (absolute) local file name for local items -- same as "dir"
    // File relativeFile = info.getFile();
    SVNURL url = info.getURL();
    SVNURL repoRoot = info.getRepositoryRootURL();
    if (debug) {
      System.out.println();
      System.out.println("repoRoot = " + repoRoot);
      System.out.println(" repoUrl = " + url);
      System.out.println("     dir = " + dir.toString());
    }

    // Strip common suffix off of local dir and repo url.
    Pair<File,File> stripped = removeCommonSuffixDirs(dir,
                                                      new File(url.getPath()),
                                                      new File(repoRoot.getPath()),
                                                      ".svn");
    dir = stripped.a;
    try {
      url = url.setPath(stripped.b.toString(), false);
    } catch (SVNException e) {
      throw new Error(e);
    }

    if (debug) {
      System.out.println("stripped: " + stripped);
      System.out.println("repoRoot = " + repoRoot);
      System.out.println(" repoUrl = " + url);
      System.out.println("     dir = " + dir.toString());
    }

    assert url.toString().startsWith(repoRoot.toString())
      : "repoRoot="+repoRoot+", url="+url;
    return new Checkout(RepoType.SVN, dir, url.toString(), null);

    /// Old implementation
    // String module = url.toString().substring(repoRoot.toString().length());
    // if (module.startsWith("/")) {
    //   module = module.substring(1);
    // }
    // if (module.equals("")) {
    //   module = null;
    // }
    // return new Checkout(RepoType.SVN, dir, repoRoot.toString(), module);



  }



  /**
   * Strip identical elements off the end of both paths, and then return
   * what is left of each.  Returned elements can be null!  If p2_limit is
   * non-null, then it should be a parent of p2, and the stripping stops
   * when p2 becomes p2_limit.  If p1_contains is non-null, then p1 must
   * contain a subdirectory of that name.
   */
  static Pair<File,File> removeCommonSuffixDirs(File p1, File p2, File p2_limit, String p1_contains) {
    if (debug) {
      System.out.printf("removeCommonSuffixDirs(%s, %s, %s, %s)%n", p1, p2, p2_limit, p1_contains);
    }
    // new names for results, because we will be side-effecting them
    /*@Nullable*/ File r1 = p1;
    /*@Nullable*/ File r2 = p2;
    while (r1 != null
           && r2 != null
           && (p2_limit == null || ! r2.equals(p2_limit))
           && r1.getName().equals(r2.getName())) {
      if (p1_contains != null
          && ! new File(r1.getParentFile(), p1_contains).isDirectory()) {
        break;
      }
      r1 = r1.getParentFile();
      r2 = r2.getParentFile();
    }
    if (debug) {
      System.out.printf("removeCommonSuffixDirs => %s %s%n", r1, r2);
    }
    return new Pair<File,File>(r1,r2);
  }


  ///////////////////////////////////////////////////////////////////////////
  /// Process checkouts
  ///

  public void process(Set<Checkout> checkouts) {
    String repo;

    for (Checkout c : checkouts) {
      System.out.println(c);
    }

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

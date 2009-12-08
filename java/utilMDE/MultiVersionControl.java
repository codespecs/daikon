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
 * This program, mvc for Multiple Version Control, lets you run a version
 * control command, such as "status" or "update", on a <b>set</b> of
 * Bzr/CVS/SVN/Hg checkouts rather than just one.<p>
 *
 * This program simplifies managing your checkouts.  You might
 * want to know whether any of them have uncommitted changes, or you
 * might want to update all of them.  Or, when setting up a new account,
 * you might want to check them all out.  This program does any of those
 * tasks.  In particular, it accepts these arguments:
 * <pre>
 *   checkout  -- checks out all repositories
 *   update    -- update all checked out repositories
 *   status    -- show files that are changed but not committed
 *   list      -- list the checkouts that this program is aware of
 * </pre>
 *
 * You can specify the set of checkouts for the program to manage, or it
 * can search your directory structure to find all of your checkouts, or
 * both.  A command that you can run right away to list un-committed
 * changed files is:
 * <pre>java utilMDE.MultiVersionControl status --search=true</pre>
 *
 * For complete usage information, run the program with no arguments.<p>
 *
 * The remainter of this document describes the file format for the
 * ".mvc-checkouts" file.<p>
 *
 * <b>File format for ".mvc-checkouts" file:</b><p>
 *
 * (Note:  because mvc can search for all checkouts in your directory, you
 * don't need a .mvc-checkouts file.  But using it makes the program faster
 * (it doesn't have to search your entire directory), or permits you to
 * exclude certain checkouts from processing.)<p>
 *
 * The ".mvc-checkouts" file contains a list of sections.  Each section names
 * either a root from which a sub-part (e.g., a module or a subdirectory)
 * will be checked out, or a repository all of which will be checked out.
 * Examples include:
 * <pre>
 * CVSROOT: :ext:login.csail.mit.edu:/afs/csail.mit.edu/u/m/mernst/.CVS/.CVS-mernst
 * SVNROOT: svn+ssh://tricycle.cs.washington.edu/cse/courses/cse403/09sp
 * REPOS: svn+ssh://login.csail.mit.edu/afs/csail/u/a/akiezun/.SVN/papers/parameterization-paper/trunk
 * HGREPOS: https://jsr308-langtools.googlecode.com/hg
 * </pre><p>
 *
 * Within each section is a list of directories that contain a checkout
 * from that repository.  If the section names a root, then a module or
 * subdirectory is needed.  By default, the directory's basename is used.
 * This can be overridden by specifying the module/subdirectory on the same
 * line, after a space.  If the section names a repository, then no module
 * information is needed or used.<p>
 *
 * Here are some example sections:
 * <pre>
 * CVSROOT: :ext:login.csail.mit.edu:/afs/csail.mit.edu/group/pag/projects/classify-tests/.CVS
 * ~/research/testing/symstra-eclat-paper
 * ~/research/testing/symstra-eclat-code
 * ~/research/testing/eclat
 *
 * SVNROOT: svn+ssh://login.csail.mit.edu/afs/csail/group/pag/projects/.SVNREPOS/
 * ~/research/typequals/igj
 * ~/research/typequals/annotations-papers
 *
 * SVNREPOS: svn+ssh://login.csail.mit.edu/afs/csail/group/pag/projects/abb/REPOS
 * ~/prof/grants/2008-06-abb/abb
 *
 * HGREPOS: https://checker-framework.googlecode.com/hg/
 * ~/research/types/checker-framework
 *
 * SVNROOT: svn+ssh://login.csail.mit.edu/afs/csail/u/d/dannydig/REPOS/
 * ~/research/concurrency/concurrentPaper
 * ~/research/concurrency/mit.edu.concurrencyRefactorings concurrencyRefactorings/project/mit.edu.concurrencyRefactorings
 * </pre>
 *
 * Furthermore, these sections have identical effects:
 * <pre>
 * SVNROOT: https://crashma.googlecode.com/svn/
 * ~/research/crashma trunk
 *
 * SVNREPOS: https://crashma.googlecode.com/svn/trunk
 * ~/research/crashma
 * </pre>
 * and, all 3 of these sections have identical effects:
 * <pre>
 * SVNROOT: svn+ssh://login.csail.mit.edu/afs/csail/group/pag/projects/
 * ~/research/typequals/annotations
 *
 * SVNROOT: svn+ssh://login.csail.mit.edu/afs/csail/group/pag/projects/
 * ~/research/typequals/annotations annotations
 *
 * SVNREPOS: svn+ssh://login.csail.mit.edu/afs/csail/group/pag/projects/annotations
 * ~/research/typequals/annotations
 * </pre><p>
 *
 * When performing a checkout, the parent directories are created if
 * needed.<p>
 *
 * In the file, blank lines, and lines beginning with "#", are ignored.<p>
 */

// TODO:

// It might be nice to list all the "unexpected" checkouts -- those found
// on disk that are not in the checkouts file.  This permits the checkouts
// file to be updated and then used in preference to searching the whole
// filesystem, which may be slow.
// You can do this from the command line by comparing the output of these
// two commands:
//   mvc list --repositories /dev/null | sort > checkouts-from-directory
//   mvc list --search=false | sort > checkouts-from-file
// but it might be nicer for the "list" command to do that explicitly.

// The "list" command should be in the .mvc-checkouts file format, rather
// than requiring the user to munge it.

// In checkouts file, use of space delimiter for specifyng module interacts
// badly with file names that contain spaces.  This doesn't seem important
// enough to fix.

// When discovering checkouts from a directory structure, there is a
// problem when two modules from the same SVN repository are checked out,
// with one checkout inside the other at the top level.  The inner
// checkout's directory can be mis-reported as the outer one.  This isn't
// always a problem for nested checkouts (so it's hard to reproduce), and
// nested checkouts are bad style anyway, so I am deferring
// investigating/fixing it.

// Add "incoming" command that shows you need to do update and/or fetch?
//
// For Mercurial, I can do "hg incoming", but how to show that the current
// working directory is not up to date with respect to the local
// repository?  "hg prompt" with the "update" tag will do the trick, see
// http://bitbucket.org/sjl/hg-prompt/src/ .  Or don't bother:  it's rarely an
// issue if you always update via "hg fetch" as done by this program.
//
// For svn, "svn status -u":
//   The out-of-date information appears in the ninth column (with -u):
//       '*' a newer revision exists on the server
//       ' ' the working copy is up to date



public class MultiVersionControl {

  @Option("File with list of checkouts.  Set it to /dev/null to suppress reading.")
  public String checkouts = new File(userHome, ".mvc-checkouts").getPath();

  @Option("Directory under which to search for checkouts; may be supplied multiple times; default=home dir")
  public List<String> dir = new ArrayList<String>();

  @Option("Directory under which to NOT search for checkouts; may be supplied multiple times")
  public List<String> ignore_dir = new ArrayList<String>();
  private List<File> ignoreDirs = new ArrayList<File>();

  // Default is false because searching whole directory structure is slow.
  @Option("Search for all checkouts, not just those listed in a file")
  public boolean search = false;

  // TODO: use consistent names: both "show" or both "print"

  @Option("Display commands as they are executed")
  public boolean show;

  @Option("Print the directory before executing commands")
  public boolean print_directory;

  @Option("Do not execute commands; just print them.  Implies --show --redo-existing")
  public boolean dry_run;

  /**  Default is for checkout command to skip existing directories. */
  @Option("Redo existing checkouts; relevant only to checkout command")
  public boolean redo_existing;

  @Option("Print debugging output")
  public static boolean debug;

  public static boolean debug_replacers = false;

  static enum Action {
    CHECKOUT,
      STATUS,
      UPDATE,
      LIST
      };
  // Shorter variants
  private static Action CHECKOUT = Action.CHECKOUT;
  private static Action STATUS = Action.STATUS;
  private static Action UPDATE = Action.UPDATE;
  private static Action LIST = Action.LIST;

  // Terminating the process can leave the repository in a bad state, so
  // set this rather high for safety.
  private static int TIMEOUT_SEC = 300;

  private Action action;

  @SuppressWarnings("nullness") // user.home property always exists
  static final /*@NonNull*/ String userHome = System.getProperty ("user.home");

  public static void main (String[] args) {
    setupSVNKIT();
    MultiVersionControl mvc = new MultiVersionControl(args);

    Set<Checkout> checkouts = new LinkedHashSet<Checkout>();

    try {
      readCheckouts(new File(mvc.checkouts), checkouts);
    } catch (IOException e) {
      System.err.println("Problem reading file " + mvc.checkouts + ": " + e.getMessage());
    }

    if (mvc.search) {
      // Postprocess command-line arguments
      for (String adir : mvc.ignore_dir) {
        File afile = new File(adir.replaceFirst("^~", userHome));
        if (! afile.isDirectory()) {
            System.err.printf("Warning: Directory to ignore while searching for checkouts is not a directory:%n  %s%n", adir);
        }
        mvc.ignoreDirs.add(afile);
      }

      for (String adir : mvc.dir) {
        adir = adir.replaceFirst("^~", userHome);
        if (debug) {
          System.out.println("Searching for checkouts under " + adir);
        }
        if (! new File(adir).isDirectory()) {
          System.err.printf("Directory in which to search for checkouts is not a directory: %s%n", adir);
          System.exit(2);
        }
        findCheckouts(new File(adir), checkouts, mvc.ignoreDirs);
      }
    }

    if (debug) {
      System.out.println("Processing checkouts read from " + checkouts);
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
    Options options = new Options ("mvc [options] {checkout,status,update,list}", this);
    String[] remaining_args = options.parse_or_usage (args);
    if (remaining_args.length != 1) {
      options.print_usage("Please supply exactly one argument (found %d)%n%s", remaining_args.length, UtilMDE.join(remaining_args, " "));
      System.exit(1);
    }
    String action_string = remaining_args[0];
    if ("checkout".startsWith(action_string)) {
      action = CHECKOUT;
    } else if ("status".startsWith(action_string)) {
      action = STATUS;
    } else if ("update".startsWith(action_string)) {
      action = UPDATE;
    } else if ("list".startsWith(action_string)) {
      action = LIST;
    } else {
      options.print_usage("Unrecognized action \"%s\"", action_string);
      System.exit(1);
    }

    // clean up options
    if (dir.isEmpty()) {
      dir.add(userHome);
    }

    if (dry_run) {
      show = true;
      redo_existing = true;
    }

    if (action == CHECKOUT) {
      search = false;
      show = true;
      // Checkouts can be much slower than other operations.
      TIMEOUT_SEC = TIMEOUT_SEC * 10;
    }
    if (action == UPDATE) {
      print_directory = true;
    }

    if (debug) {
      show = true;
    }

  }

  static enum RepoType {
    BZR,
    CVS,
    HG,
    SVN };

  // TODO: have subclasses of Checkout for the different varieties, perhaps.
  static class Checkout {
    RepoType repoType;
    /** Local directory */
    // actually the parent directory?
    File directory;
    /**
     * Non-null for CVS and SVN.
     * May be null for distributed version control systems (Bzr, Hg).
     * For distributed systems, refers to the parent repository from which
     * this was cloned, not the one here in this directory
     * <p>
     * Most operations don't need this.  it is needed for checkout, though.
     */
    /*@Nullable*/ String repository;
    /**
     * Null if no module, just whole thing.
     * Non-null for CVS and, optionally, for SVN.
     * Null for distributed version control systems (Bzr, Hg).
     */
    /*@Nullable*/ String module;


    Checkout(RepoType repoType, File directory) {
      this(repoType, directory, null, null);
    }

    Checkout(RepoType repoType, File directory, /*@Nullable*/ String repository, /*@Nullable*/ String module) {
      // Directory might not exist if we are running the checkout command.
      // If it exists, it must be a directory.
      assert (directory.exists() ? directory.isDirectory() : true)
        : "Not a directory: " + directory;
      this.repoType = repoType;
      this.directory = directory;
      this.repository = repository;
      this.module = module;
      // These asserts come at the end so that the error message can be better.
      switch (repoType) {
      case BZR:
        assertSubdirExists(directory, ".bzr");
        assert module == null;
        break;
      case CVS:
        assertSubdirExists(directory, "CVS");
        assert module != null : "No module for CVS checkout at: " + directory;
        break;
      case HG:
        assertSubdirExists(directory, ".hg");
        assert module == null;
        break;
      case SVN:
        assertSubdirExists(directory, ".svn");
        assert module == null;
        break;
      default:
        assert false;
      }
    }

    /** If the directory exists, then the subdirectory must exist too. */
    private void assertSubdirExists(File directory, String subdirName) {
      if (directory.exists()
          && ! new File(directory, subdirName).isDirectory()) {
        System.err.printf("Directory %s exists but %s subdirectory does not exist",
                          directory, subdirName);
        System.exit(2);
      }
    }


    @Override
    public boolean equals(/*@Nullable*/ Object other) {
      if (! (other instanceof Checkout))
        return false;
      Checkout c2 = (Checkout) other;
      return ((repoType == c2.repoType)
              && directory.equals(c2.directory)
              && ((repository == null)
                  ? (repository == c2.repository)
                  : repository.equals(c2.repository))
              && ((module == null)
                  ? (module == c2.module)
                  : module.equals(c2.module)));
    }

    @Override
    public int hashCode() {
      return (repoType.hashCode()
              + directory.hashCode()
              + (repository == null ? 0 : repository.hashCode())
              + (module == null ? 0 : module.hashCode()));
    }

    @Override
      public String toString() {
      return repoType
        + " " + directory
        + " " + repository
        + " " + module;
    }

  }


  ///////////////////////////////////////////////////////////////////////////
  /// Read checkouts from a file
  ///

  static void readCheckouts(File file, Set<Checkout> checkouts) throws IOException {
    RepoType currentType = RepoType.BZR; // arbitrary choice
    String currentRoot = null;
    boolean currentRootIsRepos = false;

    EntryReader er = new EntryReader(file);
    for (String line : er) {
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
        if (word1.equals("BZRROOT:") || word1.equals("BZRREPOS:")) {
          currentType = RepoType.BZR;
          currentRoot = word2;
          currentRootIsRepos = word1.equals("BZRREPOS:");
          continue;
        } else if (word1.equals("CVSROOT:")) {
          currentType = RepoType.CVS;
          currentRoot = word2;
          currentRootIsRepos = false;
          // If the CVSROOT is remote, try to make it local.
          if (currentRoot.startsWith(":ext:")) {
            String[] rootWords = currentRoot.split(":");
            String possibleRoot = rootWords[rootWords.length-1];
            if (new File(possibleRoot).isDirectory()) {
              currentRoot = possibleRoot;
            }
          }
          continue;
        } else if (word1.equals("HGROOT:") || word1.equals("HGREPOS:")) {
          currentType = RepoType.HG;
          currentRoot = word2;
          currentRootIsRepos = word1.equals("HGREPOS:");
          continue;
        } else if (word1.equals("SVNROOT:") || word1.equals("SVNREPOS:")) {
          currentType = RepoType.SVN;
          currentRoot = word2;
          currentRootIsRepos = word1.equals("SVNREPOS:");
          continue;
        }
      }

      if (currentRoot == null) {
        System.err.printf("need root before directory at line %d of file %s%n",
                          er.getLineNumber(), er.getFileName());
        System.exit(1);
      }

      // Replace "~" by "$HOME", because -d (and Athena's "cd" command) does not
      // understand ~, but it does understand $HOME.
      String dirname;
      String root = currentRoot;
      if (root.endsWith("/")) root = root.substring(0,root.length()-1);
      String module = null;

      int spacePos = line.lastIndexOf(' ');
      if (spacePos == -1) {
        dirname = line;
      } else {
        dirname = line.substring(0, spacePos);
        module = line.substring(spacePos+1);
      }

      // The directory may not yet exist if we are doing a checkout.
      File dir = new File(dirname.replaceFirst("^~", userHome));

      if (module == null) {
          module = dir.getName();
      }
      if (currentType != RepoType.CVS) {
        if (! currentRootIsRepos) {
          root = root + "/" + module;
        }
        module = null;
      }

      Checkout checkout = new Checkout(currentType, dir, root, module);
      checkouts.add(checkout);
    }
  }


  ///////////////////////////////////////////////////////////////////////////
  /// Find checkouts in a directory
  ///

  /// Note:  this can be slow, because it examines every directory in your
  /// entire home directory.

  // Find checkouts.  These are indicated by directories named .bzr, CVS,
  // .hg, or .svn.
  //
  // With some version control systems, this task is easy:  there is
  // exactly one .bzr or .hg directory per checkout.  With CVS and SVN,
  // there is one CVS/.svn directory per directory of the checkout.  It is
  // permitted for one checkout to be made inside another one (though that
  // is bad style), so we must examine every CVS/.svn directory to find all
  // the distinct checkouts.

  // An alternative implementation would use Files.walkFileTree, but that
  // is available only in Java 7.



//   /** Find all checkouts under the given directory. */
//   static Set<Checkout> findCheckouts(File dir) {
//     assert dir.isDirectory();
//
//     Set<Checkout> checkouts = new LinkedHashSet<Checkout>();
//
//     findCheckouts(dir, checkouts);
//
//     return checkouts;
//   }


  /**
   * Find all checkouts at or under the given directory (or, as a special
   * case, also its parent -- could rewrite to avoid that case), and adds
   * them to checkouts.  Works by checking whether dir or any of its
   * descendants is a version control directory.
   */
  private static void findCheckouts(File dir, Set<Checkout> checkouts, List<File> ignoreDirs) {
    assert dir.isDirectory();
    if (ignoreDirs.contains(dir)) {
      return;
    }

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
    if (childdirs == null) {
      System.err.printf("childdirs is null (permission or other I/O problem?) for %s%n", dir.toString());
      return;
    }
    for (File childdir : childdirs) {
      findCheckouts(childdir, checkouts, ignoreDirs);
    }
  }


  /** Accept only directories that are not symbolic links. */
  static class IsDirectoryFilter implements FileFilter {
    public boolean accept(File pathname) {
      try {
        return pathname.isDirectory()
          && pathname.getPath().equals(pathname.getCanonicalPath());
      } catch (IOException e) {
        System.err.printf("Exception in IsDirectoryFilter.accept(%s): %s%n", pathname, e);
        throw new Error(e);
        // return false;
      }
    }
  }

  static IsDirectoryFilter idf = new IsDirectoryFilter();


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
    Pair</*@Nullable*/ File, /*@Nullable*/ File> stripped
      = removeCommonSuffixDirs(dir, new File(pathInRepo),
                               repoFileRoot, "CVS");
    File cDir = stripped.a;
    if (cDir == null) {
      System.out.printf("dir (%s) is parent of path in repo (%s)",
                        dir, pathInRepo);
      System.exit(1);
    }
    String pathInRepoAtCheckout;
    if (stripped.b != null) {
      pathInRepoAtCheckout = stripped.b.toString();
    } else {
      pathInRepoAtCheckout = cDir.getName();
    }

    checkouts.add(new Checkout(RepoType.CVS, cDir, repoRoot, pathInRepoAtCheckout));
  }

  /**
   * Given a directory named ".hg" , create a corresponding Checkout object
   * for its parent.
   */
  static Checkout dirToCheckoutHg(File hgDir, File dir) {
    String repository = null;

    File hgrcFile = new File(hgDir, "hgrc");
    Ini ini;
    // There also exist Hg commands that will do this same thing.
    if (hgrcFile.exists()) {
      try {
        ini = new Ini(new FileReader(hgrcFile));
      } catch (IOException e) {
        throw new Error("Problem reading file " + hgrcFile);
      }

      Ini.Section pathsSection = ini.get("paths");
      if (pathsSection != null) {
        repository = pathsSection.get("default");
        if (repository != null && repository.endsWith("/")) {
          repository = repository.substring(0, repository.length()-1);
        }
      }
    }

    return new Checkout(RepoType.HG, dir, repository, null);
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
      throw new Error("Problem in dirToCheckoutSvn(" + dir + "): ", e);
    }
    // getFile is null when operating on a working copy, as I am
    // String relativeFile = info.getPath(); // relative to repository root -- can use to determine root of checkout
    // getFile is just the (absolute) local file name for local items -- same as "dir"
    // File relativeFile = info.getFile();
    SVNURL url = info.getURL();
    // This can be null (example: dir /afs/csail.mit.edu/u/m/mernst/.snapshot/class/6170/2006-spring/3dphysics).  I don't know under what circumstances.
    SVNURL repoRoot = info.getRepositoryRootURL();
    if (repoRoot == null) {
      System.err.println("Problem:  old svn working copy in " + dir.toString());
      System.err.println("Check it out again to get a 'Repository Root' entry in the svn info output.");
      System.err.println("  repoUrl = " + url);
      System.exit(2);
    }
    if (debug) {
      System.out.println();
      System.out.println("repoRoot = " + repoRoot);
      System.out.println(" repoUrl = " + url);
      System.out.println("     dir = " + dir.toString());
    }

    // Strip common suffix off of local dir and repo url.
    Pair</*@Nullable*/ File, /*@Nullable*/ File> stripped
      = removeCommonSuffixDirs(dir, new File(url.getPath()),
                               new File(repoRoot.getPath()), ".svn");
    File cDir = stripped.a;
    if (cDir == null) {
      System.out.printf("dir (%s) is parent of repository URL (%s)",
                         dir, url.getPath());
      System.exit(1);
    }
    if (stripped.b == null) {
      System.out.printf("dir (%s) is child of repository URL (%s)",
                        dir, url.getPath());
      System.exit(1);
    }
    String pathInRepoAtCheckout = stripped.b.toString();
    try {
      url = url.setPath(pathInRepoAtCheckout, false);
    } catch (SVNException e) {
      throw new Error(e);
    }

    if (debug) {
      System.out.println("stripped: " + stripped);
      System.out.println("repoRoot = " + repoRoot);
      System.out.println(" repoUrl = " + url);
      System.out.println("    cDir = " + cDir.toString());
    }

    assert url.toString().startsWith(repoRoot.toString())
      : "repoRoot="+repoRoot+", url="+url;
    return new Checkout(RepoType.SVN, cDir, url.toString(), null);

    /// Old implementation
    // String module = url.toString().substring(repoRoot.toString().length());
    // if (module.startsWith("/")) {
    //   module = module.substring(1);
    // }
    // if (module.equals("")) {
    //   module = null;
    // }
    // return new Checkout(RepoType.SVN, cDir, repoRoot.toString(), module);



  }

  /**
   * Strip identical elements off the end of both paths, and then return
   * what is left of each.  Returned elements can be null!  If p2_limit is
   * non-null, then it should be a parent of p2, and the stripping stops
   * when p2 becomes p2_limit.  If p1_contains is non-null, then p1 must
   * contain a subdirectory of that name.
   */
  static Pair</*@Nullable*/ File,/*@Nullable*/ File> removeCommonSuffixDirs(File p1, File p2, File p2_limit, String p1_contains) {
    if (debug) {
      System.out.printf("removeCommonSuffixDirs(%s, %s, %s, %s)%n", p1, p2, p2_limit, p1_contains);
    }
    // new names for results, because we will be side-effecting them
    File r1 = p1;
    File r2 = p2;
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
    return Pair.of(r1,r2);
  }


  ///////////////////////////////////////////////////////////////////////////
  /// Process checkouts
  ///

  private class Replacer {
    String regexp;
    String replacement;
    public Replacer(String regexp, String replacement) {
      this.regexp = regexp;
      this.replacement = replacement;
    }
    public String replaceAll(String s) {
      return s.replaceAll(regexp, replacement);
    }
  }


  public void process(Set<Checkout> checkouts) {
    String repo;

    ProcessBuilder pb = new ProcessBuilder("");
    // pb.redirectErrorStream(true);
    // pb.inheritIO(); //   This method (& functionality) only exists in Java 7!
    pb.redirectErrorStream(true);

    ProcessBuilder pb2;

    CHECKOUTLOOP:
    for (Checkout c : checkouts) {
      if (debug) {
        System.out.println(c);
      }
      File dir = c.directory;

      List<Replacer> replacers = new ArrayList<Replacer>();

      switch (c.repoType) {
      case BZR:
        break;
      case CVS:
        replacers.add(new Replacer("(^|\\n)([?]) ", "$1$2 " + dir + "/"));
        break;
      case HG:
        replacers.add(new Replacer("(^|\\n)(abort: .*)", "$1$2: " + dir));
        replacers.add(new Replacer("(^|\\n)([MARC!?I]) ", "$1$2 " + dir + "/"));
        replacers.add(new Replacer("(^|\\n)(\\*\\*\\* failed to import extension .*: No module named demandload\\n)", ""));
        break;
      case SVN:
        replacers.add(new Replacer("(svn: Network connection closed unexpectedly)", "$1 for " + dir));
        replacers.add(new Replacer("(svn: Repository) (UUID)", "$1 " + dir + " $2"));
        break;
      default:
        assert false;
      }
      // The \r* is necessary here; (somtimes?) there are two carriage returns.
      replacers.add(new Replacer("Warning: untrusted X11 forwarding setup failed: xauth key data not generated\r*\nWarning: No xauth data; using fake authentication data for X11 forwarding\\.\r*\n", ""));
      replacers.add(new Replacer("(working copy ')", "$1" + dir));

      pb2 = null;
      pb.command("echo", "command", "not", "set");
      pb.directory(dir);
      // Set pb.command() to be the command to be executed.
      switch (action) {
      case LIST:
        System.out.println(c);
        continue CHECKOUTLOOP;
      case CHECKOUT:
        pb.directory(dir.getParentFile());
        String dirbase = dir.getName();
        if (c.repository == null) {
          System.out.printf("Skipping checkout with unknown repository:%n  %s%n",
                            dir);
          continue CHECKOUTLOOP;
        }
        switch (c.repoType) {
        case BZR:
          throw new Error("not yet implemented");
          // break;
        case CVS:
          assert c.module != null : "@SuppressWarnings(nullness): dependent type CVS";
          pb.command("cvs", "-d", c.repository, "checkout",
                     "-P", // prune empty directories
                     "-ko", // no keyword substitution
                     c.module);
          break;
        case HG:
          pb.command("hg", "clone", c.repository, dirbase);
          break;
        case SVN:
          if (c.module != null) {
            pb.command("svn", "checkout", c.repository, c.module);
          } else {
            pb.command("svn", "checkout", c.repository);
          }
          break;
        default:
          assert false;
        }
        break;
      case STATUS:
        // I need a replacer for other version control systems, to add
        // directory names.
        switch (c.repoType) {
        case BZR:
          throw new Error("not yet implemented");
          // break;
        case CVS:
          assert c.repository != null;
          pb.command("cvs", "-q",
                     // Including "-d REPOS" seems to give errors when a
                     // subdirectory is in a different CVS repository.
                     // "-d", c.repository,
                     "diff",
                     "-b",      // compress whitespace
                     "--brief", // report only whether files differ, not details
                     "-N");     // report new files
          //         # For the last perl command, this also works:
          //         #   perl -p -e 'chomp(\$cwd = `pwd`); s/^Index: /\$cwd\\//'";
          //         # but the one we use is briefer and uses the abbreviated directory name.
          //         $filter = "grep -v \"unrecognized keyword 'UseNewInfoFmtStrings'\" | grep \"^Index:\" | perl -p -e 's|^Index: |$dir\\/|'";
          String removeRegexp
            = ("\n=+"
               + "\nRCS file: .*" // no trailing ,v for newly-created files
               + "(\nretrieving revision .*)?" // no output for newly-created files
               + "\ndiff .*"
               + "(\nFiles .* and .* differ)?" // no output if only whitespace differences
               );
          replacers.add(new Replacer(removeRegexp, ""));
          replacers.add(new Replacer("(^|\\n)Index: ", "$1" + dir + "/"));
          replacers.add(new Replacer("(^|\\n)(cvs \\[diff aborted)(\\]:)", "$1$2 in " + dir + "$3"));
          replacers.add(new Replacer("(^|\\n)(Permission denied)", "$1$2 in " + dir));
          replacers.add(new Replacer("(^|\\n)(cvs diff: cannot find )", "$1$2" + dir));
          replacers.add(new Replacer("(^|\\n)(cvs diff: in directory )", "$1$2" + dir + "/"));
          break;
        case HG:
          pb.command("hg", "status");
          pb2 = new ProcessBuilder("");
          pb2.redirectErrorStream(true);
          pb2.directory(pb.directory());
          pb2.command("hg", "outgoing", "-l", "1");
          // The third line is either "no changes found" or "changeset".
          replacers.add(new Replacer("^comparing with .*\\nsearching for changes\\nchangeset[^\001]*", "unpushed changesets: " + pb.directory() + "\n"));
          replacers.add(new Replacer("^\\n?comparing with .*\\nsearching for changes\\nno changes found\n", ""));
          break;
        case SVN:
          // This ignores columns other than the first two.
          // Handle column 1.
          replacers.add(new Replacer("(^|\\n)([ACDIMRX?!~])...... ", "$1$2 " + dir + "/"));
          // Handle column 2.
          replacers.add(new Replacer("(^|\\n).([CM])..... ", "$1$2 " + dir + "/"));
          pb.command("svn", "status");
          break;
        default:
          assert false;
        }
        break;
      case UPDATE:
        switch (c.repoType) {
        case BZR:
          throw new Error("not yet implemented");
          // break;
        case CVS:
          replacers.add(new Replacer("(^|\\n)(cvs update: ((in|skipping) directory|conflicts found in )) +", "$1$2 " + dir + "/"));
          replacers.add(new Replacer("(^|\\n)(Merging differences between 1.16 and 1.17 into )", "$1$2 " + dir + "/"));
          assert c.repository != null;
          pb.command("cvs",
                     // Including -d causes problems with CVS repositories
                     // that are embedded inside other repositories.
                     // "-d", c.repository,
                     "-Q", "update", "-d");
          //         $filter = "grep -v \"config: unrecognized keyword 'UseNewInfoFmtStrings'\"";
          replacers.add(new Replacer("(cvs update: move away )", "$1" + dir + "/"));
          replacers.add(new Replacer("(cvs \\[update aborted)(\\])", "$1 in " + dir + "$2"));
          break;
        case HG:
          replacers.add(new Replacer("(^|\\n)([?!AMR] ) +", "$1$2 " + dir + "/"));
          pb.command("hg", "-q", "fetch");
          break;
        case SVN:
          replacers.add(new Replacer("(^|\\n)([?!AMR] ) +", "$1$2 " + dir + "/"));
          assert c.repository != null;
          pb.command("svn", "-q", "update");
        //         $filter = "grep -v \"Killed by signal 15.\"";
          break;
        default:
          assert false;
        }
        break;
      default:
        assert false;
      }

      // Check that the directory exists (OK if it doesn't for checkout).
      if (debug) {
        System.out.println(dir + ":");
      }
      if (dir.exists()) {
        if (action == CHECKOUT && ! redo_existing) {
          System.out.println("Skipping checkout (dir already exists): " + dir);
          continue;
        }
      } else {
        // Directory does not exist
        File parent = dir.getParentFile();
        if (parent == null) {
          System.err.println("Root directory cannot be a checkout");
          System.exit(1);
        }
        switch (action) {
        case CHECKOUT:
          if (! parent.exists()) {
            if (show) {
              System.out.println("Directory does not exist"
                                 + (dry_run ? "" : " (creating)")
                                 + ": parent");
            }
            if (! dry_run) {
              if (! parent.mkdirs()) {
                System.err.println("Could not create directory: " + parent);
                System.exit(1);
              }
            }
          }
          break;
        case STATUS:
        case UPDATE:
          System.out.println("Cannot find directory: " + dir);
          continue CHECKOUTLOOP;
        case LIST:
        default:
          assert false;
        }
      }

      perform_command(pb, replacers);
      if (pb2 != null) perform_command(pb2, replacers);
    }
  }

  void perform_command(ProcessBuilder pb, List<Replacer> replacers) {

    if (show) {
      System.out.println(command(pb));
    }
    if (dry_run) {
      return;
    }
    try {
      // Perform the command

      // For debugging
      //  my $command_cwd_sanitized = $command_cwd;
      //  $command_cwd_sanitized =~ s/\//_/g;
      //  $tmpfile = "/tmp/cmd-output-$$-$command_cwd_sanitized";
      // my $command_redirected = "$command > $tmpfile 2>&1";
      TimeLimitProcess p = new TimeLimitProcess(pb.start(), TIMEOUT_SEC * 1000);
      p.waitFor();
      if (p.timed_out()) {
        System.out.printf("Timed out (limit: %ss):%n", TIMEOUT_SEC);
        System.out.println(command(pb));
        // Is it right to ignore output streams?
        return;
      }

      // Filter then print the output Sometimes, unpredictably this throws
      // an IOException "stream closed" from
      // java.io.BufferedInputStream.getBufIfOpen(BufferedInputStream.java:145)
      // .  I don't know why.  Re-running immediately gives fine results.
      String output = UtilMDE.readerContents(new BufferedReader(new InputStreamReader(p.getInputStream())));
      if (debug_replacers) { System.out.println("preoutput=<<<" + output + ">>>"); }
      for (Replacer r : replacers) {
        output = r.replaceAll(output);
        if (debug_replacers) { System.out.println("midoutput[" + r.regexp + "]=<<<" + output + ">>>"); }
      }
      if (debug_replacers) {
        System.out.println("postoutput=<<<" + output + ">>>");
        for (int i=0; i<Math.min(100,output.length()); i++) {
          System.out.println(i + ": " + (int) output.charAt(i) + "\n        \"" + output.charAt(i) + "\"");
        }
      }
      System.out.print(output);


    } catch (IOException e) {
      throw new Error(e);
    } catch (InterruptedException e) {
      throw new Error(e);
    }
  }


  String command(ProcessBuilder pb) {
    return "  cd " + pb.directory() + "\n"
      + "  " + UtilMDE.join(pb.command(), " ");
  }


//     # Show the command.
//     if ($show) {
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
//     if (! $dry_run) {
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
//         if ((($action eq "status") && ($result != 0) && ($result != 1))
//             || (($action ne "status") && ($result != 0))) {
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

  /**
   * A stream of newlines.  Used for processes that want input, when we
   * don't want to give them input but don't want them to simply hang. */
  static class StreamOfNewlines extends InputStream {
    public int read() {
      return (int) '\n';
    }
  }

//   static interface BufferedReaderFilter {
//     void process(Stream s);
//   }
//
//   public static class CvsDiffFilter implements BufferedReaderFilter {
//
//     BufferedReader reader;
//     String directory;
//
//     public CvsDiffFilter(BufferedReader reader, String directory) {
//       this.reader = reader;
//       this.directory = directory;
//     }
//
//     public void close() {
//       reader.close();
//     }
//
//     public void mark(int readAheadLimit) {
//       reader.mark(readAheadLimit);
//     }
//
//     public boolean markSupported() {
//       reader.markSupported();
//     }
//
//     public int read() {
//       throw new UnsupportedOperationException();
//       // reader.read();
//     }
//
//     public int read(char[] cbuf, int off, int len) {
//       throw new UnsupportedOperationException();
//       // reader.read(char[] cbuf, int off, int len);
//     }
//
//     public String readLine() {
//       String result = reader.readLine();
//       if (result == null) {
//         return result;
//       } else if (result.startsWith("Index: ")) {
//         return directory + result.substring(7);
//       } else {
//         return "";
//       }
//     }
//
//     public boolean ready() {
//       reader.ready();
//     }
//
//     public void reset() {
//       reader.reset();
//     }
//
//     public long skip(long n) {
//       reader.skip(n);
//     }
//
//   }

}

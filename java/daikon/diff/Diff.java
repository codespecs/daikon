package daikon.diff;

import daikon.inv.*;
import daikon.*;
import java.io.*;
import java.util.*;
import utilMDE.*;
import gnu.getopt.*;


/**
 * Diff is the main class for the invariant diff program.  The
 * invariant diff program outputs the differences between two sets of
 * invariants.
 *
 * The following is a high-level description of the program.  Each
 * input file contains a serialized PptMap.  The PptMap is extracted
 * from each file, and the two PptMaps are combined to form a tree.
 * The tree is exactly three levels deep.  The first level contains
 * the root, which holds no data.  Each node in the second level is a
 * pair of Ppts, and each node in the third level is a pair of
 * Invariants.  The tree is constructed by pairing the corresponding
 * Ppts and Invariants in the two PptMaps.  Finally, the tree is
 * traversed via the Visitor pattern to produce output.  The Visitor
 * pattern makes it easy to extend the program, simply by writing a
 * new Visitor.
 **/
public final class Diff {

  private static String usage =
    UtilMDE.join(new String[] {
      "Usage:",
      "    java daikon.diff.Diff [flags...] file1 [file2]",
      "  file1 and file2 are serialized invariants produced by Daikon.",
      "  If file2 is not specified, file1 is compared with the empty set.",
      "  For a list of flags, see the Daikon manual, which appears in the ",
      "  Daikon distribution and also at http://pag.lcs.mit.edu/daikon/."},
                 Global.lineSep);

  private boolean examineAllPpts;

    // added to disrupt the tree when bug hunting -LL
    private static boolean treeManip = false;

    // this is set only when the manip flag is set "-z"
    private static PptMap manip = null;

  /**
   * Determine which Ppts and Invariants should be paired together in
   * the tree.
   **/
  private static final Comparator PPT_COMPARATOR =
    new Ppt.NameComparator();
  private static final Comparator INV_COMPARATOR =
    new Invariant.ClassVarnameComparator();

  public Diff() {
    this(false);
  }

  public Diff(boolean examineAllPpts) {
    this.examineAllPpts = examineAllPpts;
  }

  /**
   * Read two PptMap objects from their respective files and diff them.
   **/
  public static void main(String[] args) throws FileNotFoundException,
  StreamCorruptedException, OptionalDataException, IOException,
  ClassNotFoundException {

    boolean printDiff = false;
    boolean printAll = false;
    boolean stats = false;
    boolean tabSeparatedStats = false;
    boolean examineAllPpts = false;
    boolean printEmptyPpts = false;
    boolean verbose = false;
    boolean continuousJustification = false;
    boolean logging = false;



    boolean optionSelected = false;

    daikon.Logger.setupLogs (daikon.Logger.INFO);

    Getopt g = new Getopt("daikon.diff.Diff", args, "hdastjpevlz");
    int c;
    while ((c = g.getopt()) !=-1) {
      switch (c) {
      case 'h':
        System.out.println(usage);
        System.exit(1);
        break;
      case 'd':
        optionSelected = true;
        printDiff = true;
        break;
      case 'a':
        optionSelected = true;
        printAll = true;
        break;
      case 's':
        optionSelected = true;
        stats = true;
        break;
      case 't':
        optionSelected = true;
        tabSeparatedStats = true;
        break;
      case 'j':
        continuousJustification = true;
        break;
      case 'p':
        examineAllPpts = true;
        break;
      case 'e':
        printEmptyPpts = true;
        break;
      case 'z':
	treeManip = true;
	break;
      case 'v':
        verbose = true;
        break;
      case 'l':
        logging = true;
        break;
      case '?':
        break; // getopt() already printed an error
      default:
        System.out.println("getopt() returned " + c);
        break;
      }
    }

    // Turn on the defaults
    if (! optionSelected) {
      printDiff = true;
    }

    if (logging)
      System.err.println("Invariant Diff: Starting Log");

    if (logging)
      System.err.println("Invariant Diff: Creating Diff Object");

    Diff diff = new Diff(examineAllPpts);

    // The index of the first non-option argument -- the name of the
    // first file
    int firstFileIndex = g.getOptind();
    int numFiles = args.length - firstFileIndex;

    PptMap map1 = null;
    PptMap map2 = null;

    if (logging)
      System.err.println("Invariant Diff: Reading Files");

    try {
      if (numFiles == 1) {
	String filename1 = args[firstFileIndex];
	map1 = FileIO.read_serialized_pptmap(new File(filename1),
					     false // use saved config
					     );
	map2 = new PptMap();
      } else if (numFiles == 2) {
	String filename1 = args[firstFileIndex];
	String filename2 = args[firstFileIndex + 1];
	map1 = FileIO.read_serialized_pptmap(new File(filename1),
					     false // use saved config
					     );
	map2 = FileIO.read_serialized_pptmap(new File(filename2),
					     false // use saved config
					     );
      }

      else if (treeManip) {
	  System.out.println ("Warning, the postSplit file must be second");
	  if (numFiles < 3) {
	      System.out.println
		  ("Sorry, no manip file [preSplit] [postSplit] [manip]");
	  }
	  String filename1 = args[firstFileIndex];
	  String filename2 = args[firstFileIndex + 1];
	  String manipFile = args[firstFileIndex + 2];
	  map1 = FileIO.read_serialized_pptmap(new File(filename1),
					       false // use saved config
					       );
	  map2 = FileIO.read_serialized_pptmap(new File(filename2),
					       false // use saved config
					       );
	  manip = FileIO.read_serialized_pptmap(new File(manipFile),
					       false // use saved config
					       );
	  // form the root with tree manips
	  RootNode root = diff.diffPptMap (map1, map2);

	  // now run the stats visitor for checking matches
	  MatchCountVisitor mcv = new MatchCountVisitor
	      (System.out, verbose, false);
	  root.accept (mcv);
	  System.out.println ("Precison: " + mcv.calcPrecision());
	  System.out.println ("Success");
	  System.exit(0);

      }

      else if (numFiles > 2) {

	  // The new stuff that allows multiple files -LL


	  PptMap[] mapAr = new PptMap[numFiles];
	int j = 0;
	for (int i = firstFileIndex; i < args.length; i++) {
	    String fileName = args[i];
	    mapAr[j++] = FileIO.read_serialized_pptmap(new File (fileName),
						       false);
	}

	// Cascade a lot of the different invariants into one map,
	// and then put them into map1, map2

	// Initialize it all
	RootNode root = null;
	MultiDiffVisitor v1 = new MultiDiffVisitor (mapAr[0]);

	for (int i = 1; i < mapAr.length; i++) {
	    root = diff.diffPptMap (mapAr[i], v1.currMap);
	    root.accept (v1);
	}

	// now take the final result for the MultiDiffVisitor
	// and use it along side a null empty map
	map1 = v1.currMap;
	map2 = new PptMap();

	v1.printAll();
	return;
      }
      else {
	  System.out.println (usage);
	  System.exit(0);
      }

    } catch (IOException e) {
      throw new RuntimeException("Could not load .inv file: " + e);
    }

    if (logging)
      System.err.println("Invariant Diff: Creating Tree");

    RootNode root = diff.diffPptMap(map1, map2);

    if (logging)
      System.err.println("Invariant Diff: Visiting Tree");

    if (stats) {
      DetailedStatisticsVisitor v =
        new DetailedStatisticsVisitor(continuousJustification);
      root.accept(v);
      System.out.print(v.format());
    }

    if (tabSeparatedStats) {
      DetailedStatisticsVisitor v =
        new DetailedStatisticsVisitor(continuousJustification);
      root.accept(v);
      System.out.print(v.repr());
    }

    if (printDiff) {
      PrintDifferingInvariantsVisitor v = new PrintDifferingInvariantsVisitor
        (System.out, verbose, printEmptyPpts);
      root.accept(v);
    }

    if (printAll) {
      PrintAllVisitor v = new PrintAllVisitor
        (System.out, verbose, printEmptyPpts);
      root.accept(v);
    }

    if (logging)
      System.err.println("Invariant Diff: Ending Log");

    System.exit(0);
  }


  /**
   * Returns a tree of corresponding program points, and corresponding
   * invariants at each program point.  This tree can be walked to
   * determine differences between the sets of invariants.
   **/
  public RootNode diffPptMap(PptMap map1, PptMap map2) {
    RootNode root = new RootNode();

    SortedSet sset1 = new TreeSet(PPT_COMPARATOR);
    sset1.addAll(map1.asCollection());
    List l1 = new ArrayList(sset1);
    SortedSet sset2 = new TreeSet(PPT_COMPARATOR);
    sset2.addAll(map2.asCollection());
    List l2 = new ArrayList(sset2);

    if (examineAllPpts) {
      addConditionalPpts(l1);
      addConditionalPpts(l2);
    }

    Iterator opi = new OrderedPairIterator(l1.iterator(), l2.iterator(),
                                           PPT_COMPARATOR);
    while (opi.hasNext()) {
      Pair ppts = (Pair) opi.next();
      PptTopLevel ppt1 = (PptTopLevel) ppts.a;
      PptTopLevel ppt2 = (PptTopLevel) ppts.b;
      if (shouldAdd(ppt1) || shouldAdd(ppt2)) {
        PptNode node = diffPptTopLevel(ppt1, ppt2);
        root.add(node);
      }
    }

    return root;
  }

  /**
   * For each Ppt in the list, add its conditional program points
   * immediately after itself.
   **/
  private void addConditionalPpts(List ppts) {
    // ListIterator allows us to insert elements while we are iterating
    ListIterator pptsIter = ppts.listIterator();
    while (pptsIter.hasNext()) {
      PptTopLevel ppt = (PptTopLevel) pptsIter.next();
      Iterator condIter = ppt.views_cond.iterator();
      while (condIter.hasNext()) {
	PptConditional pptCond = (PptConditional) condIter.next();
	pptsIter.add(pptCond);
      }
    }
  }

  /**
   * Returns true if the program point should be added to the tree,
   * false otherwise.
   **/
  private boolean shouldAdd(PptTopLevel ppt) {
    if (examineAllPpts) {
      return true;
    } else {
      if (ppt == null) {
        return false;
      } else if (ppt.ppt_name.isEnterPoint()) {
        return true;
      } else if (ppt.ppt_name.isCombinedExitPoint()) {
        return true;
      } else {
        return false;
      }
    }
  }

  /**
   * Takes a pair of corresponding top-level program points, and
   * returns a tree of the corresponding invariants.  Either of the
   * program points may be null.
   **/
  private PptNode diffPptTopLevel(PptTopLevel ppt1, PptTopLevel ppt2) {
    PptNode pptNode = new PptNode(ppt1, ppt2);

    Assert.assert(ppt1 == null || ppt2 == null ||
                  PPT_COMPARATOR.compare(ppt1, ppt2) == 0,
                  "Program points do not correspond");

    List invs1;
    if (ppt1 != null) {
      invs1 = ppt1.invariants_vector();
      Collections.sort(invs1, INV_COMPARATOR);
    } else {
      invs1 = Collections.EMPTY_LIST;
    }

    List invs2;
    if (ppt2 != null) {
      invs2 = ppt2.invariants_vector();
      Collections.sort(invs2, INV_COMPARATOR);
    } else {

	if ( treeManip && isCond (ppt1)) {

	    invs2 = findCondPpt (manip, ppt1);

	}
	else invs2 = Collections.EMPTY_LIST;
    }

    Iterator opi = new OrderedPairIterator(invs1.iterator(), invs2.iterator(),
                                           INV_COMPARATOR);
    while (opi.hasNext()) {
      Pair invariants = (Pair) opi.next();
      Invariant inv1 = (Invariant) invariants.a;
      Invariant inv2 = (Invariant) invariants.b;
      InvNode invNode = new InvNode(inv1, inv2);
      pptNode.add(invNode);
    }

    return pptNode;
  }

    private boolean isCond (PptTopLevel ppt) {

	boolean ret =  (ppt instanceof PptConditional);

	return ret;


    }

    private List findCondPpt (PptMap manip, PptTopLevel ppt) {
	// targetName should look like this below
	// Contest.smallestRoom(II)I:::EXIT9;condition="max < num
	String targetName = ppt.name;

	String targ = targetName.substring (0, targetName.indexOf(';'));

	for ( Iterator i = manip.nameStringSet().iterator(); i.hasNext();) {
	    String somePptName = (String) i.next();
	    // A conditional Ppt always contains the normal Ppt
	    if (targ.equals (somePptName)) {
		PptTopLevel repl = manip.get (somePptName);
		System.out.println (targetName + "\n" + somePptName);
		return repl.invariants_vector();
	    }
	    else {

	    }
	}
	System.out.println ("Oh no!!!");
	return Collections.EMPTY_LIST;
    }

}




package daikon.diff;

import daikon.inv.*;
import daikon.*;
import java.io.*;
import java.util.*;
import utilMDE.*;
import gnu.getopt.*;

// Notes on options
//
// -j: Normally, the justification of invariants is taken into
// account.  This option turns this off.  When justification is taken
// into account, invariants inv1 and inv2 are "the same" if
// inv1.isSameInvariant(inv2) returns true, and are both justified.  A
// difference is worth printing if at least one invariant is
// justified.


public final class Diff {
  public static final String lineSep = Global.lineSep;

  private static String usage =
    "Usage: java daikon.diff.Diff [OPTION]... FILE1 [FILE2]" +
    lineSep +
    "  If FILE2 is not specified, FILE1 is compared with an empty set" +
    lineSep +
    "  -h  Display this usage message" +
    lineSep +
    "  -d  Display the tree of differing invariants (default)" +
    lineSep +
    "  -a  Display the tree of all invariants" +
    lineSep +
    "  -s  Display the statistics between two sets of invariants (default)" +
    lineSep +
    "  -u  Display uninteresting invariants" +
    lineSep +
    "  -j  Ignore justification when displaying differing invariants" +
    lineSep +
    "  -v  Verbose output" +
    lineSep;


  // Types of output
  private static boolean printDiff = false;
  private static boolean printAll = false;
  private static boolean stats = false;
  private static boolean printUninteresting = false;
  private static boolean considerJustification  = true;
  private static boolean verbose = false;

  private static boolean optionSelected = false;

  /** Read two PptMap objects from their respective files and diff them. */
  public static void main(String[] args) throws FileNotFoundException,
  StreamCorruptedException, OptionalDataException, IOException,
  ClassNotFoundException {

    Getopt g = new Getopt("daikon.diff.Diff", args, "hdasujv");
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
      case 'u':
        printUninteresting = true;
        break;
      case 'j':
        considerJustification = false;
        break;
      case 'v':
        verbose = true;
        break;
      case '?':
        break; // getopt() already printed an error
      default:
        System.out.println("getopt() returned " + c);
        break;
      }
    }

    // stats, printDiff is the default mode
    if (! optionSelected) {
      stats = true;
      printDiff = true;
    }

    // The index of the first non-option argument -- the name of the
    // first file
    int firstFileIndex = g.getOptind();
    int numFiles = args.length - firstFileIndex;

    PptMap map1 = null;
    PptMap map2 = null;

    if (numFiles == 1) {
      String filename1 = args[firstFileIndex];
      map1 = FileIO.read_invariant_file(filename1);
      map2 = new PptMap();
    } else if (numFiles == 2) {
      String filename1 = args[firstFileIndex];
      String filename2 = args[firstFileIndex + 1];
      map1 = FileIO.read_invariant_file(filename1);
      map2 = FileIO.read_invariant_file(filename2);
    } else {
      System.out.println(usage);
      System.exit(1);
    }

    RootNode root = diffPptMap(map1, map2);

    if (stats) {
      DetailedStatisticsVisitor v = new DetailedStatisticsVisitor();
      root.accept(v);
      System.out.println(v.format());
    }
    
    if (printDiff) {
      PrintDifferingInvariantsVisitor v = new PrintDifferingInvariantsVisitor
        (System.out, verbose, printUninteresting, considerJustification);
      root.accept(v);
    }
    
    if (printAll) {
      PrintAllVisitor v = new PrintAllVisitor(System.out, verbose);
      root.accept(v);
    }
    
  }


  // Returns a tree of corresponding program points, and corresponding
  // invariants at each program point.  This tree can be walked to
  // determine differences between the sets of invaraints.
  public static RootNode diffPptMap(PptMap map1, PptMap map2) {
    RootNode root = new RootNode();

    Comparator comparator = new Ppt.NameComparator();

    SortedSet sset1 = new TreeSet(comparator);
    sset1.addAll(map1.asCollection());
    SortedSet sset2 = new TreeSet(comparator);
    sset2.addAll(map2.asCollection());

    Iterator opi = new OrderedPairIterator(sset1.iterator(), sset2.iterator(),
                                           comparator);
    while(opi.hasNext()) {
      Pair ppts = (Pair) opi.next();
      PptTopLevel ppt1 = (PptTopLevel) ppts.a;
      PptTopLevel ppt2 = (PptTopLevel) ppts.b;
      PptNode node = diffPptTopLevel(ppt1, ppt2);
      root.add(node);
    }

    return root;
  }


  // Takes a pair of corresponding top-level program points, and
  // returns a tree of the corresponding invariants.  Either of the
  // program points may be null.
  private static PptNode diffPptTopLevel(PptTopLevel ppt1, PptTopLevel ppt2) {
    PptNode pptNode = new PptNode(ppt1, ppt2);

    Comparator pptComparator = new Ppt.NameComparator();
    Assert.assert(ppt1 == null || ppt2 == null ||
                  pptComparator.compare(ppt1, ppt2) == 0,
                  "Program points do not correspond");

    Comparator invComparator = new Invariant.ClassVarnameComparator();
    List invs1;
    if (ppt1 != null) {
      invs1 = ppt1.invariants_vector();
      Collections.sort(invs1, invComparator);
    } else {
      invs1 = Collections.EMPTY_LIST;
    }

    List invs2;
    if (ppt2 != null) {
      invs2 = ppt2.invariants_vector();
      Collections.sort(invs2, invComparator);
    } else {
      invs2 = Collections.EMPTY_LIST;
    }

    Iterator opi = new OrderedPairIterator(invs1.iterator(), invs2.iterator(),
                                           invComparator);
    while(opi.hasNext()) {
      Pair invariants = (Pair) opi.next();
      Invariant inv1 = (Invariant) invariants.a;
      Invariant inv2 = (Invariant) invariants.b;
      InvNode invNode = new InvNode(inv1, inv2);
      pptNode.add(invNode);
    }

    return pptNode;
  }

}

package daikon.diff;

import daikon.inv.*;
import daikon.*;
import java.io.*;
import java.util.*;
import utilMDE.*;
import gnu.getopt.*;

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
    "  -s  Display the statistics between two sets of invariants" +
    lineSep +
    "  -t  Display the statistics as a tab-separated list" +
    lineSep +
    "  -j  Treat justification as a continuous value" +
    lineSep +
    "  -p  Examine all program points" +
    lineSep +
    "  -v  Verbose output" +
    lineSep;

  private boolean examineAllPpts;

  public Diff() {
    this(false);
  }

  public Diff(boolean examineAllPpts) {
    this.examineAllPpts = examineAllPpts;
  }

  /** Read two PptMap objects from their respective files and diff them. */
  public static void main(String[] args) throws FileNotFoundException,
  StreamCorruptedException, OptionalDataException, IOException,
  ClassNotFoundException {

    boolean printDiff = false;
    boolean printAll = false;
    boolean stats = false;
    boolean tabSeparatedStats = false;
    boolean examineAllPpts = false;
    boolean verbose = false;
    boolean continuousJustification = false;

    boolean optionSelected = false;

    Getopt g = new Getopt("daikon.diff.Diff", args, "hdastjpv");
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

    // Turn on the defaults
    if (! optionSelected) {
      printDiff = true;
    }

    Diff diff = new Diff(examineAllPpts);

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

    RootNode root = diff.diffPptMap(map1, map2);

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
      PrintDifferingInvariantsVisitor v =
        new PrintDifferingInvariantsVisitor(System.out, verbose);
      root.accept(v);
    }
    
    if (printAll) {
      PrintAllVisitor v = new PrintAllVisitor(System.out, verbose);
      root.accept(v);
    }
    
  }


  // Returns a tree of corresponding program points, and corresponding
  // invariants at each program point.  This tree can be walked to
  // determine differences between the sets of invariants.
  public RootNode diffPptMap(PptMap map1, PptMap map2) {
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
      if (shouldAdd(ppt1) || shouldAdd(ppt2)) {
        PptNode node = diffPptTopLevel(ppt1, ppt2);
        root.add(node);
      }
    }

    return root;
  }

  private boolean shouldAdd(PptTopLevel ppt) {
    if (examineAllPpts) {
      return true;
    } else {
      if (ppt == null) {
        return false;
      } else if (ppt.ppt_name.isEnterPoint()) {
        return true;
      } else if (ppt.ppt_name.isExitPoint() && ppt.combined_exit == null) {
        return true;
      } else {
        return false;
      }
    }
  }


  // Takes a pair of corresponding top-level program points, and
  // returns a tree of the corresponding invariants.  Either of the
  // program points may be null.
  private PptNode diffPptTopLevel(PptTopLevel ppt1, PptTopLevel ppt2) {
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

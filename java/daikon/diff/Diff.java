package daikon.diff;

import daikon.inv.*;
import daikon.*;
import java.io.*;
import java.util.*;
import java.util.zip.GZIPInputStream;
import javax.swing.tree.*;
import utilMDE.*;
import gnu.getopt.*;

public final class Diff {
  public static final String lineSep = Global.lineSep;

  private static String usage =
    "Usage: java daikon.Diff [OPTION]... FILE1 FILE2" +
    lineSep +
    "  -h  Display this usage message" +
    lineSep +
    "  -d  Display the tree of differing invariants (default)" +
    lineSep +
    "  -a  Display the tree of all invariants" +
    lineSep +
    "  -s  Display the statistics between two sets of invariants (default)" +
    lineSep +
    "  -v  Verbose output" +
    lineSep;


  // Types of output
  private static boolean print_diff = false;
  private static boolean print_all = false;
  private static boolean stats = false;
  private static boolean verbose = false;

  private static boolean option_selected = false;

  /** Read two PptMap objects from their respective files and diff them. */
  public static void main(String[] args) throws FileNotFoundException,
  StreamCorruptedException, OptionalDataException, IOException,
  ClassNotFoundException {

    Getopt g = new Getopt("daikon.Diff", args, "hdasv");
    int c;
    while ((c = g.getopt()) !=-1) {
      option_selected = true;
      switch (c) {
      case 'h':
        System.out.println(usage);
        System.exit(1);
        break;
      case 'd':
        print_diff = true;
        break;
      case 'a':
        print_all = true;
        break;
      case 's':
        stats = true;
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

    // stats, print_diff is the default mode
    if (! option_selected) {
      stats = true;
      print_diff = true;
    }
    // The index of the first non-option argument -- the name of the
    // first file
    int firstFileIndex = g.getOptind();
    if (args.length - firstFileIndex != 2) {
        System.out.println(usage);
        System.exit(1);
    }
    String filename1 = args[firstFileIndex];
    String filename2 = args[firstFileIndex + 1];

    InputStream istream1 = new FileInputStream(filename1);
    if (filename1.endsWith(".gz")) {
      istream1 = new GZIPInputStream(istream1);
    }
    ObjectInputStream oistream1 = new ObjectInputStream(istream1);
    InputStream istream2 = new FileInputStream(filename2);
    if (filename2.endsWith(".gz")) {
      istream2 = new GZIPInputStream(istream2);
    }
    ObjectInputStream oistream2 = new ObjectInputStream(istream2);

    PptMap map1 = (PptMap) oistream1.readObject();
    PptMap map2 = (PptMap) oistream2.readObject();

    RootNode root = diffPptMap(map1, map2);

    if (stats) {
      StatisticsVisitor v = new StatisticsVisitor();
      root.accept(v);
      System.out.println();
      System.out.println(v.format());
    }

    if (print_diff) {
      PrintDifferingInvariantsVisitor v =
        new PrintDifferingInvariantsVisitor(verbose);
      root.accept(v);
      System.out.println(v.getOutput());
    }

    if (print_all) {
      PrintAllVisitor v = new PrintAllVisitor(verbose);
      root.accept(v);
      System.out.println(v.getOutput());
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

package daikon.diff;

import daikon.inv.*;
import daikon.*;
import java.io.*;
import java.util.*;
import javax.swing.tree.*;
import utilMDE.*;
import gnu.getopt.*;

public final class Diff {
  public static final String lineSep = Global.lineSep;

  private static String usage = 
    "Usage: java daikon.Diff [OPTION]... FILE1 FILE2" + lineSep + lineSep +
    "  -h  Display this usage message" + lineSep +
    "  -d  Display the tree of differing invariants (default)" + lineSep +
    "  -a  Display the tree of all invariants" + lineSep +
    "  -c  Display the number of differing invariants" + lineSep +
    "  -s  Display the statistics between two sets of invariants" + lineSep
    ;


  // The output mode selected by the user
  private static final int PRINT_DIFF_INV = 0;
  private static final int PRINT_ALL_INV = 1;
  private static final int COUNT_DIFF_INV = 2;
  private static final int STATS_INV = 3;
  private static int mode = PRINT_DIFF_INV;

  /** Read two PptMap objects from their respective files and diff them. */
  public static void main(String[] args) throws FileNotFoundException,
  StreamCorruptedException, OptionalDataException, IOException,
  ClassNotFoundException {

    Getopt g = new Getopt("daikon.Diff", args, "hdacs");
    int c;
    while ((c = g.getopt()) !=-1) {
      switch (c) {
      case 'h':
        System.out.println(usage);
        System.exit(1);
        break;
      case 'd':
        mode = PRINT_DIFF_INV;
        break;
      case 'a':
        mode = PRINT_ALL_INV;
        break;
      case 'c':
        mode = COUNT_DIFF_INV;
        break;
      case 's':
        mode = STATS_INV;
        break;
      case '?':
        break; // getopt() already printed an error
      default:
        System.out.println("getopt() returned " + c);
        break;
      }
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
    
    FileInputStream istream1 = new FileInputStream(filename1);
    ObjectInputStream oistream1 = new ObjectInputStream(istream1);
    FileInputStream istream2 = new FileInputStream(filename2);
    ObjectInputStream oistream2 = new ObjectInputStream(istream2);

    PptMap map1 = (PptMap) oistream1.readObject();
    PptMap map2 = (PptMap) oistream2.readObject();

    RootNode root = diffPptMap(map1, map2);

    switch (mode) {
    case PRINT_DIFF_INV:
      {
        PrintDifferingInvariantsVisitor v =
          new PrintDifferingInvariantsVisitor();
        root.accept(v);
        System.out.println(v.getOutput());
      }
      break;
    case PRINT_ALL_INV:
      {
        PrintAllVisitor v = new PrintAllVisitor();
        root.accept(v);
        System.out.println(v.getOutput());
      }
      break;
    case COUNT_DIFF_INV:
      {
        CountDifferingInvariantsVisitor v =
          new CountDifferingInvariantsVisitor();
        root.accept(v);
        int differingInvariants = v.getDifferingInvariantsCount();
        System.out.println("There are " + differingInvariants +
                           " differing invariants.");
      }
      break;
    case STATS_INV:
      {
        StatisticsVisitor v = new StatisticsVisitor();
        root.accept(v);
        System.out.println("Invariant similarities and differences of " +
                           filename1 + " vs. " + filename2);
        System.out.println(v.format());
      }      
      break;
    default:
      Assert.assert(false, "Can't get here");
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



  // This has been moved to the Visitors in the package daikon.diff.
  
  /*
  // Prints all nodes in the tree, except nodes with the same
  // invariants.
  public static String printDifferences(DefaultMutableTreeNode root) {
    StringWriter sw = new StringWriter();
    PrintWriter pw = new PrintWriter(sw);
    for(Enumeration e = root.preorderEnumeration(); e.hasMoreElements(); ) {
      DefaultMutableTreeNode node = (DefaultMutableTreeNode) e.nextElement();
      Object o = node.getUserObject();
      if (o instanceof Pair) {
        Pair p = (Pair) o;
        if (! sameInvariant(p)) {
          pw.print(printIndent(node));
          pw.println(printTreePair(p));
        }
      } else {
        pw.print(printIndent(node));
        pw.println(o);
      }
    }   
    return sw.toString();
  }

  // Returns true if each member of the pair is an invariant, and the
  // invariants are "the same"
  private static boolean sameInvariant(Pair p) {
    if (p.a instanceof Invariant && p.b instanceof Invariant) {
      Invariant inva = (Invariant) p.a;
      Invariant invb = (Invariant) p.b;
      return inva.isSameInvariant(invb);
    }
    return false;
  }

  // Prints a tree generated by diffing two PptMaps
  public static String printTree(DefaultMutableTreeNode root) {
    StringWriter sw = new StringWriter();
    PrintWriter pw = new PrintWriter(sw);
    for(Enumeration e = root.preorderEnumeration(); e.hasMoreElements(); ) {
      DefaultMutableTreeNode node = (DefaultMutableTreeNode) e.nextElement();
      pw.print(printIndent(node));
      Object o = node.getUserObject();
      if (o instanceof Pair) {
	pw.println(printTreePair((Pair) o));
      } else {
	pw.println(o);
      }
    }   
    
    return sw.toString();
  }

  private static String printIndent(DefaultMutableTreeNode node) {
    StringBuffer sb = new StringBuffer();
    for (int i = 0; i < node.getLevel(); i++) {
      sb.append("  ");
    }
    return sb.toString();
  }

  private static String printTreePair(Pair p) {
    StringBuffer sb = new StringBuffer();
    sb.append("<");
    
    if (p.a instanceof PptTopLevel) {
      sb.append(((PptTopLevel) p.a).name);
    } else if (p.a instanceof Invariant) {
      sb.append(printInvariant((Invariant) p.a));
    } else {
      sb.append(p.a);
    }
    
    sb.append(", ");
    
    if (p.b instanceof PptTopLevel) {
      sb.append(((PptTopLevel) p.b).name);
    } else if (p.b instanceof Invariant) {
      sb.append(printInvariant((Invariant) p.b));
    } else {
      sb.append(p.b);
    }
    
    sb.append(">");
    return sb.toString();
  }


  private static String printInvariant(Invariant inv) {
    return inv.repr_prob();
  }

  public static int countDifferingInvariants(DefaultMutableTreeNode root) {
    int differingInvariants = 0;
    
    for(Enumeration e = root.preorderEnumeration(); e.hasMoreElements(); ) {
      DefaultMutableTreeNode node = (DefaultMutableTreeNode) e.nextElement();
      Object o = node.getUserObject();
      if (o instanceof Pair) {
        Pair p = (Pair) o;
        if ((p.a instanceof Invariant || p.a == null) &&
            (p.b instanceof Invariant || p.b == null) &&
            ! sameInvariant(p)) {
          differingInvariants++;
        }
      }
    }   
    return differingInvariants;
    
  }
  */

}

package daikon;

import daikon.inv.*;
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
    "  -a  Display the tree of all invariants" + lineSep
    ;


  // The output mode selected by the user
  private static final int PRINT_DIFF_INV = 0;
  private static final int PRINT_ALL_INV = 1;
  private static int mode = PRINT_DIFF_INV;

  /** Read two PptMap objects from their respective files and diff them. */
  public static void main(String[] args) throws FileNotFoundException,
  StreamCorruptedException, OptionalDataException, IOException,
  ClassNotFoundException {

    Getopt g = new Getopt("daikon.Diff", args, "hda");
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

    DefaultMutableTreeNode tree = diffPptMap(map1, map2);

    switch (mode) {
    case PRINT_DIFF_INV:
      System.out.println(printDifferences(tree));
      break;
    case PRINT_ALL_INV:
      System.out.println(printTree(tree));
      break;
    default:
      Assert.assert(false, "Can't get here");
    }
  }


  // Returns a tree of corresponding program points, and corresponding
  // invariants at each program point.  This tree can be walked to
  // determine differences between the sets of invaraints.
  public static DefaultMutableTreeNode diffPptMap(PptMap map1, PptMap map2) {
    DefaultMutableTreeNode root = new DefaultMutableTreeNode("root");

    SortedSet sset1 = new TreeSet(Ppt.NAME_COMPARATOR);
    sset1.addAll(map1.asCollection());
    SortedSet sset2 = new TreeSet(Ppt.NAME_COMPARATOR);
    sset2.addAll(map2.asCollection());    

    Iterator opi = new OrderedPairIterator(sset1.iterator(), sset2.iterator(),
                                           Ppt.NAME_COMPARATOR);
    while(opi.hasNext()) {
      Pair ppts = (Pair) opi.next();
      DefaultMutableTreeNode node = diffPptTopLevel(ppts);
      root.add(node);
    }
    
    return root;
  }


  // Takes a pair of corresponding top-level program points, and
  // returns a tree of the corresponding invariants.  Either of the
  // program points may be null.
  public static DefaultMutableTreeNode diffPptTopLevel(Pair ppts) {
    DefaultMutableTreeNode pptsNode = new DefaultMutableTreeNode(ppts);
    PptTopLevel ppt1 = (PptTopLevel) ppts.a;
    PptTopLevel ppt2 = (PptTopLevel) ppts.b;

    Assert.assert(ppt1 == null || ppt2 == null ||
                  Ppt.NAME_COMPARATOR.compare(ppt1, ppt2) == 0,
                  "Program points do not correspond");

    List invs1;
    if (ppt1 != null) {
      invs1 = ppt1.invariants_vector();
      Collections.sort(invs1, Invariant.CLASS_VARNAME_COMPARATOR);
    } else {
      invs1 = Collections.EMPTY_LIST;
    }

    List invs2;
    if (ppt2 != null) {
      invs2 = ppt2.invariants_vector();
      Collections.sort(invs2, Invariant.CLASS_VARNAME_COMPARATOR);
    } else {
      invs2 = Collections.EMPTY_LIST;
    }

    Iterator opi = new OrderedPairIterator(invs1.iterator(), invs2.iterator(),
                                           Invariant.CLASS_VARNAME_COMPARATOR);
    while(opi.hasNext()) {
      Pair invariants = (Pair) opi.next();
      DefaultMutableTreeNode invsNode =
        new DefaultMutableTreeNode(invariants, false);
      pptsNode.add(invsNode);      
    }

    return pptsNode;
  }

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

}

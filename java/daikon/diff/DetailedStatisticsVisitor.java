package daikon.diff;

import java.io.*;
import daikon.inv.*;
import daikon.inv.unary.*;
import daikon.inv.unary.scalar.*;
import utilMDE.*;

public class DetailedStatisticsVisitor implements NodeVisitor {

  // Types of invariants
  public static final int NUM_TYPES = 5;
  public static final int TYPE_NULLARY = 0;
  public static final int TYPE_UNARY_INTERESTING = 1;
  public static final int TYPE_UNARY_UNINTERESTING = 2;
  public static final int TYPE_BINARY = 3;
  public static final int TYPE_TERNARY = 4;

  // Relationships between invariants
  public static final int NUM_RELATIONSHIPS = 8;
  // Both present, same invariant, same justification
  public static final int REL_IDENTICAL = 0;
  // Both present, same invariant, justified in file1, not justified in file2
  public static final int REL_JUST1 = 1;
  // Both present, same invariant, justified in file2, not justified in file1
  public static final int REL_JUST2 = 2;
  // Both present, different invariant
  public static final int REL_DIFF = 3;
  // Present in file1, justified in file1, not present in file2
  public static final int REL_MISS_1JUST = 4;
  // Present in file1, not justified in file1, not present in file2
  public static final int REL_MISS_1UNJUST = 5;
  // Present in file2, justified in file2, not present in file1
  public static final int REL_MISS_2JUST = 6;
  // Present in file2, not justified in file2, not present in file1
  public static final int REL_MISS_2UNJUST = 7;

  // Table of frequencies, indexed by type of invariant, and
  // relationship between the invariants
  protected int[][] freq = new int[NUM_TYPES][NUM_RELATIONSHIPS];


  public void visitRootNode(RootNode node) { }

  public void visitPptNode(PptNode node) { }

  public void visitInvNode(InvNode node) {
    Invariant inv1 = node.getInv1();
    Invariant inv2 = node.getInv2();

    // Set inv to a non-null invariant
    Invariant inv = (inv1 != null) ? inv1 : inv2;

    int arity = inv.ppt.arity;
    int type;
    switch (arity) {
    case 0:
      type = TYPE_NULLARY;
      break;
    case 1:
      if (inv.isInteresting()) {
        type = TYPE_UNARY_INTERESTING;
      } else {
        type = TYPE_UNARY_UNINTERESTING;
      }
      break;
    case 2:
      type = TYPE_BINARY;
      break;
    case 3:
      type = TYPE_TERNARY;
      break;
    default:
      throw new Error("Invalid arity: " + arity);
    }

    int relationship;

    if (inv1 == null) {
      relationship = inv2.justified() ? REL_MISS_2JUST : REL_MISS_2UNJUST;
    } else if (inv2 == null) {
      relationship = inv1.justified() ? REL_MISS_1JUST : REL_MISS_1UNJUST;
    } else if (inv1.isSameInvariant(inv2)) {
      boolean justified1 = inv1.justified();
      boolean justified2 = inv2.justified();
      if (justified1 && justified2) {
        relationship = REL_IDENTICAL;
      } else if (justified1) {
        relationship = REL_JUST1;
      } else {
        relationship = REL_JUST2;
      }
    } else {
      relationship = REL_DIFF;
    }

    freq[type][relationship]++;
  }

  // Returns a tab-separated listing of its data, suitable for storing in a
  // file
  public String repr() {
    StringWriter sw = new StringWriter();
    PrintWriter pw = new PrintWriter(sw);
    
    for (int type=0; type < NUM_TYPES; type++) {
      for (int rel=0; rel < NUM_RELATIONSHIPS; rel++) {
        pw.println(String.valueOf(type) + "\t" + String.valueOf(rel) + "\t" +
                   String.valueOf(freq[type][rel]));
      }
    }

    return sw.toString();
  }

  // Returns a human-readable table of its data
  public String format() {
    StringWriter sw = new StringWriter();
    PrintWriter pw = new PrintWriter(sw);
    
    pw.println("STATISTICS");
    pw.println("\t0\t1\t2\t3\t4\t5\t6\t7\tTOTAL");

    pw.print("Null");
    for (int rel=0; rel < NUM_RELATIONSHIPS; rel++) {
      pw.print("\t" + freq[TYPE_NULLARY][rel]);
    }
    pw.print("\t" + ArraysMDE.sum(freq[TYPE_NULLARY]));
    pw.println();

    /*
    pw.print("Unary");
    for (int rel=0; rel < NUM_RELATIONSHIPS; rel++) {
      pw.print("\t" + (freq[TYPE_UNARY_INTERESTING][rel] +
                       freq[TYPE_UNARY_UNINTERESTING][rel]));
    }
    pw.print("\t" + (ArraysMDE.sum(freq[TYPE_UNARY_INTERESTING]) +
                     ArraysMDE.sum(freq[TYPE_UNARY_UNINTERESTING])));
    pw.println();
    */

    pw.print("UInt");
    for (int rel=0; rel < NUM_RELATIONSHIPS; rel++) {
      pw.print("\t" + freq[TYPE_UNARY_INTERESTING][rel]);
    }
    pw.print("\t" + ArraysMDE.sum(freq[TYPE_UNARY_INTERESTING]));
    pw.println();

    pw.print("U!Int");
    for (int rel=0; rel < NUM_RELATIONSHIPS; rel++) {
      pw.print("\t" + freq[TYPE_UNARY_UNINTERESTING][rel]);
    }
    pw.print("\t" + ArraysMDE.sum(freq[TYPE_UNARY_UNINTERESTING]));
    pw.println();

    pw.print("Bin");
    for (int rel=0; rel < NUM_RELATIONSHIPS; rel++) {
      pw.print("\t" + freq[TYPE_BINARY][rel]);
    }
    pw.print("\t" + ArraysMDE.sum(freq[TYPE_BINARY]));
    pw.println();

    pw.print("Ter");
    for (int rel=0; rel < NUM_RELATIONSHIPS; rel++) {
      pw.print("\t" + freq[TYPE_TERNARY][rel]);
    }
    pw.print("\t" + ArraysMDE.sum(freq[TYPE_TERNARY]));
    pw.println();

    pw.print("TOTAL");
    for (int rel = 0; rel < NUM_RELATIONSHIPS; rel++) {
      int sum = 0;
      for (int type = 0; type < NUM_TYPES; type++) {
        sum += freq[type][rel];
      }
      pw.print("\t" + sum);
    }
    pw.print("\t" + ArraysMDE.sum(freq));

    pw.println();

    pw.println();

    return sw.toString();
  }

  // Use this method instead of making the array public, to preserve
  // abstraction
  public int freq(int type, int relationship) {
    return freq[type][relationship];
  }

}

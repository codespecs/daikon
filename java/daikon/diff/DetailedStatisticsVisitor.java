package daikon.diff;

import daikon.inv.Invariant;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.SideEffectFree;
import org.plumelib.util.ArraysPlume;
import org.plumelib.util.StringsPlume;

/**
 * Computes statistics about the differences between the sets of invariants. The statistics can be
 * printed as a human-readable table or a tab-separated list suitable for further processing.
 */
public class DetailedStatisticsVisitor extends DepthFirstVisitor {

  public static final Logger debug = Logger.getLogger("daikon.diff.DetailedStatisticsVisitor");

  private static final int FIELD_WIDTH = 5;
  private static final int LABEL_WIDTH = 7;

  /** The number of arities for invariants; equivalently, 1 more than the max arity. */
  public static final int NUM_ARITIES = 4;

  /** A string representations for each arity. Length = NUM_ARITIES. */
  public static final String[] ARITY_LABELS = {"Nul", "Una", "Bin", "Ter"};

  // Relationships between invariants
  public static final int NUM_RELATIONSHIPS = 12;
  // Both present, same invariant, justified in file1, justified in file2
  public static final int REL_SAME_JUST1_JUST2 = 0;
  // Both present, same invariant, justified in file1, unjustified in file2
  public static final int REL_SAME_JUST1_UNJUST2 = 1;
  // Both present, same invariant, unjustified in file1, justified in file2
  public static final int REL_SAME_UNJUST1_JUST2 = 2;
  // Both present, same invariant, unjustified in file1, unjustified in file2
  public static final int REL_SAME_UNJUST1_UNJUST2 = 3;
  // Both present, diff invariant, justification in file1, justified
  // in file2
  public static final int REL_DIFF_JUST1_JUST2 = 4;
  // Both present, different invariant, justified in file1,
  // unjustified in file2
  public static final int REL_DIFF_JUST1_UNJUST2 = 5;
  // Both present, different invariant, unjustified in file1,
  // justified in file2
  public static final int REL_DIFF_UNJUST1_JUST2 = 6;
  // Both present, different invariant, unjustified in file1,
  // unjustified in file2
  public static final int REL_DIFF_UNJUST1_UNJUST2 = 7;
  // Present in file1, justified in file1, not present in file2
  public static final int REL_MISS_JUST1 = 8;
  // Present in file1, unjustified in file1, not present in file2
  public static final int REL_MISS_UNJUST1 = 9;
  // Not present in file1, present in file2, justified in file2
  public static final int REL_MISS_JUST2 = 10;
  // Not present in file1, present in file2, unjustified in file2
  public static final int REL_MISS_UNJUST2 = 11;

  public static final String[] RELATIONSHIP_LABELS = {
    "SJJ", "SJU", "SUJ", "SUU", "DJJ", "DJU", "DUJ", "DUU", "JM", "UM", "MJ", "MU"
  };

  /**
   * Table of frequencies, indexed by arity of invariant, and relationship between the invariants.
   *
   * <p>Unfortunately, this is heterogeneous: some measurement are integers and others are doubles.
   */
  private double[][] freq = new double[NUM_ARITIES][NUM_RELATIONSHIPS];

  private boolean continuousJustification;

  public DetailedStatisticsVisitor(boolean continuousJustification) {
    this.continuousJustification = continuousJustification;
  }

  @Override
  public void visit(InvNode node) {
    Invariant inv1 = node.getInv1();
    Invariant inv2 = node.getInv2();
    if (shouldAddFrequency(inv1, inv2)) {
      addFrequency(node.getInv1(), node.getInv2());
    }
  }

  /**
   * Adds the difference between the two invariants to the appropriate entry in the frequencies
   * table.
   */
  private void addFrequency(@Nullable Invariant inv1, @Nullable Invariant inv2) {
    if (continuousJustification) {
      addFrequencyContinuous(inv1, inv2);
    } else {
      addFrequencyBinary(inv1, inv2);
    }
  }

  /**
   * Treats justification as a binary value. The table entry is incremented by 1 regardless of the
   * difference in justifications.
   */
  private void addFrequencyBinary(@Nullable Invariant inv1, @Nullable Invariant inv2) {
    int arity = determineArity(inv1, inv2);
    int relationship = determineRelationship(inv1, inv2);
    freq[arity][relationship]++;
  }

  /**
   * Treats justification as a continuous value. If one invariant is justified but the other is
   * unjustified, the table entry is incremented by the difference in justifications.
   */
  private void addFrequencyContinuous(@Nullable Invariant inv1, @Nullable Invariant inv2) {
    int arity = determineArity(inv1, inv2);
    int relationship = determineRelationship(inv1, inv2);

    switch (relationship) {
      case REL_SAME_JUST1_UNJUST2:
      case REL_SAME_UNJUST1_JUST2:
        assert inv1 != null && inv2 != null
            : "@AssumeAssertion(nullness)"; // application invariant about return value of
        // determineRelationship
        freq[arity][relationship] += calculateConfidenceDifference(inv1, inv2);
        break;
      default:
        freq[arity][relationship]++;
    }
  }

  /**
   * Returns the difference in the probabilites of the two invariants. Confidence values less than 0
   * (i.e. CONFIDENCE_NEVER) are rounded up to 0.
   */
  private static double calculateConfidenceDifference(Invariant inv1, Invariant inv2) {
    assert inv1 != null && inv2 != null;
    double diff;
    double conf1 = Math.max(inv1.getConfidence(), 0);
    double conf2 = Math.max(inv2.getConfidence(), 0);
    diff = Math.abs(conf1 - conf2);
    return diff;
  }

  /** Returns the arity of the invariant pair. */
  public static int determineArity(@Nullable Invariant inv1, @Nullable Invariant inv2) {

    // Set inv to a non-null invariant
    @SuppressWarnings("nullness") // at least one argument is non-null
    @NonNull Invariant inv = (inv1 != null) ? inv1 : inv2;

    if (debug.isLoggable(Level.FINE)) {
      debug.fine(
          "visit: "
              + ((inv1 != null) ? inv1.ppt.parent.name() : "NULL")
              + " "
              + ((inv1 != null) ? inv1.repr() : "NULL")
              + " - "
              + ((inv2 != null) ? inv2.repr() : "NULL"));
    }

    int arity = inv.ppt.arity();
    if (debug.isLoggable(Level.FINE)) {
      debug.fine("  arity: " + arity);
    }
    return arity;
  }

  /**
   * Returns the relationship between the two invariants. There are twelve possible relationships,
   * described at the beginning of this file.
   */
  public static int determineRelationship(@Nullable Invariant inv1, @Nullable Invariant inv2) {
    int relationship;

    if (inv1 == null) {
      assert inv2 != null : "@AssumeAssertion(nullness): at least one argument is non-null";
      relationship = inv2.justified() ? REL_MISS_JUST2 : REL_MISS_UNJUST2;
    } else if (inv2 == null) {
      relationship = inv1.justified() ? REL_MISS_JUST1 : REL_MISS_UNJUST1;
    } else {
      boolean justified1 = inv1.justified();
      boolean justified2 = inv2.justified();
      if (inv1.isSameInvariant(inv2)) {
        if (justified1 && justified2) {
          relationship = REL_SAME_JUST1_JUST2;
        } else if (justified1 && !justified2) {
          relationship = REL_SAME_JUST1_UNJUST2;
        } else if (!justified1 && justified2) {
          relationship = REL_SAME_UNJUST1_JUST2;
        } else {
          relationship = REL_SAME_UNJUST1_UNJUST2;
        }
      } else {
        if (justified1 && justified2) {
          relationship = REL_DIFF_JUST1_JUST2;
        } else if (justified1 && !justified2) {
          relationship = REL_DIFF_JUST1_UNJUST2;
        } else if (!justified1 && justified2) {
          relationship = REL_DIFF_UNJUST1_JUST2;
        } else {
          relationship = REL_DIFF_UNJUST1_UNJUST2;
        }
      }
    }

    return relationship;
  }

  /** Returns a tab-separated listing of its data, suitable for post-processing. */
  public String repr() {
    StringWriter sw = new StringWriter();
    PrintWriter pw = new PrintWriter(sw);

    for (int arity = 0; arity < NUM_ARITIES; arity++) {
      for (int rel = 0; rel < NUM_RELATIONSHIPS; rel++) {
        pw.println(
            String.valueOf(arity)
                + "\t"
                + String.valueOf(rel)
                + "\t"
                + String.valueOf(freq[arity][rel]));
      }
    }

    return sw.toString();
  }

  /** Returns a human-readable table of its data. */
  @SuppressWarnings("NarrowingCompoundAssignment") // due to heterogeneous freq array
  @SideEffectFree
  public String format() {
    StringWriter sw = new StringWriter();
    PrintWriter pw = new PrintWriter(sw);

    pw.println("STATISTICS");
    pw.print("       ");
    for (int rel = 0; rel < NUM_RELATIONSHIPS; rel++) {
      pw.print(StringsPlume.rpad(RELATIONSHIP_LABELS[rel], FIELD_WIDTH));
    }
    pw.println(StringsPlume.rpad("TOTAL", FIELD_WIDTH));

    for (int arity = 0; arity < NUM_ARITIES; arity++) {
      pw.print(StringsPlume.rpad(ARITY_LABELS[arity], LABEL_WIDTH));
      for (int rel = 0; rel < NUM_RELATIONSHIPS; rel++) {
        int f = (int) freq[arity][rel];
        pw.print(StringsPlume.rpad(f, FIELD_WIDTH));
      }
      int s = (int) ArraysPlume.sum(freq[arity]);
      pw.print(StringsPlume.rpad(s, FIELD_WIDTH));
      pw.println();
    }

    pw.print(StringsPlume.rpad("TOTAL", LABEL_WIDTH));
    for (int rel = 0; rel < NUM_RELATIONSHIPS; rel++) {
      int sum = 0;
      for (int arity = 0; arity < NUM_ARITIES; arity++) {
        sum += (int) freq[arity][rel];
      }
      pw.print(StringsPlume.rpad(sum, FIELD_WIDTH));
    }
    pw.print(StringsPlume.rpad((int) ArraysPlume.sum(freq), FIELD_WIDTH));

    pw.println();

    pw.println();

    return sw.toString();
  }

  /**
   * Returns the frequency of pairs of invariants we have seen with this arity and relationship. May
   * be a non-integer, since we may be treating justification as a continuous value.
   */
  public double freq(int arity, int relationship) {
    return freq[arity][relationship];
  }

  /**
   * Returns true if the pair of invariants should be added to the frequency table, based on their
   * printability.
   */
  private static boolean shouldAddFrequency(@Nullable Invariant inv1, @Nullable Invariant inv2) {
    return (inv1 != null && inv1.isWorthPrinting()) || (inv2 != null && inv2.isWorthPrinting());
  }
}

package daikon.diff;

import java.io.*;
import java.text.*;
import daikon.*;
import daikon.inv.Invariant;

/**
 * Prints all the invariant pairs, including pairs containing
 * identical invariants.
 **/
public class PrintAllVisitor extends DepthFirstVisitor {
  
  private static DecimalFormat PROBABILITY_FORMAT =
    new DecimalFormat("0.####");

  private PrintStream ps;
  private boolean verbose;
  private boolean printEmptyPpts;
  
  /**
   * Stores the output generated when visiting invariant nodes.  This
   * output cannot be printed directly to the print stream, because
   * the Ppt output must come before the Invariant output.
   **/
  private StringBuffer bufOutput = new StringBuffer();

  public PrintAllVisitor(PrintStream ps, boolean verbose,
                         boolean printEmptyPpts) {
    this.ps = ps;
    this.verbose = verbose;
    this.printEmptyPpts = printEmptyPpts;
  }

  /**
   * Prints the pair of program points, and all the invariants
   * contained within them.
   **/
  public void visit(PptNode node) {
    // Empty the string buffer
    bufOutput.setLength(0);

    super.visit(node);

    if (bufOutput.length() > 0 || printEmptyPpts) {
      Ppt ppt1 = node.getPpt1();
      Ppt ppt2 = node.getPpt2();
      
      ps.print("<");
      if (ppt1 == null) {
        ps.print((String) null);
      } else {
        ps.print(ppt1.name);
      }
      
      if (ppt1 == null || ppt2 == null || !ppt1.name.equals(ppt2.name)) {
        ps.print(", ");
        if (ppt2 == null) {
          ps.print((String) null);
        } else {
          ps.print(ppt2.name);
        }
      }
      ps.println(">");
      ps.print(bufOutput.toString());
    }
  }

  /**
   * Prints a pair of invariants.  Includes the type of the invariants
   * and their relationship.
   **/
  public void visit(InvNode node) {
    Invariant inv1 = node.getInv1();
    Invariant inv2 = node.getInv2();

    bufPrint("  " + "<");
    if (inv1 == null) {
      bufPrint((String) null);
    } else {
      printInvariant(inv1);
    }
    bufPrint(", ");
    if (inv2 == null) {
      bufPrint((String) null);
    } else {
      printInvariant(inv2);
    }
    bufPrint(">");

    int type = DetailedStatisticsVisitor.determineType(inv1, inv2);
    String typeLabel = DetailedStatisticsVisitor.TYPE_LABELS[type];
    int rel = DetailedStatisticsVisitor.determineRelationship(inv1, inv2);
    String relLabel = DetailedStatisticsVisitor.RELATIONSHIP_LABELS[rel];

    bufPrint(" (" + typeLabel + "," + relLabel + ")");

    bufPrintln();
  }

  /**
   * Prints an invariant, including its printability and possibly its
   * probability.  Example: "argv != null {0.0001+}"
   * 
   **/
  protected void printInvariant(Invariant inv) {
    if (verbose) {
      bufPrint(inv.repr_prob());
      bufPrint(" {");
      printPrintability(inv);
      bufPrint("}");
    } else {
      bufPrint(inv.format());
      bufPrint(" {");
      printProbability(inv);
      printPrintability(inv);
      bufPrint("}");
    }    
  }

  /**
   * Prints the probability of the invariant.  Probabilities between
   * .0001 and 0 are rounded to .0001.
   **/
  private void printProbability(Invariant inv) {
    double prob = inv.getProbability();

    if (0 < prob && prob < .0001) {
      prob = .0001;
    }

    bufPrint(PROBABILITY_FORMAT.format(prob));
  }

  /**
   * Prints '+' if the invariant is worth printing, '-' otherwise.
   **/
  private void printPrintability(Invariant inv) {
    if (inv.isWorthPrinting()) {
      bufPrint("+");
    } else {
      bufPrint("-");
    }
  }
  
  // "prints" by appending to a string buffer
  protected void bufPrint(String s) {
    bufOutput.append(s);
  }
  protected void bufPrintln(String s) {
    bufPrint(s);
    bufPrintln();
  }
  protected void bufPrintln() {
    bufOutput.append(Global.lineSep);
  }  

}

package daikon.diff;

import java.io.*;
import java.text.*;
import daikon.*;
import daikon.inv.Invariant;

public class PrintAllVisitor extends NodeVisitor {
  
  private static DecimalFormat PROBABILITY_FORMAT =
    new DecimalFormat("0.####");

  private PrintStream ps;
  private boolean verbose;
  
  // Stores the output generated when visiting invariant nodes.  This
  // output cannot be printed directly to the print stream, because
  // the ppt output must come before the invariant output.
  private StringBuffer invariantOutput = new StringBuffer();

  public PrintAllVisitor(PrintStream ps, boolean verbose) {
    this.ps = ps;
    this.verbose = verbose;
  }

  public void preVisitPptNode(PptNode node) {
    // Empty the string buffer
    invariantOutput.setLength(0);
  }

  public void postVisitPptNode(PptNode node) {
    if (invariantOutput.length() > 0) {
      Ppt ppt1 = node.getPpt1();
      Ppt ppt2 = node.getPpt2();
      
      print("<");
      if (ppt1 == null) {
        print((String) null);
      } else {
        print(ppt1.name);
      }
      
      if (ppt1 == null || ppt2 == null || !ppt1.name.equals(ppt2.name)) {
        print(", ");
        if (ppt2 == null) {
          print((String) null);
        } else {
          print(ppt2.name);
        }
      }
      println(">");
      print(invariantOutput.toString());
    }
  }

  public void preVisitInvNode(InvNode node) {
    Invariant inv1 = node.getInv1();
    Invariant inv2 = node.getInv2();

    invPrint("  " + "<");
    if (inv1 == null) {
      invPrint((String) null);
    } else {
      printInvariant(inv1, node);
    }
    invPrint(", ");
    if (inv2 == null) {
      invPrint((String) null);
    } else {
      printInvariant(inv2, node);
    }
    invPrint(">");

    int type = DetailedStatisticsVisitor.determineType(inv1, inv2);
    String typeLabel = DetailedStatisticsVisitor.TYPE_LABELS[type];
    int rel = DetailedStatisticsVisitor.determineRelationship(inv1, inv2);

    invPrint(" (" + typeLabel + "," + rel + ")");

    invPrintln();
  }

  protected void printInvariant(Invariant inv, InvNode node) {
    if (verbose) {
      invPrint(inv.repr_prob());
      invPrint(" {");
      printPrintability(inv);
      invPrint("}");
    } else {
      invPrint(inv.format());
      invPrint(" {");
      printProbability(inv);
      printPrintability(inv);
      invPrint("}");
    }    
  }

  private void printProbability(Invariant inv) {
    double prob = inv.getProbability();

    // Round small probabilities to .0001
    if (0 < prob && prob < .0001) {
      prob = .0001;
    }

    invPrint(PROBABILITY_FORMAT.format(prob));
  }

  private void printPrintability(Invariant inv) {
    if (inv.isWorthPrinting()) {
      invPrint("+");
    } else {
      invPrint("-");
    }
  }

  protected void print(String s) {
    ps.print(s);
  }

  protected void println(String s) {
    ps.println(s);
  }

  protected void println() {
    ps.println();
  }

  // "prints" invariant differences by appending to a string buffer
  protected void invPrint(String s) {
    invariantOutput.append(s);
  }
  protected void invPrintln(String s) {
    invPrint(s);
    invPrintln();
  }
  protected void invPrintln() {
    invariantOutput.append(Global.lineSep);
  }  

}

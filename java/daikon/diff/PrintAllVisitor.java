package daikon.diff;

import java.io.*;
import java.text.*;
import daikon.*;
import daikon.inv.Invariant;

public class PrintAllVisitor implements NodeVisitor {
  
  private static DecimalFormat PROBABILITY_FORMAT =
    new DecimalFormat("0.####");

  private PrintStream ps;
  protected boolean verbose;

  public PrintAllVisitor(PrintStream ps, boolean verbose) {
    this.ps = ps;
    this.verbose = verbose;
  }

  public void visitRootNode(RootNode node) {
  }

  public void visitPptNode(PptNode node) {
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
  }

  public void visitInvNode(InvNode node) {
    Invariant inv1 = node.getInv1();
    Invariant inv2 = node.getInv2();

    print("  " + "<");
    if (inv1 == null) {
      print((String) null);
    } else {
      printInvariant(inv1, node);
    }
    print(", ");
    if (inv2 == null) {
      print((String) null);
    } else {
      printInvariant(inv2, node);
    }
    print(">");

    int type = DetailedStatisticsVisitor.determineType(inv1, inv2);
    String typeLabel = DetailedStatisticsVisitor.TYPE_LABELS[type];
    int rel = DetailedStatisticsVisitor.determineRelationship(inv1, inv2);

    print(" (" + typeLabel + "," + rel + ")");

    println();
  }

  protected void printInvariant(Invariant inv, InvNode node) {
    if (verbose) {
      print(inv.repr_prob());
      print(" {");
      printPrintability(inv);
      print("}");
    } else {
      print(inv.format());
      print(" {");
      printProbability(inv);
      printPrintability(inv);
      print("}");
    }    
  }

  private void printProbability(Invariant inv) {
    double prob = inv.getProbability();

    // Round small probabilities to .0001
    if (0 < prob && prob < .0001) {
      prob = .0001;
    }

    print(PROBABILITY_FORMAT.format(prob));
  }

  private void printPrintability(Invariant inv) {
    if (inv.isWorthPrinting()) {
      print("+");
    } else {
      print("-");
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
}

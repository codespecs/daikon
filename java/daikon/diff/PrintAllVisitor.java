package daikon.diff;

import java.io.*;
import daikon.inv.Invariant;

public class PrintAllVisitor implements NodeVisitor {
  
  private PrintStream ps;
  protected boolean verbose;

  public PrintAllVisitor(PrintStream ps, boolean verbose) {
    this.ps = ps;
    this.verbose = verbose;
  }

  public void visitRootNode(RootNode node) {
    println("root");
  }

  public void visitPptNode(PptNode node) {
    print("  " + "<");
    if (node.getPpt1() == null) {
      print((String) null);
    } else {
      print(node.getPpt1().name);
    }
    print(", ");
    if (node.getPpt2() == null) {
      print((String) null);
    } else {
      print(node.getPpt2().name);
    }
    println(">");
  }

  public void visitInvNode(InvNode node) {
    print("    " + "<");
    if (node.getInv1() == null) {
      print((String) null);
    } else {
      printInvariant(node.getInv1(), node);
    }
    print(", ");
    if (node.getInv2() == null) {
      print((String) null);
    } else {
      printInvariant(node.getInv2(), node);
    }
    println(">");
  }

  protected void printInvariant(Invariant inv, InvNode node) {
    if (verbose)
      print(inv.repr_prob() + " (worth printing? " +
            inv.isWorthPrinting_sansControlledCheck_debug() + ")");
    else
      print(inv.format());
  }

  protected void print(String s) {
    ps.print(s);
  }

  protected void println(String s) {
    ps.println(s);
  }
}

package daikon.diff;

import java.io.*;
import daikon.inv.Invariant;

public class PrintAllVisitor implements NodeVisitor {
  
  private StringWriter sw;
  private PrintWriter pw;
  protected boolean verbose;

  public PrintAllVisitor(boolean verbose) {
    this.verbose = verbose;
    sw = new StringWriter();
    pw = new PrintWriter(sw);
  }

  public String getOutput() {
    return sw.toString();
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
      print(inv.repr_prob());
    else
      print(inv.format());
  }

  protected void print(String s) {
    pw.print(s);
  }

  protected void println(String s) {
    pw.println(s);
  }
}

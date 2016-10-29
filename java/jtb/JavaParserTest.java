package jtb;

import java.io.*;
import jtb.JavaParser;
import jtb.ParseException;
import jtb.syntaxtree.*;
import jtb.visitor.*;

public class JavaParserTest {

  /**
   * Reads the {@code .java} file given on the command line, and writes a
   * {@code .java-parsed} file that should be identical.
   */
  public static void main(String args[]) {
    JavaParser parser = null;
    Node root;
    if (args.length == 0) {
      System.out.println("Java Parser Version 1.1:  Reading from standard input . . .");
      parser = new JavaParser(System.in);
    } else {
      if (args.length == 1) {
        System.out.println("Java Parser Version 1.1:  Reading from file " + args[0] + " . . .");
        try {
          parser = new JavaParser(new java.io.FileInputStream(args[0]));
        } catch (java.io.FileNotFoundException e) {
          System.out.println("Java Parser Version 1.1:  File " + args[0] + " not found.");
          System.exit(1);
        }
      } else {
        System.out.println("Java Parser Version 1.1:  Usage is one of:");
        System.out.println("         java JavaParser < inputfile");
        System.out.println("OR");
        System.out.println("         java JavaParser inputfile");
        System.exit(1);
      }
    }
    try {
      root = parser.CompilationUnit();
      root.accept(
          new DepthFirstVisitor() {
            public void visit(NodeToken n) {
              //System.out.println("{" + ((n.getParent()).getClass()).getName() +"}: " + n.tokenImage);
            }
          });

      try {
        Writer output = new FileWriter(args[0] + "-parsed");
        root.accept(new TreeFormatter(2, 80));
        root.accept(new TreeDumper(output));
        output.close();
      } catch (IOException e) {
        System.err.println(
            "While trying to write file "
                + args[0]
                + "-parsed"
                + ", an IOException "
                + "occurred. Here is the message and stack trace: "
                + e.getMessage());
        e.printStackTrace();
        throw new Error(e);
      }

      System.out.println("Java Parser Version 1.1:  Java program parsed successfully.");
    } catch (ParseException e) {
      System.out.println(e.getMessage());
      System.out.println("Java Parser Version 1.1:  Encountered errors during parse.");
      System.exit(1);
    }
  }
}

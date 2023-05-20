package daikon.split;

import daikon.VarInfo;
import daikon.tools.jtb.Ast;
import jtb.ParseException;
import jtb.syntaxtree.Node;
import jtb.syntaxtree.NodeToken;
import jtb.visitor.DepthFirstVisitor;
import org.checkerframework.checker.nullness.qual.MonotonicNonNull;

/**
 * ThisFixer is a visitor for a jtb syntax tree that changes all instances of "this." to "this_".
 * For example "this.x" would go to "this_x". It also finds unqualified member variables and inserts
 * a preceeding "this_". These two changes allow splitter expressions that contain a parameter and
 * an unqualified member variable with the same name to compile correctly.
 */
class ThisFixer extends DepthFirstVisitor {
  // Note: the instances of "this." are not really being removed; instead,
  // the token images are being changed to "this_" so when viewed as a
  // string subsequent splitter code can process them correctly.
  // However, the nodes are still in the jtb tree.

  private int columnshift = 0;
  private int columnshiftline = -1;

  // column shifting only applies to a single line, then is turned off again.
  // States for the variables:
  // columnshift == 0, columnshiftline == -1:
  //    no column shifting needed
  // columnshift != 0, columnshiftline != -1:
  //    column shifting being needed, applies only to specified line

  /** All possible varInfos for the variables in the conditions. */
  private VarInfo[] varInfos;

  /** The token previously visited. Null only when visiting the first token. */
  private @MonotonicNonNull NodeToken lastToken;

  /**
   * Creates a new instance of ThisFixer.
   *
   * @param varInfos is a list of the VarInfos for the ppt
   */
  private ThisFixer(VarInfo[] varInfos) {
    super();
    this.varInfos = varInfos;
  }

  /**
   * Modifies "this." or inserts "this_" in expression.
   *
   * @param expression valid segment of java code which should be modified
   * @return expression with instances of "this." changed to "this_"
   */
  public static String fixThisUsage(String expression, VarInfo[] varInfos) throws ParseException {
    Node root = Visitors.getJtbTree(expression);
    ThisFixer fixer = new ThisFixer(varInfos);
    root.accept(fixer);
    return Ast.format(root);
  }

  /**
   * This method should not be directly used by users of this class. Replaces the token image of "."
   * with "_" if was preceeded by "this", or adds "this_" to the token image of an unqualified
   * member variable.
   */
  @Override
  public void visit(NodeToken n) {
    boolean found = false;

    if (n.beginLine == columnshiftline) {
      n.beginColumn = n.beginColumn + columnshift;
    } else {
      columnshift = 0;
      columnshiftline = -1;
    }

    if (Visitors.isDot(n) && ((lastToken == null) || Visitors.isThis(lastToken))) {
      n.tokenImage = "_";
    } else if (Visitors.isIdentifier(n) && ((lastToken == null) || !Visitors.isDot(lastToken))) {

      for (VarInfo varInfo : varInfos) {
        if (varInfo.isParam()) {
          if (varInfo.name().equals(n.tokenImage)) {
            // variable is a formal parameter, don't
            // add "this_".
            found = true;
            break;
          }
        }
      }

      if (!found) {
        for (VarInfo varInfo : varInfos) {
          if (!varInfo.isParam()) {
            if (varInfo.name().equals("this." + n.tokenImage)) {
              n.tokenImage = "this_" + n.tokenImage;
              columnshift = columnshift + 5;
              columnshiftline = n.beginLine;
            }
          }
        }
      }
    }
    n.endColumn = n.endColumn + columnshift;

    lastToken = n;
    super.visit(n);
  }
}

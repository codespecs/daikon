package daikon.split;

import daikon.Global;
import daikon.VarInfo;
import daikon.tools.jtb.Ast;
import jtb.ParseException;
import jtb.syntaxtree.Node;
import jtb.syntaxtree.NodeToken;
import jtb.visitor.DepthFirstVisitor;
import org.checkerframework.checker.nullness.qual.MonotonicNonNull;

/**
 * NameFixer is a visitor for a JTB syntax tree that checks for unqualifed class member variables
 * and adds the className as a qualifier. For example, the condition:
 *
 * <pre>D[a]</pre>
 *
 * (where D is a member variable of the current class and a is a local) would be changed to:
 *
 * <pre>className.D[a]</pre>
 */
class NameFixer extends DepthFirstVisitor {

  private int columnshift = 0;
  private int columnshiftline = -1;

  // column shifting only applies to a single line, then is turned off again.
  // States for the variables:
  // columnshift == 0, columnshiftline == -1:
  //    no column shifting needed
  // columnshift != 0, columnshiftline != -1:
  //    column shifting being needed, applies only to specified line

  /** The name of the class containing the ppt. */
  private String className;

  /** All possible varInfos for the variables in the conditions. */
  private VarInfo[] varInfos;

  /**
   * The token previously visited. Null only when visiting the first token. Non-null if
   * lastTokenMayBeMemberVar is true.
   */
  private @MonotonicNonNull NodeToken lastToken;

  /**
   * True if the last token visited could be the name of a variable that needs the className prefix.
   */
  private boolean lastTokenMayBeMemberVar = false;

  /**
   * Creates a new instance of NameFixer.
   *
   * @param className is name of the containing class
   * @param varInfos is a list of the VarInfos for the ppt
   */
  private NameFixer(String className, VarInfo[] varInfos) {
    super();
    this.className = className;
    this.varInfos = varInfos;
  }

  /**
   * Fixes unqualifed class member variables.
   *
   * @param expression a valid segment of java code
   * @param className is name of the containing class
   * @param varInfos is a List of VarInfos for all the variables available
   * @return condition with all unqualifed variable references now qualified with className
   * @throws ParseException when condition is not a valid segment of java code
   */
  public static String fixUnqualifiedMemberNames(
      String expression, String className, VarInfo[] varInfos) throws ParseException {
    Global.debugSplit.fine("<<enter>> fixUnqualifiedMemberNames; expression: " + expression);
    Node root = Visitors.getJtbTree(expression);
    NameFixer fixer = new NameFixer(className, varInfos);
    root.accept(fixer);
    fixer.fixLastToken();
    String result = Ast.format(root);
    Global.debugSplit.fine("<<exit>>  fixUnqualifiedMemberNames; expression: " + result);
    return result;
  }

  /**
   * This method should not be directly used by users of this class; however, must be public to
   * fulfill Visitor interface.
   */
  @Override
  public void visit(NodeToken n) {
    boolean found = false;

    if (lastTokenMayBeMemberVar && !(Visitors.isLParen(n) || Visitors.isDot(n))) {
      assert lastToken != null
          : "@AssumeAssertion(nullness): dependent: because lastTokenMayBeMemberVar == true";
      int len = className.length() + 1;
      lastToken.tokenImage = className + "." + lastToken.tokenImage;
      lastToken.endColumn = lastToken.endColumn + len;
      columnshift = columnshift + len;
      columnshiftline = lastToken.beginLine;
    }

    lastTokenMayBeMemberVar = false;
    if (Visitors.isIdentifier(n) && ((lastToken == null) || !Visitors.isDot(lastToken))) {

      for (VarInfo varInfo : varInfos) {
        if (varInfo.name().equals(n.tokenImage)) {
          found = true;
          break; // expression variable is OK as is
        }
      }

      if (!found && !className.equals(n.tokenImage)) {
        String testName = className + "." + n.tokenImage;
        for (VarInfo varInfo : varInfos) {
          if (varInfo.name().equals(testName)) {
            lastTokenMayBeMemberVar = true;
            break; // could be previously unqualified member var
            // but we still need to make sure not followed by '(' or '.'
          }
        }
      }
    }

    if (n.beginLine == columnshiftline) {
      n.beginColumn = n.beginColumn + columnshift;
      n.endColumn = n.endColumn + columnshift;
    } else {
      columnshift = 0;
      columnshiftline = -1;
    }
    lastToken = n;
    super.visit(n);
  }

  private void fixLastToken() {
    if (lastTokenMayBeMemberVar) {
      assert lastToken != null
          : "@AssumeAssertion(nullness): dependent: because lastTokenMayBeMemberVar == true";
      int len = className.length() + 1;
      lastToken.tokenImage = className + "." + lastToken.tokenImage;
      lastToken.endColumn = lastToken.endColumn + len;
      columnshift = columnshift + len;
      columnshiftline = lastToken.beginLine;
    }
  }
}

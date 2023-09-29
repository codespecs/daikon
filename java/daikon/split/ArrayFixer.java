package daikon.split;

import daikon.ProglangType;
import daikon.VarInfo;
import daikon.tools.jtb.Ast;
import jtb.ParseException;
import jtb.syntaxtree.Node;
import jtb.syntaxtree.NodeToken;
import jtb.visitor.DepthFirstVisitor;
import org.checkerframework.checker.nullness.qual.MonotonicNonNull;

/**
 * ArrayFixer is a visitor for a JTB syntax tree that adds the "_identity" and "_array" suffixes to
 * an expression as needed. For example, the condition:
 *
 * <pre>{@code this_a[2] == 3 && this_a.length == 3 && this_a == this_b}</pre>
 *
 * (where a and b are int arrays) would be changed to:
 *
 * <pre>{@code
 * this_a_array[2] == 3 && this_a_array.length == 3 &&
 * this_a_identity == this_b_identity
 * }</pre>
 */
class ArrayFixer extends DepthFirstVisitor {

  private int columnshift = 0;
  private int columnshiftline = -1;

  // column shifting only applies to a single line, then is turned off again.
  // States for the variables:
  // columnshift == 0, columnshiftline == -1:
  //    no column shifting needed
  // columnshift != 0, columnshiftline != -1:
  //    column shifting being needed, applies only to specified line

  /** All possible variable names in the conditional. */
  private String[] varNames;

  /** All possible varInfos for the variables in the conditions. */
  private VarInfo[] varInfos;

  // varNames and varInfo must be the same length and be in the same order.

  /**
   * The token previously visited. Null only when visiting the first token. Non-null if
   * lastTokenMayBeElements or lastTokenMayBeIdentity is true.
   */
  private @MonotonicNonNull NodeToken lastToken;

  /**
   * True if the last token visited could be the name of a variable that needs the "_array" suffix.
   */
  private boolean lastTokenMayBeElements = false;

  /**
   * True if the last token visited could be the name of a variable that needs the "_identity"
   * suffix.
   */
  private boolean lastTokenMayBeIdentity = false;

  /**
   * Creates a new instance of ArrayFixer.
   *
   * @param vars is a list of strings of possible names of the variables in the condition
   * @param varInfos is a list of the corresponding VarInfos for each of the names in vars
   */
  private ArrayFixer(String[] vars, VarInfo[] varInfos) {
    super();
    this.varNames = vars;
    this.varInfos = varInfos;
  }

  /**
   * Fixes the arrays found in statement (see class description). names and varInfos must be in same
   * order s.t. the ith element of varInfos is the VarInfo for the ith element of names.
   *
   * @param expression a valid segment of java code
   * @param names is a List of Strings that are the names of all the variables in statement
   * @param varInfos is a List of VarInfos for all the variables named in names
   * @return condition with all variable referring to arrays suffixed with "_identity" or "_array"
   *     as needed
   * @throws ParseException when condition is not a valid segment of java code
   */
  public static String fixArrays(String expression, String[] names, VarInfo[] varInfos)
      throws ParseException {
    Node root = Visitors.getJtbTree(expression);
    ArrayFixer fixer = new ArrayFixer(names, varInfos);
    root.accept(fixer);
    fixer.fixLastToken();
    return Ast.format(root);
  }

  /**
   * Fixes the arrays found in statement (see class description). names and varInfos must be in same
   * order s.t. the ith element of varInfos is the VarInfo for the ith element of names.
   *
   * @param root the root of a jtb syntax tree
   * @param names is a List of Strings that are the names of all the variables in statement
   * @param varInfos is a List of VarInfos for all the variables named in names
   */
  public static void fixArrays(Node root, String[] names, VarInfo[] varInfos) {
    ArrayFixer fixer = new ArrayFixer(names, varInfos);
    root.accept(fixer);
    fixer.fixLastToken();
  }

  /**
   * This method should not be directly used by users of this class; however, must be public by to
   * fulfill Visitor interface. Adds "_identity" or "_array" if needed at this node token.
   */
  @Override
  public void visit(NodeToken n) {
    if (lastTokenMayBeIdentity && !(Visitors.isLBracket(n) || Visitors.isDot(n))) {
      assert lastToken != null
          : "@AssumeAssertion(nullness): dependent: because lastTokenMayBeIdentity == true";
      lastToken.tokenImage = lastToken.tokenImage + "_identity";
      lastToken.endColumn = lastToken.endColumn + 9;
      columnshift = columnshift + 9;
      columnshiftline = lastToken.beginLine;
    } else if (lastTokenMayBeElements && (Visitors.isLBracket(n) || Visitors.isDot(n))) {
      assert lastToken != null
          : "@AssumeAssertion(nullness): dependent: because lastTokenMayBeElements == true";
      lastToken.tokenImage = lastToken.tokenImage + "_array";
      lastToken.endColumn = lastToken.endColumn + 6;
      columnshift = columnshift + 6;
      columnshiftline = lastToken.beginLine;
    }
    lastTokenMayBeIdentity = false;
    lastTokenMayBeElements = false;
    if (Visitors.isIdentifier(n)) {
      for (int i = 0; i < varInfos.length; i++) {
        if (varInfos[i].type.isArray()) {
          String varName = varNames[i];
          if (varInfos[i].file_rep_type == ProglangType.HASHCODE) {
            if (varName.equals(n.tokenImage)) {
              lastTokenMayBeIdentity = true;
            }
          } else if (varName.equals(n.tokenImage)) {
            lastTokenMayBeElements = true;
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
  }

  private void fixLastToken() {
    if (lastTokenMayBeIdentity) {
      assert lastToken != null
          : "@AssumeAssertion(nullness): dependent: because lastTokenMayBeIdentity == true";
      lastToken.tokenImage = lastToken.tokenImage + "_identity";
      lastToken.endColumn = lastToken.endColumn + 9;
      columnshift = columnshift + 9;
      columnshiftline = lastToken.beginLine;
    }
  }
}

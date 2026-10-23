package daikon.split;

import daikon.ProglangType;
import daikon.VarInfo;
import daikon.tools.jtb.Ast;
import java.util.LinkedHashSet;
import java.util.Set;
import jtb.ParseException;
import jtb.syntaxtree.ArgumentList;
import jtb.syntaxtree.CastExpression;
import jtb.syntaxtree.ConditionalExpression;
import jtb.syntaxtree.Node;
import jtb.syntaxtree.NodeChoice;
import jtb.syntaxtree.NodeList;
import jtb.syntaxtree.NodeListOptional;
import jtb.syntaxtree.NodeOptional;
import jtb.syntaxtree.NodeSequence;
import jtb.syntaxtree.NodeToken;
import jtb.syntaxtree.PrimaryExpression;
import jtb.syntaxtree.PrimaryPrefix;
import jtb.visitor.DepthFirstVisitor;
import org.checkerframework.checker.nullness.qual.MonotonicNonNull;
import org.checkerframework.checker.nullness.qual.Nullable;

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
 *
 * An array that is passed to a method is given the "_array" suffix, so the method receives the
 * elements of the array. For example, {@code Arrays.equals(this_a, (this_b))} would be changed to
 * {@code Arrays.equals(this_a_array, (this_b_array))}. (QuantFixer has already replaced the calls
 * to daikon.Quant methods that access arrays.)
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
   * lastTokenElements or lastTokenIdentity is non-null.
   */
  private @MonotonicNonNull NodeToken lastToken;

  /**
   * The variable for the elements of the array named by the last token visited, which needs the
   * "_array" suffix; or null if there is none.
   */
  private @Nullable VarInfo lastTokenElements = null;

  /**
   * The variable for the identity of the array named by the last token visited, which needs the
   * "_identity" suffix; or null if there is none.
   */
  private @Nullable VarInfo lastTokenIdentity = null;

  /** Descriptions of the arrays that cannot be translated. */
  private final Set<String> errors = new LinkedHashSet<>();

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
   * @throws ParseException when condition is not a valid segment of java code, or when an array
   *     whose elements are not available is passed to a method
   */
  public static String fixArrays(String expression, String[] names, VarInfo[] varInfos)
      throws ParseException {
    Node root = Visitors.getJtbTree(expression);
    ArrayFixer fixer = new ArrayFixer(names, varInfos);
    root.accept(fixer);
    fixer.fixLastToken(false);
    if (!fixer.errors.isEmpty()) {
      throw new ParseException(String.join(System.lineSeparator(), fixer.errors));
    }
    return Ast.format(root);
  }

  /**
   * This method should not be directly used by users of this class; however, must be public to
   * fulfill the Visitor interface. Adds "_identity" or "_array" if needed at the previous node
   * token.
   */
  @Override
  public void visit(NodeToken n) {
    fixLastToken(Visitors.isLBracket(n) || Visitors.isDot(n));
    lastTokenIdentity = null;
    lastTokenElements = null;
    if (Visitors.isIdentifier(n)) {
      lastTokenIdentity = findArrayVar(n.tokenImage, true);
      lastTokenElements = findArrayVar(n.tokenImage, false);
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

  /**
   * Returns the array variable whose base name is name.
   *
   * @param name a base name, as it appears in the condition
   * @param identity if true, return the variable for the identity of the array; if false, return
   *     the variable for its elements
   * @return the array variable whose base name is name, or null if there is none
   */
  private @Nullable VarInfo findArrayVar(String name, boolean identity) {
    for (int i = 0; i < varInfos.length; i++) {
      if (varInfos[i].type.isArray()
          && (varInfos[i].file_rep_type == ProglangType.HASHCODE) == identity
          && varNames[i].equals(name)) {
        return varInfos[i];
      }
    }
    return null;
  }

  /**
   * Adds "_identity" or "_array" to the last token visited, if needed. The "_array" suffix is
   * needed if the elements of the array are accessed or if the array is passed to a method;
   * otherwise, the "_identity" suffix is needed. If the elements are needed but the program point
   * has no variable for them, records an error instead.
   *
   * @param nextIsSelector true if the next token is "[" or "."
   */
  private void fixLastToken(boolean nextIsSelector) {
    if (lastTokenIdentity == null && lastTokenElements == null) {
      return;
    }
    assert lastToken != null
        : "@AssumeAssertion(nullness): dependent: because lastTokenIdentity or lastTokenElements is"
            + " non-null";
    Use use = nextIsSelector ? Use.SELECTED : use(lastToken);
    boolean isArgument = use == Use.ARGUMENT;
    if (use != Use.OTHER) {
      if (lastTokenElements != null) {
        addSuffix(lastToken, "_array");
      } else {
        assert lastTokenIdentity != null
            : "@AssumeAssertion(nullness): lastTokenElements is null, so lastTokenIdentity is not";
        String what =
            isArgument
                ? "pass the array " + lastTokenIdentity.name() + " to a method"
                : "use the elements or length of the array " + lastTokenIdentity.name();
        errors.add(
            "Cannot " + what + ", because the program point has no variable for its elements");
      }
    } else if (lastTokenIdentity != null) {
      addSuffix(lastToken, "_identity");
    }
  }

  /**
   * Appends suffix to the image of token, and shifts the columns of the following tokens on the
   * same line.
   *
   * @param token the token to modify
   * @param suffix the suffix to append
   */
  private void addSuffix(NodeToken token, String suffix) {
    token.tokenImage = token.tokenImage + suffix;
    token.endColumn = token.endColumn + suffix.length();
    columnshift = columnshift + suffix.length();
    columnshiftline = token.beginLine;
  }

  /** How the value of an expression is used. */
  private enum Use {
    /** The value is passed to a method or constructor. */
    ARGUMENT,
    /** The value is followed by a selector, such as "[...]" or ".length". */
    SELECTED,
    /** Any other use. */
    OTHER
  }

  /**
   * Returns how the value of token is used. The value of token is also the value of an enclosing
   * expression that puts it within parentheses, within a cast, or as a branch of a conditional
   * expression.
   *
   * @param token a token
   * @return how the value of token is used
   */
  @SuppressWarnings("interning:not.interned") // syntax tree nodes are compared by identity
  private static Use use(NodeToken token) {
    // The node whose value is the value of token.
    Node value = token;
    Node child = token;
    for (Node parent = token.getParent(); parent != null; parent = parent.getParent()) {
      if (parent instanceof ArgumentList) {
        return Use.ARGUMENT;
      }
      if (parent instanceof PrimaryExpression
          && child == ((PrimaryExpression) parent).f0
          && ((PrimaryExpression) parent).f1.present()) {
        return Use.SELECTED;
      }
      if (!(parent instanceof NodeChoice
          || parent instanceof NodeList
          || parent instanceof NodeListOptional
          || parent instanceof NodeOptional
          || parent instanceof NodeSequence)) {
        boolean sameValue =
            hasSameTokens(parent, value)
                // `value` is a branch, not the condition.
                || (parent instanceof ConditionalExpression
                    && child == ((ConditionalExpression) parent).f1)
                // `value` is the operand, not the type.
                || (parent instanceof CastExpression && lastTokenOf(parent) == lastTokenOf(value))
                // The choice "(" Expression() ")".
                || (parent instanceof PrimaryPrefix && ((PrimaryPrefix) parent).f0.which == 4);
        if (!sameValue) {
          return Use.OTHER;
        }
        value = parent;
      }
      child = parent;
    }
    return Use.OTHER;
  }

  /**
   * Returns true if n1 and n2 consist of the same tokens.
   *
   * @param n1 a node
   * @param n2 a node
   * @return true if n1 and n2 consist of the same tokens
   */
  @SuppressWarnings("interning:not.interned") // syntax tree nodes are compared by identity
  private static boolean hasSameTokens(Node n1, Node n2) {
    NodeToken[] tokens1 = TokenExtractor.extractTokens(n1);
    NodeToken[] tokens2 = TokenExtractor.extractTokens(n2);
    // A node's tokens are a contiguous run of the token stream, so equal endpoints imply equal
    // tokens.
    return tokens1[0] == tokens2[0] && tokens1[tokens1.length - 1] == tokens2[tokens2.length - 1];
  }

  /**
   * Returns the last token of n.
   *
   * @param n a node
   * @return the last token of n
   */
  private static NodeToken lastTokenOf(Node n) {
    NodeToken[] tokens = TokenExtractor.extractTokens(n);
    return tokens[tokens.length - 1];
  }
}

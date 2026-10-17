package daikon.split;

import daikon.ProglangType;
import daikon.VarInfo;
import daikon.tools.jtb.Ast;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Set;
import jtb.ParseException;
import jtb.syntaxtree.EqualityExpression;
import jtb.syntaxtree.Node;
import jtb.syntaxtree.NodeSequence;
import jtb.syntaxtree.NodeToken;
import jtb.visitor.DepthFirstVisitor;

/**
 * NullReplacer is a JTB syntax tree visitor that replaces instances of "null" with "0" in a given
 * expression. Note: "null" is only referring to the java reserved word "null" not to any instances
 * of the string "null".
 *
 * <p>A splitter represents a reference by its hashcode, which is 0 for null, except that it
 * represents a String, a String array, or a char array as a reference. Therefore, a "null" that is
 * compared with an expression is replaced only if the splitter represents that expression by a
 * hashcode. For example, in {@code this_next == null && this_name != null}, where {@code this_next}
 * is a hashcode and {@code this_name} is a String, only the first "null" is replaced. A "null" that
 * is not an operand of {@code ==} or {@code !=} is always replaced.
 *
 * <p>NullReplacer runs after the other fixers, because it recognizes the names that they produce.
 */
class NullReplacer extends DepthFirstVisitor {

  private int columnshift = 0;
  private int columnshiftline = -1;

  // column shifting only applies to a single line, then is turned off again.
  // States for the variables:
  // columnshift == 0, columnshiftline == -1:
  //    no column shifting needed
  // columnshift != 0, columnshiftline != -1:
  //    column shifting being needed, applies only to specified line

  /** The compilable names of the variables that the splitter represents by a hashcode. */
  private final Set<String> hashcodeNames = new HashSet<>();

  /** The compilable names of the arrays whose elements the splitter represents by hashcodes. */
  private final Set<String> hashcodeArrayNames = new HashSet<>();

  /** The null literals that are not replaced, because they are compared with a reference. */
  private final Set<NodeToken> referenceNulls = Collections.newSetFromMap(new IdentityHashMap<>());

  /**
   * Creates a new NullReplacer.
   *
   * @param varInfos the VarInfos of the variables that may appear in the expression
   */
  private NullReplacer(VarInfo[] varInfos) {
    super();
    for (VarInfo varInfo : varInfos) {
      if (varInfo.file_rep_type == ProglangType.HASHCODE) {
        hashcodeNames.add(SplitterJavaSource.compilableName(varInfo));
      } else if (varInfo.file_rep_type == ProglangType.HASHCODE_ARRAY) {
        hashcodeArrayNames.add(SplitterJavaSource.compilableName(varInfo));
      }
    }
  }

  /**
   * Replaces instances of "null" with "0" (see class description).
   *
   * @param expression a valid java expression, whose variables have their compilable names
   * @param varInfos the VarInfos of the variables that may appear in the expression
   * @return expression with instances of null replaced by instances of "0"
   * @throws ParseException if expression is not a valid java expression
   */
  public static String replaceNull(String expression, VarInfo[] varInfos) throws ParseException {
    Node root = Visitors.getJtbTree(expression);
    NullReplacer replacer = new NullReplacer(varInfos);
    root.accept(replacer);
    return Ast.format(root);
  }

  /**
   * This method should not be directly used by user of this class; however it must be public to
   * fulfill the visitor interface. Records the null literals in n that are compared with an
   * expression that the splitter does not represent by a hashcode.
   */
  @Override
  public void visit(EqualityExpression n) {
    List<NodeToken[]> operands = new ArrayList<>();
    operands.add(TokenExtractor.extractTokens(n.f0));
    for (Node operatorAndOperand : n.f1.nodes) {
      operands.add(TokenExtractor.extractTokens(((NodeSequence) operatorAndOperand).elementAt(1)));
    }
    for (int i = 0; i + 1 < operands.size(); i++) {
      NodeToken[] left = operands.get(i);
      NodeToken[] right = operands.get(i + 1);
      if (isNullLiteral(left) && !isHashcode(right)) {
        referenceNulls.add(left[0]);
      } else if (isNullLiteral(right) && !isHashcode(left)) {
        referenceNulls.add(right[0]);
      }
    }
    super.visit(n);
  }

  /**
   * Returns true if tokens is the null literal.
   *
   * @param tokens the tokens of an expression
   * @return true if tokens is the null literal
   */
  private static boolean isNullLiteral(NodeToken[] tokens) {
    return tokens.length == 1 && Visitors.isNull(tokens[0]);
  }

  /**
   * Returns true if the splitter represents the expression whose tokens are given by a hashcode:
   * that is, if the expression is (possibly in parentheses) a variable that the splitter represents
   * by a hashcode, an element of an array of hashcodes such as {@code a[i]}, or a call {@code
   * getElement_Object(a, i)} on such an array.
   *
   * @param tokens the tokens of an expression
   * @return true if the splitter represents the expression by a hashcode
   */
  private boolean isHashcode(NodeToken[] tokens) {
    int start = 0;
    int end = tokens.length - 1;
    while (end > start
        && tokens[start].tokenImage.equals("(")
        && matchingIndex(tokens, start) == end) {
      start++;
      end--;
    }
    if (start == end) {
      return hashcodeNames.contains(tokens[start].tokenImage);
    }
    if (end - start >= 3
        && hashcodeArrayNames.contains(tokens[start].tokenImage)
        && tokens[start + 1].tokenImage.equals("[")
        && matchingIndex(tokens, start + 1) == end) {
      return true;
    }
    return end - start >= 5
        && tokens[start].tokenImage.equals("getElement_Object")
        && tokens[start + 1].tokenImage.equals("(")
        && hashcodeArrayNames.contains(tokens[start + 2].tokenImage)
        && tokens[start + 3].tokenImage.equals(",")
        && matchingIndex(tokens, start + 1) == end;
  }

  /**
   * Returns the index of the token that closes the parenthesis or bracket at index open.
   *
   * @param tokens the tokens of an expression
   * @param open the index of a "(" or "[" token in tokens
   * @return the index of the matching ")" or "]" token, or -1 if there is none
   */
  private static int matchingIndex(NodeToken[] tokens, int open) {
    int depth = 0;
    for (int i = open; i < tokens.length; i++) {
      String image = tokens[i].tokenImage;
      if (image.equals("(") || image.equals("[")) {
        depth++;
      } else if (image.equals(")") || image.equals("]")) {
        depth--;
        if (depth == 0) {
          return i;
        }
      }
    }
    return -1;
  }

  /**
   * This method should not be directly used by user of this class; however it must be public to
   * fulfill the visitor interface. If n represents null then it is replaced by "0", unless n is
   * compared with an expression that the splitter does not represent by a hashcode.
   */
  @Override
  public void visit(NodeToken n) {
    if (n.beginLine == columnshiftline) {
      n.beginColumn = n.beginColumn - columnshift;
    } else {
      columnshift = 0;
      columnshiftline = -1;
    }
    if (Visitors.isNull(n) && !referenceNulls.contains(n)) {
      columnshift = columnshift + 3;
      n.tokenImage = "0";
      columnshiftline = n.beginLine;
      n.kind = Visitors.STRING_LITERAL;
    }
    n.endColumn = n.endColumn + columnshift;
  }
}

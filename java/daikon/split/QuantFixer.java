package daikon.split;

import daikon.VarInfo;
import daikon.tools.jtb.Ast;
import java.util.Map;
import java.util.Set;
import jtb.ParseException;
import jtb.syntaxtree.ArgumentList;
import jtb.syntaxtree.Arguments;
import jtb.syntaxtree.Name;
import jtb.syntaxtree.Node;
import jtb.syntaxtree.NodeSequence;
import jtb.syntaxtree.NodeToken;
import jtb.syntaxtree.PrimaryExpression;
import jtb.syntaxtree.PrimarySuffix;
import jtb.visitor.DepthFirstVisitor;
import org.checkerframework.checker.nullness.qual.Nullable;

/**
 * QuantFixer is a visitor for a jtb syntax tree that replaces calls to the daikon.Quant methods
 * that access arrays, which appear in Daikon's Java output format, by calls to the methods of the
 * same name in {@link Splitter}, which take an array as a splitter represents it. For example, if
 * {@code this_a} is the base name of an int array, then {@code daikon.Quant.getElement_int(this_a,
 * i)} yields {@code getElement_int(this_a_array, i)}, and {@code daikon.Quant.size(this_a)} yields
 * {@code size(this_a_array)}.
 *
 * <p>QuantFixer runs after the fixers that convert variable names to base names, such as {@link
 * PrefixFixer}, and before {@link ArrayFixer}, which would convert the array arguments to their
 * identities. A call to a daikon.Quant method whose arguments include an array cannot be evaluated
 * correctly on an array's identity, so QuantFixer reports an error for such a call that it does not
 * replace, such as one whose array argument is not the base name of an array whose elements the
 * splitter represents.
 */
class QuantFixer extends DepthFirstVisitor {

  /** Maps the base name of each array variable whose elements the splitter represents to it. */
  private final Map<String, VarInfo> arrays;

  /** The base names of all the array variables, including those represented only by identity. */
  private final Set<String> allArrays;

  /** A description of a call that QuantFixer cannot replace, or null if there is none. */
  private @Nullable String error = null;

  /**
   * Creates a new QuantFixer.
   *
   * @param arrays maps the base name of each array variable whose elements the splitter represents
   *     to its VarInfo
   * @param allArrays the base names of all the array variables
   */
  private QuantFixer(Map<String, VarInfo> arrays, Set<String> allArrays) {
    super();
    this.arrays = arrays;
    this.allArrays = allArrays;
  }

  /**
   * Replaces calls to daikon.Quant methods that access arrays (see class description).
   *
   * @param expression a valid segment of Java code
   * @param arrays maps the base name of each array variable whose elements the splitter represents
   *     to its VarInfo
   * @param allArrays the base names of all the array variables
   * @return expression, with calls to daikon.Quant array methods replaced
   * @throws ParseException if expression is not valid Java code, or if it contains a call to a
   *     daikon.Quant method on an array that cannot be replaced
   */
  public static String fixQuant(
      String expression, Map<String, VarInfo> arrays, Set<String> allArrays) throws ParseException {
    if (!expression.contains("daikon.Quant")) {
      return expression;
    }
    Node root = Visitors.getJtbTree(expression);
    QuantFixer fixer = new QuantFixer(arrays, allArrays);
    root.accept(fixer);
    if (fixer.error != null) {
      throw new ParseException(fixer.error);
    }
    return Ast.format(root);
  }

  /**
   * This method should not be directly used by users of this class. If n is a call to a
   * daikon.Quant method that accesses an array, it is replaced.
   */
  @Override
  public void visit(PrimaryExpression n) {
    // Visit the arguments first, so that the check of n's arguments below does not see the array
    // arguments of nested calls that are replaced.
    super.visit(n);
    String methodName = quantMethodName(n);
    if (methodName == null) {
      return;
    }
    Arguments args = (Arguments) ((PrimarySuffix) n.f1.elementAt(0)).f0.choice;
    boolean accessesArray = methodName.equals("size") || methodName.startsWith("getElement_");
    if (accessesArray && fixQuantCall(n, args)) {
      return;
    }
    if (accessesArray || mentionsArray(args)) {
      error =
          "Cannot translate a call to daikon.Quant."
              + methodName
              + " whose arguments are "
              + Ast.format(args);
    }
  }

  /**
   * This method should not be directly used by users of this class. Clears the position of n,
   * because changes to token images invalidate the positions.
   */
  @Override
  public void visit(NodeToken n) {
    n.beginColumn = -1;
    n.endColumn = -1;
  }

  /**
   * Replaces n, a call to a daikon.Quant method that accesses an array, if the array argument is
   * the base name of an array whose elements the splitter represents.
   *
   * @param n a call to a daikon.Quant method that accesses an array
   * @param args the arguments of n
   * @return true if n was replaced
   */
  private boolean fixQuantCall(PrimaryExpression n, Arguments args) {
    if (!args.f1.present()) {
      return false;
    }
    NodeToken[] arrayTokens = TokenExtractor.extractTokens(((ArgumentList) args.f1.node).f0);
    if (arrayTokens.length != 1) {
      return false;
    }
    VarInfo array = arrays.get(arrayTokens[0].tokenImage);
    if (array == null) {
      return false;
    }
    // Remove "daikon.Quant.", leaving the method name.
    NodeToken[] methodTokens = TokenExtractor.extractTokens(n.f0);
    for (int i = 0; i < methodTokens.length - 1; i++) {
      methodTokens[i].tokenImage = "";
    }
    arrayTokens[0].tokenImage = SplitterJavaSource.compilableName(array);
    return true;
  }

  /**
   * Returns true if some argument in args is the base name of an array. {@link ArrayFixer} converts
   * such an argument to the array's identity.
   *
   * @param args the arguments of a method call
   * @return true if some argument in args is an array
   */
  private boolean mentionsArray(Arguments args) {
    if (!args.f1.present()) {
      return false;
    }
    ArgumentList argList = (ArgumentList) args.f1.node;
    if (isArray(argList.f0)) {
      return true;
    }
    for (Node commaAndArg : argList.f1.nodes) {
      if (isArray(((NodeSequence) commaAndArg).elementAt(1))) {
        return true;
      }
    }
    return false;
  }

  /**
   * Returns true if arg is the base name of an array.
   *
   * @param arg an argument of a method call
   * @return true if arg is the base name of an array
   */
  private boolean isArray(Node arg) {
    NodeToken[] tokens = TokenExtractor.extractTokens(arg);
    return tokens.length == 1 && allArrays.contains(tokens[0].tokenImage);
  }

  /**
   * If n is a call to a method of daikon.Quant, returns the method name. Otherwise, returns null.
   *
   * @param n an expression
   * @return the name of the daikon.Quant method that n calls, or null
   */
  private static @Nullable String quantMethodName(PrimaryExpression n) {
    if (!(n.f0.f0.choice instanceof Name)
        || n.f1.size() == 0
        || !(((PrimarySuffix) n.f1.elementAt(0)).f0.choice instanceof Arguments)) {
      return null;
    }
    Name name = (Name) n.f0.f0.choice;
    if (!name.f0.tokenImage.equals("daikon") || name.f1.size() < 2) {
      return null;
    }
    NodeToken classToken = (NodeToken) ((NodeSequence) name.f1.elementAt(0)).elementAt(1);
    if (!classToken.tokenImage.equals("Quant")) {
      return null;
    }
    // For a call to a method of a nested class, such as daikon.Quant.fuzzy.eq, the method name
    // includes the nested class name.
    StringBuilder methodName = new StringBuilder();
    for (int i = 1; i < name.f1.size(); i++) {
      if (i > 1) {
        methodName.append(".");
      }
      methodName.append(
          ((NodeToken) ((NodeSequence) name.f1.elementAt(i)).elementAt(1)).tokenImage);
    }
    return methodName.toString();
  }
}

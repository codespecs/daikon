package daikon.split;

import daikon.ProglangType;
import daikon.VarInfo;
import daikon.tools.jtb.Ast;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
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
 * identities. QuantFixer reports an error for a call to a daikon.Quant method that accesses an
 * array but that it cannot replace: one whose first argument is not the base name of an array whose
 * elements the splitter represents, or one whose element type differs from the array's. The
 * splitter's representation of an array does not distinguish some element types (for example, int
 * and boolean arrays are both represented as long[]), so without the check a mismatched call would
 * compile and quietly evaluate to a meaningless value.
 */
class QuantFixer extends DepthFirstVisitor {

  /** Maps the base name of each array variable whose elements the splitter represents to it. */
  private final Map<String, VarInfo> arrays = new HashMap<>();

  /** Descriptions of the calls that QuantFixer cannot replace. */
  private final List<String> errors = new ArrayList<>();

  /**
   * The prefix of the placeholders that stand for the array arguments of the replaced calls. Neither
   * the expression nor any base name contains it.
   */
  private final String placeholderPrefix;

  /**
   * The compilable names of the array arguments of the replaced calls. The array argument of a
   * replaced call is the placeholder {@link #placeholderPrefix} followed by its index in this list.
   */
  private final List<String> arrayNames = new ArrayList<>();

  /**
   * Creates a new QuantFixer.
   *
   * @param expression the expression that this will visit
   * @param baseNames the base names of the variables that may appear in the expression
   * @param varInfos the VarInfos of the variables; the ith element is the VarInfo for the ith
   *     element of baseNames
   */
  private QuantFixer(String expression, String[] baseNames, VarInfo[] varInfos) {
    super();
    for (int i = 0; i < varInfos.length; i++) {
      if (varInfos[i].type.isArray() && varInfos[i].file_rep_type != ProglangType.HASHCODE) {
        arrays.put(baseNames[i], varInfos[i]);
      }
    }
    placeholderPrefix = choosePlaceholderPrefix(expression, baseNames);
  }

  /**
   * Returns a prefix for placeholders that neither expression nor any base name contains. Because
   * no base name contains the prefix, ArrayFixer does not change a placeholder.
   *
   * @param expression an expression
   * @param baseNames the base names of the variables that may appear in the expression
   * @return a prefix for placeholders
   */
  private static String choosePlaceholderPrefix(String expression, String[] baseNames) {
    String prefix = "daikon_quant_array_";
    while (containsPrefix(expression, baseNames, prefix)) {
      prefix = prefix + "x_";
    }
    return prefix;
  }

  /**
   * Returns true if expression or any base name contains prefix.
   *
   * @param expression an expression
   * @param baseNames base names of variables
   * @param prefix a candidate placeholder prefix
   * @return true if expression or any base name contains prefix
   */
  private static boolean containsPrefix(String expression, String[] baseNames, String prefix) {
    if (expression.contains(prefix)) {
      return true;
    }
    for (String baseName : baseNames) {
      if (baseName.contains(prefix)) {
        return true;
      }
    }
    return false;
  }

  /**
   * The result of {@link QuantFixer#fixQuant}: an expression in which the array arguments of the replaced
   * calls are placeholders, and a way to replace the placeholders by the arrays' compilable names.
   *
   * <p>The placeholders keep {@link ArrayFixer} from treating a compilable name as a base name. For
   * example, if a program point has int arrays a and a_array, then the compilable name of a is
   * a_array, which is also the base name of a_array; ArrayFixer would change it to a_array_array.
   */
  static final class Result {

    /** The expression, with placeholders for the array arguments of the replaced calls. */
    final String expression;

    /** The prefix of the placeholders. */
    private final String placeholderPrefix;

    /** The compilable names for the placeholders, in order. */
    private final List<String> arrayNames;

    /**
     * Creates a new Result.
     *
     * @param expression the expression, with placeholders
     * @param placeholderPrefix the prefix of the placeholders
     * @param arrayNames the compilable names for the placeholders, in order
     */
    private Result(String expression, String placeholderPrefix, List<String> arrayNames) {
      this.expression = expression;
      this.placeholderPrefix = placeholderPrefix;
      this.arrayNames = arrayNames;
    }

    /**
     * Replaces the placeholders in fixedExpression by the compilable names of the arrays that they
     * stand for.
     *
     * @param fixedExpression {@link #expression}, possibly modified in ways that do not change the
     *     placeholders
     * @return fixedExpression, with the placeholders replaced
     */
    String restoreArrayNames(String fixedExpression) {
      if (arrayNames.isEmpty()) {
        return fixedExpression;
      }
      Pattern placeholder =
          Pattern.compile("(?<![\\w$])" + Pattern.quote(placeholderPrefix) + "(\\d+)(?![\\w$])");
      Matcher m = placeholder.matcher(fixedExpression);
      StringBuilder sb = new StringBuilder();
      while (m.find()) {
        String index = m.group(1);
        assert index != null : "@AssumeAssertion(nullness): the group is not optional";
        m.appendReplacement(sb, Matcher.quoteReplacement(arrayNames.get(Integer.parseInt(index))));
      }
      m.appendTail(sb);
      return sb.toString();
    }
  }

  /**
   * Replaces calls to daikon.Quant methods that access arrays (see class description). In the
   * result, the array arguments of the replaced calls are placeholders; after {@link ArrayFixer}
   * runs, call {@link Result#restoreArrayNames} to replace them by the arrays' compilable names.
   *
   * @param expression a valid segment of Java code
   * @param baseNames the base names of the variables that may appear in the expression
   * @param varInfos the VarInfos of the variables; the ith element is the VarInfo for the ith
   *     element of baseNames
   * @return expression, with calls to daikon.Quant array methods replaced
   * @throws ParseException if expression is not valid Java code, or if it contains a call to a
   *     daikon.Quant method that accesses an array but cannot be replaced
   */
  public static Result fixQuant(String expression, String[] baseNames, VarInfo[] varInfos)
      throws ParseException {
    if (!expression.contains("daikon.Quant")) {
      return new Result(expression, "", new ArrayList<>());
    }
    Node root = Visitors.getJtbTree(expression);
    QuantFixer fixer = new QuantFixer(expression, baseNames, varInfos);
    root.accept(fixer);
    if (!fixer.errors.isEmpty()) {
      throw new ParseException(String.join(System.lineSeparator(), fixer.errors));
    }
    return new Result(Ast.format(root), fixer.placeholderPrefix, fixer.arrayNames);
  }

  /**
   * This method should not be directly used by users of this class. If n is a call to a
   * daikon.Quant method that accesses an array, it is replaced.
   */
  @Override
  public void visit(PrimaryExpression n) {
    // Visit the arguments first, so that nested calls are replaced.
    super.visit(n);
    String methodName = quantMethodName(n);
    if (methodName == null
        || !(methodName.equals("size") || methodName.startsWith("getElement_"))) {
      return;
    }
    Arguments args = (Arguments) ((PrimarySuffix) n.f1.elementAt(0)).f0.choice;
    String error = fixQuantCall(n, methodName, args);
    if (error != null) {
      errors.add("Cannot translate daikon.Quant." + methodName + Ast.format(args) + ": " + error);
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
   * the base name of an array whose elements the splitter represents and whose element type is the
   * one that the method expects.
   *
   * @param n a call to a daikon.Quant method that accesses an array
   * @param methodName the name of the method that n calls
   * @param args the arguments of n
   * @return null if n was replaced, or else the reason that n cannot be replaced
   */
  private @Nullable String fixQuantCall(PrimaryExpression n, String methodName, Arguments args) {
    NodeToken[] arrayTokens =
        args.f1.present()
            ? TokenExtractor.extractTokens(((ArgumentList) args.f1.node).f0)
            : new NodeToken[0];
    VarInfo array = (arrayTokens.length == 1) ? arrays.get(arrayTokens[0].tokenImage) : null;
    if (array == null) {
      return "the first argument is not an array whose elements are available to the splitter";
    }
    if (methodName.startsWith("getElement_")
        && !hasElementType(array, methodName.substring("getElement_".length()))) {
      return "the element type of " + array.name() + " is not the one that the method expects";
    }
    // Remove "daikon.Quant.", leaving the method name.
    NodeToken[] methodTokens = TokenExtractor.extractTokens(n.f0);
    for (int i = 0; i < methodTokens.length - 1; i++) {
      methodTokens[i].tokenImage = "";
    }
    // A placeholder, which fixQuant's caller replaces by the compilable name after ArrayFixer runs.
    arrayTokens[0].tokenImage = placeholderPrefix + arrayNames.size();
    arrayNames.add(SplitterJavaSource.compilableName(array));
    return null;
  }

  /**
   * Returns true if the elements of array have the given type, as the name of a daikon.Quant
   * getElement_ method expresses it. Daikon's Java output format uses getElement_Object for every
   * array whose elements are not primitives, including a String array.
   *
   * @param array a one-dimensional array variable
   * @param elementType the suffix of the name of a daikon.Quant getElement_ method, such as "int",
   *     "String", or "Object"
   * @return true if the elements of array have type elementType
   */
  private static boolean hasElementType(VarInfo array, String elementType) {
    if (array.type.dimensions() != 1) {
      return false;
    }
    String base = array.type.base();
    switch (elementType) {
      case "Object":
        return !array.type.baseIsPrimitive();
      case "String":
        return base.equals("java.lang.String") || base.equals("String");
      default:
        return base.equals(elementType);
    }
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

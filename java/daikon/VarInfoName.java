package daikon;

import daikon.inv.Invariant.OutputFormat;

import utilMDE.*;

import org.apache.log4j.Category;

import java.lang.ref.WeakReference;
import java.io.Serializable;
import java.util.*;
import java.lang.reflect.*;

/**
 * VarInfoName is an type which represents the "name" of a variable.
 * Calling it a "name", however, is somewhat misleading.  It can be
 * some expression which includes more than one variable, term, etc.
 * We separate this from the VarInfo itself because clients wish to
 * manipulate names into new expressions independent of the VarInfo
 * which they might be associated with.  VarInfoName's child classes
 * are specific types of names, like applying a function to something.
 * For example, "a" is a name, and "sin(a)" is a name that is the name
 * "a" with the function "sin" applied to it.
 **/
public abstract class VarInfoName
  implements Serializable, Comparable
{

  /**
   * Debugging Category
   **/

  public static Category debug = Category.getInstance(VarInfoName.class.getName());

  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  /**
   * Given the standard String representation of a variable name (from
   * a decls file), return the corresponding VarInfoName.  This cannot
   * handle generalized expressions, so name.equals(parse(e.name()))
   * is not certain to be true.
   **/
  public static VarInfoName parse(String name) {
    // x.class
    if (name.endsWith(".class")) {
      return parse(name.substring(0, name.length()-6)).applyTypeOf();
    }

    // x or this.x
    if ((name.indexOf('[') == -1) && (name.indexOf('(') == -1)) {
      // checking for only legal characters would be more robust
      int dot = name.lastIndexOf('.');
      if (dot >= 0) {
	String first = name.substring(0, dot);
	String field = name.substring(dot+1);
	return parse(first).applyField(field);
      } else {
	return (new Simple(name)).intern();
      }
    }

    // a[]
    if (name.endsWith("[]")) {
      return parse(name.substring(0, name.length()-2)).applyElements();
    }

    // foo[bar] -- IOA input only (pre-derived)
    if (name.endsWith("]")) {
      int lbracket = name.lastIndexOf("[");
      if (lbracket >= 0) {
	String seqname = name.substring(0, lbracket) + "[]";
	String idxname = name.substring(lbracket + 1, name.length() - 1);
	VarInfoName seq = parse(seqname);
	VarInfoName idx = parse(idxname);
	return seq.applySubscript(idx);
      }
    }

    // a[].foo or a[].foo.bar
    if (name.indexOf("[]") >= 0) {
      int brackets = name.lastIndexOf("[]");
      int dot = name.lastIndexOf('.');
      if (dot >= brackets) {
	String first = name.substring(0, dot);
	String field = name.substring(dot+1);
	return parse(first).applyField(field);
      }
    }

    // ??
    throw new UnsupportedOperationException("parse error: '" + name + "'");
  }

  /**
   * Return the String representation of this name in the default
   * output format.
   * @return the string representation (interned) of this name, in the
   * default output format
   **/
  public String name() {
    if (name_cached == null) {
      try {
	name_cached = name_impl().intern();
      } catch (RuntimeException e) {
	System.err.println("repr = " + repr());
	throw e;
      }
    }
    return name_cached;
  }
  private String name_cached = null;
  protected abstract String name_impl();

  /**
   * Return the String representation of this name in the esc style
   * output format
   * @return the string representation (interned) of this name, in the
   * esc style output format
   **/
  public String esc_name() {
    if (esc_name_cached == null) {
      try {
	esc_name_cached = esc_name_impl().intern();
      } catch (RuntimeException e) {
	System.err.println("repr = " + repr());
	throw e;
      }
    }
    // System.out.println("esc_name = " + esc_name_cached + " for " + name() + " of class " + this.getClass().getName());
    return esc_name_cached;
  }
  private String esc_name_cached = null;
  protected abstract String esc_name_impl();
  
  /**
   * @return the string representation (interned) of this name, in the
   * Simplify tool output format
   **/
  public String simplify_name() {
    return simplify_name(false);
  }

  /**
   * @return the string representation (interned) of this name, in the
   * Simplify tool output format, in the given pre/post-state context.
   **/
  protected String simplify_name(boolean prestate) {
    int which = prestate ? 0 : 1;
    if (simplify_name_cached[which] == null) {
      try {
	simplify_name_cached[which] = simplify_name_impl(prestate).intern();
      } catch (RuntimeException e) {
	System.err.println("repr = " + repr());
	throw e;
      }
    }
    return simplify_name_cached[which];
  }
  private String simplify_name_cached[] = new String[2];
  protected abstract String simplify_name_impl(boolean prestate);


  /**
   * Return the string representation of this name in IOA format
   * @return the string representation (interned) of this name, in the
   * IOA style output format
   * @param classname Name of the class of this variable so we can
   * remove it for IOA output.
   **/
  public String ioa_name() {
    if (debug.isDebugEnabled()) {
      debug.debug ("ioa_name: " + this.toString());
    }

    if (ioa_name_cached == null) {
      try {
	ioa_name_cached = ioa_name_impl().intern();
      } catch (RuntimeException e) {
	System.err.println("name = " + name());
	throw e;
      }
    }
    return ioa_name_cached;
  }
  private String ioa_name_cached = null;

  /**
   * Called in subclasses to return the internal implementation of
   * ioa_name.
   * @param classname Name of the class of this variable so we can
   * remove it for IOA output.
   *
   **/
  protected abstract String ioa_name_impl();

  /**
   * Return the String representation of this name in the java style
   * output format
   * @return the string representation (interned) of this name, in the
   * java style output format
   **/
  public String java_name() {
    if (java_name_cached == null) {
      try {
	java_name_cached = java_name_impl().intern();
      } catch (RuntimeException e) {
	System.err.println("repr = " + repr());
	throw e;
      }
    }
    return java_name_cached;
  }
  private String java_name_cached = null;
  protected abstract String java_name_impl();

  /**
   * @return name of this in the specified format
   **/
  public String name_using(OutputFormat format) {
    if (format == OutputFormat.DAIKON) return name();
    if (format == OutputFormat.SIMPLIFY) return simplify_name();
    if (format == OutputFormat.ESCJAVA) return esc_name();
    if (format == OutputFormat.JAVA) return java_name();
    if (format == OutputFormat.IOA) return ioa_name();
    throw new UnsupportedOperationException
      ("Unknown format requested: " + format);
  }

  /**
   * @return the string reprenentation (interned) of this name, in a
   * debugging format
   **/
  public String repr() {
    // AAD: Used to be interned for space reasons, but removed during
    // profiling when it was determined that the interns are unique
    // anyway.
    if (repr_cached == null) {
      repr_cached = repr_impl();//.intern();
    }
    return repr_cached;
  }
  private String repr_cached = null;
  protected abstract String repr_impl();

  // It would be nice if a generalized form of the mechanics of
  // interning were abstracted out somewhere.
  private static final WeakHashMap internTable = new WeakHashMap();
  public VarInfoName intern() {
    Object lookup = internTable.get(this);
    if (lookup != null) {
      WeakReference ref = (WeakReference)lookup;
      return (VarInfoName)ref.get();
    } else {
      internTable.put(this, new WeakReference(this));
      return this;
    }
  }

  // Not sure exactly where these belong in this file.
  protected abstract Class resolveType(PptTopLevel ppt);
  protected abstract java.lang.reflect.Field resolveField(PptTopLevel ppt);


  // ============================================================
  // Helpful constants

  public static final VarInfoName ZERO = parse("0");

  // ============================================================
  // Interesting observers

  /**
   * @return true when this is "0", "-1", "1", etc.
   **/
  public boolean isLiteralConstant() {
    return false;
  }

  /**
   * @return the nodes of this, as given by an inorder traversal.
   **/
  public Collection inOrderTraversal() {
    return Collections.unmodifiableCollection(new InorderFlattener(this).nodes());
  }

  /**
   * @return true iff the given node can be found in this.  If the
   * node has children, the whole subtree must match.
   **/
  public boolean hasNode(VarInfoName node) {
    return inOrderTraversal().contains(node);
  }

  /**
   * @return true iff a node of the given type exists in this
   **/
  public boolean hasNodeOfType(Class type) {
    Iterator nodes = inOrderTraversal().iterator();
    while (nodes.hasNext()) {
      if (type.equals(nodes.next().getClass())) {
	return true;
      }
    }
    return false;
  }

  /**
   * @return true if the given node is in a prestate context within
   * this tree; the node must be a member of this tree.
   **/
  public boolean inPrestateContext(VarInfoName node) {
    return (new NodeFinder(this, node)).inPre();
  }

  // ============================================================
  // Special producers, or other helpers

  public VarInfoName replaceAll(VarInfoName node, VarInfoName replacement) {
    if (node == replacement)
      return this;
    Assert.assert(! replacement.hasNode(node)); // no infinite loop
    VarInfoName result = this;
    Replacer r = new Replacer(node, replacement);
    while (result.hasNode(node)) {
      result = r.replace(result).intern();
    }
    return result;
  }

  // ============================================================
  // The usual Object methods

  public boolean equals(Object o) {
    return (o instanceof VarInfoName) && equals((VarInfoName) o);
  }

  public boolean equals(VarInfoName other) {
    return (other == this) || ((other != null) && (this.repr().equals(other.repr())));
  }

  public int hashCode() {
    return repr().hashCode();
  }

  public int compareTo(Object o) {
    int nameCmp = name().compareTo(((VarInfoName) o).name());
    if (nameCmp != 0) return nameCmp;
    int reprCmp = repr().compareTo(((VarInfoName) o).repr());
    return reprCmp;
  }

  public String toString() {
    // Too much code uses the implicit toString when it really wants
    // name().  Eventually change this to repr() and hunt down diffs
    // in the regression tests and fix the other code.
    return name();
    // return repr();
  }


  // ============================================================
  // IOA

  /**
   * Format this in IOA format, and remove all "this." and
   * "classname".
   * @param classname the String to remove
   *
   **/
  public String ioaFormatVar(String varname) {
    /*    int this_index = varname.indexOf("this.");
    int class_index = varname.indexOf(classname+".");
    String ioa_name = varname;

    while ((this_index>=0) || (class_index>=0)) {
      if (this_index>=0) {
	ioa_name = varname.substring(this_index+5, varname.length());
	if (this_index>0)
	  ioa_name = varname.substring(0, this_index) + ioa_name;
      } else if (class_index>=0) {
	ioa_name = varname.substring(class_index+classname.length(), varname.length());
	if (class_index>0)
	  ioa_name = varname.substring(0, class_index) + ioa_name;
      }
      this_index = ioa_name.indexOf("this.");
      class_index = ioa_name.indexOf(classname+".");
      }
      return ioa_name;*/
    return varname;
  }


  // ============================================================
  // Static inner classes which form the expression langugage

  /**
   * A simple identifier like "a", "this.foo", etc.
   **/
  public static class Simple extends VarInfoName {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20020130L;

    public final String name;
    public Simple(String name) {
      Assert.assert(name != null);
      this.name = name;
    }
    public boolean isLiteralConstant() {
      try {
	Integer.parseInt(name);
	return true;
      } catch (NumberFormatException e) {
	return false;
      }
    }
    protected String repr_impl() {
      return name;
    }
    protected String name_impl() {
      return name;
    }
    protected String esc_name_impl() {
      return "return".equals(name) ? "\\result" : name;
    }

    protected String ioa_name_impl() {
      return ioaFormatVar(name);
    }

    protected String simplify_name_impl(boolean prestate) {
      if (isLiteralConstant()) {
	return name;
      } else {
	return simplify_name_impl(name, prestate);
      }
    }
    protected static String simplify_name_impl(String s, boolean prestate) {
      if (s.startsWith("~") && s.endsWith("~")) {
	s = s.substring(1, s.length()-2) + ":closure";
      }
      if (prestate) {
	s = "__orig__" + s;
      }
      return "|" + s + "|";
    }
    protected String java_name_impl() {
      return "return".equals(name) ? "daikon_return" : name;
    }
    protected Class resolveType(PptTopLevel ppt) {
      // System.out.println("" + repr() + " resolveType(" + ppt.name + ")");
      // Also see Ast.getClass(String s)
      if (name.equals("this")) {
        try {
          String classname = ppt.ppt_name.getFullClassName();
          // System.out.println("classname = " + classname);
          Class result = Class.forName(classname);
          // System.out.println("resolveType => " + result);
          return result;
        } catch (Exception e) {
        }
      }
      return null;
    }
    protected java.lang.reflect.Field resolveField(PptTopLevel ppt) {
      // System.out.println("" + repr() + " resolveField(" + ppt.name + ")");
      try {
        if (name.startsWith("this.")) {
          Class c = Class.forName(ppt.ppt_name.getFullClassName());
          if (c != null) {
            java.lang.reflect.Field f = c.getDeclaredField(name.substring(5));
            return f;
          }
        }
      } catch (Exception e) {
      }
      return null;
    }

    public Object accept(Visitor v) {
      return v.visitSimple(this);
    }
  }

  /**
   * @return true iff applySize will not throw an exception
   * @see #applySize
   **/
  public boolean isApplySizeSafe() {
    return (new ElementsFinder(this)).elems() != null;
  }

  /**
   * Returns a name for the size of this (this object should be a
   * sequence).  Form is like "size(a[])" or "a.length".
   **/
  public VarInfoName applySize() {
    // The simple approach is wrong because this might be "orig(a[])"
    // return (new SizeOf((Elements) this)).intern();
    Elements elems = (new ElementsFinder(this)).elems();
    Assert.assert(elems != null,
                  "applySize should have elements to use in " + this + ";\n"
                  + "that is, " + this + " does not appear to be a sequence/collection.\n"
                  + "Perhaps its name should be suffixed by \"[]\"?");
    Replacer r = new Replacer(elems, (new SizeOf(elems)).intern());
    return r.replace(this).intern();
  }


  /**
   * The size of a contained sequence; form is like "size(sequence)"
   * or "sequence.length".
   **/
  public static class SizeOf extends VarInfoName {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20020130L;

    public final Elements sequence;
    public SizeOf(Elements sequence) {
      Assert.assert(sequence != null);
      this.sequence = sequence;
    }
    protected String repr_impl() {
      return "SizeOf[" + sequence.repr() + "]";
    }
    protected String name_impl() {
      return "size(" + sequence.name() + ")";
    }
    protected String esc_name_impl() {
      return sequence.term.esc_name() + ".length";
    }

    protected String simplify_name_impl(boolean prestate) {
      return "(arrayLength " + sequence.term.simplify_name(prestate) + ")";
    }

    protected String ioa_name_impl() {
      return "size(" + sequence.ioa_name() + ")";
    }
    protected String java_name_impl() {
      return sequence.term.java_name() + ".length";
    }

    protected Class resolveType(PptTopLevel ppt) {
      return Integer.TYPE;
    }
    protected java.lang.reflect.Field resolveField(PptTopLevel ppt) {
      return null;
    }

    public Object accept(Visitor v) {
      return v.visitSizeOf(this);
    }
  }

  /**
   * Returns a name for a function over this object; form is like
   * "sum(this)".
   **/
  public VarInfoName applyFunction(String function) {
    return (new FunctionOf(function, this)).intern();
  }

  /**
   * Returns a name for a function of two arguments;
   * form is like "sum(var1, var2))".
   * @param function the name of the function
   * @param vars The arguments to the function, of type VarInfoName
   *
   **/
  public static VarInfoName applyFunctionOfN(String function, List vars) {
    return (new FunctionOfN(function, vars)).intern();
  }

  /**
   * Returns a name for a function of two arguments;
   * form is like "sum(var1, var2))".
   * @param function the name of the function
   * @param vars The arguments to the function
   *
   **/
  public static VarInfoName applyFunctionOfN(String function, VarInfoName[] vars) {
    return applyFunctionOfN(function, Arrays.asList(vars));
  }

  /**
   * A function over a term, like "sum(argument)"
   **/
  public static class FunctionOf extends VarInfoName {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20020130L;

    public final String function;
    public final VarInfoName argument;
    public FunctionOf(String function, VarInfoName argument) {
      Assert.assert(function != null);
      Assert.assert(argument != null);
      this.function = function;
      this.argument = argument;
    }
    protected String repr_impl() {
      return "FunctionOf{" + function + "}[" + argument.repr() + "]";
    }
    protected String name_impl() {
      return function + "(" + argument.name() + ")";
    }
    protected String esc_name_impl() {
      return "(warning: format_esc() needs to be implemented: " +
	function + " on " + argument.repr() + ")";
    }
    protected String simplify_name_impl(boolean prestate) {
      return "(warning: format_simplify() needs to be implemented: " +
	function + " on " + argument.repr() + ")";
    }
    protected String ioa_name_impl() {
      return function + "(" + argument.ioa_name() + ")**";
    }
    protected String java_name_impl() {
      return "(warning: format_java() needs to be implemented: " +
	function + " on " + argument.repr() + ")";
    }
    protected Class resolveType(PptTopLevel ppt) {
      return null;
    }
    protected java.lang.reflect.Field resolveField(PptTopLevel ppt) {
      return null;
    }
    public Object accept(Visitor v) {
      return v.visitFunctionOf(this);
    }
  }


  /**
   * A function of two variables
   **/
  public static class FunctionOfN extends VarInfoName {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20020130L;

    public final String function;
    public final List args;

    /**
     * Construct a new function of two
     * @param function the name of the function
     * @param args the arguments to the function, of type VarInfoName
     **/

    public FunctionOfN(String function, List args) {
      Assert.assert(function != null);
      Assert.assert(args != null);
      this.args = args;
      this.function = function;
    }

    protected String repr_impl() {
      StringBuffer sb = new StringBuffer();
      for (Iterator i = args.iterator(); i.hasNext();) {
	sb.append (((VarInfoName) i.next()).repr());
	if (i.hasNext()) sb.append (", ");
      }
      return "FunctionOfN{" + function + "}[" + sb.toString() + "]";
    }
    protected String name_impl() {
      StringBuffer sb = new StringBuffer();
      for (Iterator i = args.iterator(); i.hasNext();) {
	sb.append (((VarInfoName) i.next()).name());
	if (i.hasNext()) sb.append (", ");
      }
      return function + "(" + sb.toString() + ")";
    }
    protected String esc_name_impl() {
      StringBuffer sb = new StringBuffer();
      for (Iterator i = args.iterator(); i.hasNext();) {
	sb.append (((VarInfoName) i.next()).repr());
	if (i.hasNext()) sb.append (", ");
      }
      return "(warning: format_esc() needs to be implemented: " +
	function + " on " + sb.toString() + ")";
    }
    protected String simplify_name_impl(boolean prestate) {
      StringBuffer sb = new StringBuffer();
      for (Iterator i = args.iterator(); i.hasNext();) {
	sb.append (((VarInfoName) i.next()).repr());
	if (i.hasNext()) sb.append (", ");
      }
      return "(warning: format_simplify() needs to be implemented: " +
	function + " on " + sb.toString() + ")";
    }
    protected String ioa_name_impl() {
      StringBuffer sb = new StringBuffer();
      for (Iterator i = args.iterator(); i.hasNext();) {
	sb.append (((VarInfoName) i.next()).ioa_name());
	if (i.hasNext()) sb.append (", ");
      }
      return function + "(" + sb.toString() + ")";
    }
    protected String java_name_impl() {
      StringBuffer sb = new StringBuffer();
      for (Iterator i = args.iterator(); i.hasNext();) {
	sb.append (((VarInfoName) i.next()).repr());
	if (i.hasNext()) sb.append (", ");
      }
      return "(warning: format_java() needs to be implemented: " +
	function + " on " + sb.toString() + ")";
    }

    /**
     * Shortcut getter to avoid repeated type casting
     **/
    public VarInfoName getArg (int n) {
      return (VarInfoName) args.get(n);
    }
    protected Class resolveType(PptTopLevel ppt) {
      return null;
    }
    protected java.lang.reflect.Field resolveField(PptTopLevel ppt) {
      return null;
    }
    public Object accept(Visitor v) {
      return v.visitFunctionOfN(this);
    }

  }

  /**
   * Returns a name for the intersection of with another sequence, like
   * "intersect(a[], b[])"
   **/
  public VarInfoName applyIntersection(VarInfoName seq2) {
    Assert.assert(seq2 != null);
    return (new Intersection(this, seq2)).intern();
  }

  /**
   * Returns a name for the intersection of with another sequence, like
   * "union(a[], b[])"
   **/
  public VarInfoName applyUnion(VarInfoName seq2) {
    Assert.assert(seq2 != null);
    return (new Union(this, seq2)).intern();
  }


  /**
   * Intersection of two sequences.  Extends FunctionOfTwo, and the
   * only change is that it does special formatting for IOA.
   **/
  public static class Intersection extends FunctionOfN {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20020130L;

    public Intersection(VarInfoName seq1, VarInfoName seq2) {
      super ("intersection", Arrays.asList(new VarInfoName[] {seq1, seq2}));
    }

    protected String ioa_name_impl() {
      return "(" + getArg(0).ioa_name() + " \\I " + getArg(1).ioa_name() + ")";
    }

  }

  /**
   * Union of two sequences.  Extends FunctionOfTwo, and the
   * only change is that it does special formatting for IOA.
   **/
  public static class Union extends FunctionOfN {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20020130L;

    public Union(VarInfoName seq1, VarInfoName seq2) {
      super ("intersection", Arrays.asList(new VarInfoName[] {seq1, seq2}));
    }

    protected String ioa_name_impl() {
      return "(" + getArg(0).ioa_name() + " \\U " + getArg(1).ioa_name() + ")";
    }

  }



  /**
   * Returns a 'getter' operation for some field of this name, like
   * a[i].foo if this is a[i].
   **/
  public VarInfoName applyField(String field) {
    return (new Field(this, field)).intern();
  }

  /**
   * A 'getter' operation for some field, like a[i].foo
   **/
  public static class Field extends VarInfoName {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20020130L;

    public final VarInfoName term;
    public final String field;
    public Field(VarInfoName term, String field) {
      Assert.assert(term != null);
      Assert.assert(field != null);
      this.term = term;
      this.field = field;
    }
    protected String repr_impl() {
      return "Field{" + field + "}[" + term.repr() + "]";
    }
    protected String name_impl() {
      return term.name() + "." + field;
    }
    protected String esc_name_impl() {
      return term.esc_name() + "." + field;
    }
    protected String simplify_name_impl(boolean prestate) {
      return "(select " + Simple.simplify_name_impl(field, prestate) + " " + term.simplify_name(prestate) + ")";
    }
    protected String ioa_name_impl() {
      return term.ioa_name() + "." + field;
    }
    protected String java_name_impl() {
      return term.java_name() + "." + field;
    }
    protected Class resolveType(PptTopLevel ppt) {
      // System.out.println("" + repr() + " resolveType(" + ppt.name + ")");
      java.lang.reflect.Field f = resolveField(ppt);
      if (f != null) {
        // System.out.println("resolveType => " + f.getType());
        return f.getType();
      }
      return null;
    }
    protected java.lang.reflect.Field resolveField(PptTopLevel ppt) {
      // System.out.println("" + repr() + " resolveField(" + ppt.name + ")");
      Class c = term.resolveType(ppt);
      if (c != null) {
        try {
          // System.out.println("resolveField found type " + c.getName());
          return c.getDeclaredField(field);
        } catch (Exception e) {
        }
      }
      return null;
    }
    public Object accept(Visitor v) {
      return v.visitField(this);
    }
  }

  /**
   * Returns a name for a the type of this object; form is like
   * "this.class" or "\typeof(this)".
   **/
  public VarInfoName applyTypeOf() {
    return (new TypeOf(this)).intern();
  }

  /**
   * The type of the term, like "term.class"
   **/
  public static class TypeOf extends VarInfoName {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20020130L;

    public final VarInfoName term;
    public TypeOf(VarInfoName term) {
      Assert.assert(term != null);
      this.term = term;
    }
    protected String repr_impl() {
      return "TypeOf[" + term.repr() + "]";
    }
    protected String name_impl() {
      return term.name() + ".class";
    }
    protected String esc_name_impl() {
      return "\\typeof(" + term.esc_name() + ")";
    }
    protected String simplify_name_impl(boolean prestate) {
      return "(typeof " + term.simplify_name(prestate) + ")";
    }
    protected String ioa_name_impl() {
      return "(typeof " + term.ioa_name() + ")**";
    }
    protected String java_name_impl() {
      return term.name() + ".class";
    }
    protected Class resolveType(PptTopLevel ppt) {
      return Class.class;
    }
    protected java.lang.reflect.Field resolveField(PptTopLevel ppt) {
      return null;
    }
    public Object accept(Visitor v) {
      return v.visitTypeOf(this);
    }
  }

  /**
   * Returns a name for a the prestate value of this object; form is
   * like "orig(this)" or "\old(this)".
   **/
  public VarInfoName applyPrestate() {
    if (this instanceof Poststate) {
      return ((Poststate)this).term;
    } else if ((this instanceof Add) && ((Add)this).term instanceof Poststate) {
      Add a = (Add)this;
      Poststate p = (Poststate)a.term;
      return p.term.applyAdd(a.amount);
    } else {
      return (new Prestate(this)).intern();
    }
  }

  /**
   * The prestate value of a term, like "orig(term)"
   **/
  public static class Prestate extends VarInfoName {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20020130L;

    public final VarInfoName term;
    public Prestate(VarInfoName term) {
      Assert.assert(term != null);
      this.term = term;
    }
    protected String repr_impl() {
      return "Prestate[" + term.repr() + "]";
    }
    protected String name_impl() {
      return "orig(" + term.name() + ")";
    }
    protected String esc_name_impl() {
      return "\\old(" + term.esc_name() + ")";
    }
    protected String simplify_name_impl(boolean prestate) {
      return term.simplify_name(true);
    }
    protected String ioa_name_impl() {
      return "preState(" + term.ioa_name() + ")";
    }
    protected String java_name_impl() {
      return "orig(" + term.name() + ")";
    }
    protected Class resolveType(PptTopLevel ppt) {
      return term.resolveType(ppt);
    }
    protected java.lang.reflect.Field resolveField(PptTopLevel ppt) {
      return term.resolveField(ppt);
    }
    public Object accept(Visitor v) {
      return v.visitPrestate(this);
    }
  }

  // sansOrig()
  //      int origpos = s.indexOf("orig(");
  //      Assert.assert(origpos != -1);
  //      int rparenpos = s.lastIndexOf(")");
  //      return s.substring(0, origpos)
  //        + s.substring(origpos+5, rparenpos)
  //        + s.substring(rparenpos+1);

  //      int origpos = s.indexOf("\\old(");
  //      Assert.assert(origpos != -1);
  //      int rparenpos = s.lastIndexOf(")");
  //      return s.substring(0, origpos)
  //        + s.substring(origpos+5, rparenpos)
  //        + s.substring(rparenpos+1);

  /**
   * Returns a name for a the poststate value of this object; form is
   * like "new(this)" or "\new(this)".
   **/
  public VarInfoName applyPoststate() {
    return (new Poststate(this)).intern();
  }

  /**
   * The poststate value of a term, like "new(term)".  Only used
   * within prestate, so like "orig(this.myArray[new(index)]".
   **/
  public static class Poststate extends VarInfoName {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20020130L;

    public final VarInfoName term;
    public Poststate(VarInfoName term) {
      Assert.assert(term != null);
      this.term = term;
    }
    protected String repr_impl() {
      return "Poststate[" + term.repr() + "]";
    }
    protected String name_impl() {
      return "post(" + term.name() + ")";
    }
    protected String esc_name_impl() {
      return "\\new(" + term.esc_name() + ")";
    }
    protected String simplify_name_impl(boolean prestate) {
      return term.simplify_name(false);
    }
    protected String ioa_name_impl() {
      return term.ioa_name() + "'";
    }
    protected String java_name_impl() {
      return "post(" + term.name() + ")";
    }
    protected Class resolveType(PptTopLevel ppt) {
      return term.resolveType(ppt);
    }
    protected java.lang.reflect.Field resolveField(PptTopLevel ppt) {
      return term.resolveField(ppt);
    }
    public Object accept(Visitor v) {
      return v.visitPoststate(this);
    }
  }

  /**
   * Returns a name for the this term plus a constant, like "this-1"
   * or "this+1".
   **/
  public VarInfoName applyAdd(int amount) {
    return (new Add(this, amount)).intern();
  }

  /**
   * An integer amount more or less than some other value
   **/
  public static class Add extends VarInfoName {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20020130L;

    public final VarInfoName term;
    public final int amount;
    public Add(VarInfoName term, int amount) {
      Assert.assert(term != null);
      this.term = term;
      this.amount = amount;
    }
    private String amount() {
      return (amount < 0) ? String.valueOf(amount) : "+" + amount;
    }
    protected String repr_impl() {
      return "Add{" + amount() + "}[" + term.repr() + "]";
    }
    protected String name_impl() {
      return term.name() + amount();
    }
    protected String esc_name_impl() {
      return term.esc_name() + amount();
    }
    protected String simplify_name_impl(boolean prestate) {
      return (amount < 0) ?
	"(- " + term.simplify_name(prestate) + " " + (-amount) + ")" :
	"(+ " + term.simplify_name(prestate) + " " + amount + ")";
    }
    protected String ioa_name_impl() {
      return term.ioa_name() + amount();
    }
    protected String java_name_impl() {
      return term.java_name() + amount();
    }
    protected Class resolveType(PptTopLevel ppt) {
      return term.resolveType(ppt);
    }
    protected java.lang.reflect.Field resolveField(PptTopLevel ppt) {
      return term.resolveField(ppt);
    }
    public Object accept(Visitor v) {
      return v.visitAdd(this);
    }
    // override for cleanliness
    public VarInfoName applyAdd(int _amount) {
      int amt = _amount + this.amount;
      return (amt == 0) ? term : term.applyAdd(amt);
    }
  }

  /**
   * Returns a name for the decrement of this term, like "this-1".
   **/
  public VarInfoName applyDecrement() {
    return applyAdd(-1);
  }

  /**
   * Returns a name for the increment of this term, like "this+1".
   **/
  public VarInfoName applyIncrement() {
    return applyAdd(+1);
  }

  /**
   * Returns a name for the elements of a container (as opposed to the
   * identity of the container) like "this[]" or "(elements this)".
   **/
  public VarInfoName applyElements() {
    return (new Elements(this)).intern();
  }

  /**
   * The elements of a container, like "term[]"
   **/
  public static class Elements extends VarInfoName {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20020130L;

    public final VarInfoName term;
    public Elements(VarInfoName term) {
      Assert.assert(term != null);
      this.term = term;
    }
    protected String repr_impl() {
      return "Elements[" + term.repr() + "]";
    }
    protected String name_impl() {
      return name_impl("");
    }
    protected String name_impl(String index) {
      return term.name() + "[" + index + "]";
    }
    protected String esc_name_impl() {
      throw new UnsupportedOperationException("ESC cannot format an unquantified sequence of elements" +
					      " [repr=" + repr() + "]");
    }
    protected String esc_name_impl(String index) {
      return term.esc_name() + "[" + index + "]";
    }
    protected String simplify_name_impl(boolean prestate) {
      return "(select elems " + term.simplify_name(prestate) + ")";
    }
    protected String ioa_name_impl() {
      return term.ioa_name();
    }
    protected String ioa_name_impl(String index) {
      return term.ioa_name() + "[" + index + "]";
    }
    protected String java_name_impl() {
      /* throw new UnsupportedOperationException("JAVA cannot format an unquantified sequence of elements" +
	 " [repr=" + repr() + "]");
      */
      // For now, do return the default implementation.
      return name_impl();
    }
    protected String java_name_impl(String index) {
      return term.name() + "[" + index + "]";
    }
    protected Class resolveType(PptTopLevel ppt) {
      Class c = term.resolveType(ppt);
      if (c != null) {
        return c.getComponentType();
      }
      return null;
    }
    protected java.lang.reflect.Field resolveField(PptTopLevel ppt) {
      return term.resolveField(ppt);
    }
    public Object accept(Visitor v) {
      return v.visitElements(this);
    }
  }

  /**
   * Caller is subscripting an orig(a[]) array.  Take the requested
   * index and make it useful in that context.
   **/
  static VarInfoName indexToPrestate(VarInfoName index) {
    // 1 orig(a[]) . orig(index) -> orig(a[index])
    // 2 orig(a[]) . index       -> orig(a[post(index)])
    if (index instanceof Prestate) {
      index = ((Prestate) index).term; // #1
    } else if (index instanceof Add) {
      Add add = (Add) index;
      if (add.term instanceof Prestate) {
	index = ((Prestate) add.term).term.applyAdd(add.amount); // #1
      } else {
	index = index.applyPoststate();  // #2
      }
    } else if (index.isLiteralConstant()) {
      // we don't want orig(a[post(0)]), so leave index alone
    } else {
      index = index.applyPoststate();  // #2
    }
    return index;
  }

  /**
   * Returns a name for an element selected from a sequence, like
   * "this[i]"
   **/
  public VarInfoName applySubscript(VarInfoName index) {
    Assert.assert(index != null);
    ElementsFinder finder = new ElementsFinder(this);
    Elements elems = finder.elems();
    Assert.assert(elems != null, "applySubscript should have elements to use in " + this);
    if (finder.inPre()) {
      index = indexToPrestate(index);
    }
    Replacer r = new Replacer(elems, (new Subscript(elems, index)).intern());
    return r.replace(this).intern();
  }

  // Given a sequence and subscript index, convert the index to an
  // explicit form if necessary (e.g. a[-1] becomes a[a.length-1])
  static VarInfoName indexExplicit(Elements sequence, VarInfoName index) {
    if (!index.isLiteralConstant()) {
      return index;
    }

    int i = Integer.parseInt(index.name());
    if (i >= 0) {
      return index;
    }

    return sequence.applySize().applyAdd(i);
  }

  /**
   * An element from a sequence, like "sequence[index]"
   **/
  public static class Subscript extends VarInfoName {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20020130L;

    public final Elements sequence;
    public final VarInfoName index;
    public Subscript(Elements sequence, VarInfoName index) {
      Assert.assert(sequence != null);
      Assert.assert(index != null);
      this.sequence = sequence;
      this.index = index;
    }
    protected String repr_impl() {
      return "Subscript{" + index.repr() + "}[" + sequence.repr() + "]";
    }
    protected String name_impl() {
      return sequence.name_impl(index.name());
    }
    protected String esc_name_impl() {
      return sequence.esc_name_impl(indexExplicit(sequence, index).esc_name());
    }
    protected String simplify_name_impl(boolean prestate) {
      return "(select " + sequence.simplify_name(prestate) + " " +
	indexExplicit(sequence, index).simplify_name(prestate) + ")";
    }
    protected String ioa_name_impl() {
      return sequence.ioa_name_impl(indexExplicit(sequence, index).ioa_name());
    }
    protected String java_name_impl() {
      return sequence.name_impl(index.name());
    }
    protected Class resolveType(PptTopLevel ppt) {
      Class c = sequence.resolveType(ppt);
      if (c != null) {
        return c.getComponentType();
      }
      return null;
    }
    protected java.lang.reflect.Field resolveField(PptTopLevel ppt) {
      return sequence.resolveField(ppt);
    }
    public Object accept(Visitor v) {
      return v.visitSubscript(this);
    }
  }


  /**
   * Returns a name for a slice of element selected from a sequence,
   * like "this[i..j]".  If an endpoint is null, it means "from the
   * start" or "to the end".
   **/
  public VarInfoName applySlice(VarInfoName i, VarInfoName j) {
    // a[] -> a[index..]
    // orig(a[]) -> orig(a[post(index)..])
    ElementsFinder finder = new ElementsFinder(this);
    Elements elems = finder.elems();
    Assert.assert(elems != null);
    if (finder.inPre()) {
      if (i != null) {
	i = indexToPrestate(i);
      }
      if (j != null) {
	j = indexToPrestate(j);
      }
    }
    Replacer r = new Replacer(finder.elems(), (new Slice(elems, i, j)).intern());
    return r.replace(this).intern();
  }

  /**
   * A slice of elements from a sequence, like "sequence[i..j]"
   **/
  public static class Slice extends VarInfoName {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20020130L;

    public final Elements sequence;
    public final VarInfoName i, j;
    public Slice(Elements sequence, VarInfoName i, VarInfoName j) {
      Assert.assert(sequence != null);
      Assert.assert((i != null) || (j != null));
      this.sequence = sequence;
      this.i = i;
      this.j = j;
    }
    protected String repr_impl() {
      return "Slice{" +
	((i == null) ? "" : i.repr()) + "," +
	((j == null) ? "" : j.repr()) + "}[" +
	sequence.repr() + "]";
    }
    protected String name_impl() {
      return sequence.name_impl("" +
				((i == null) ? "0" : i.name()) +
				".." +
				((j == null) ? ""  : j.name())
				);
    }
    protected String esc_name_impl() {
      throw new UnsupportedOperationException("ESC cannot format an unquantified slice of elements");
    }
    protected String simplify_name_impl(boolean prestate) {
      throw new UnsupportedOperationException("Simplify cannot format an unquantified slice of elements");
    }
    protected String ioa_name_impl() {
      // Need to be in form: \A e (i <= e <= j) => seq[e]"
      String result = "\\A e:Int (";
      result += ((i == null) ? "0" : i.ioa_name()) + " <= e <= ";
      result += ((j == null) ? "size("+sequence.ioa_name_impl()+")" :
		 j.ioa_name()) + ") => ";
      result += sequence.ioa_name_impl("e");
      return result;
    }
    protected String java_name_impl() {
      //throw new UnsupportedOperationException("JAVA cannot format an unquantified slice of elements");
      // For now, return the default implementation.
      return name_impl();
    }
    protected Class resolveType(PptTopLevel ppt) {
      Class c = sequence.resolveType(ppt);
      if (c != null) {
        return c.getComponentType();
      }
      return null;
    }
    protected java.lang.reflect.Field resolveField(PptTopLevel ppt) {
      return sequence.resolveField(ppt);
    }
    public Object accept(Visitor v) {
      return v.visitSlice(this);
    }
  }


  /**
   * Accept the actions of a visitor
   **/
  public abstract Object accept(Visitor v);

  /**
   * Visitor framework for processing of VarInfoNames
   **/
  public static interface Visitor {
    public Object visitSimple(Simple o);
    public Object visitSizeOf(SizeOf o);
    public Object visitFunctionOf(FunctionOf o);
    public Object visitFunctionOfN(FunctionOfN o);
    public Object visitField(Field o);
    public Object visitTypeOf(TypeOf o);
    public Object visitPrestate(Prestate o);
    public Object visitPoststate(Poststate o);
    public Object visitAdd(Add o);
    public Object visitElements(Elements o);
    public Object visitSubscript(Subscript o);
    public Object visitSlice(Slice o);
  }

  /**
   * Traverse the tree elements which have exactly one branch (so the
   * traversal order doesn't matter).  Visitors need to implement
   * methods for traversing elements (e.g. FunctionOfTwo) with more
   * than one branch.
   **/
  public static abstract class AbstractVisitor
    implements Visitor
  {
    public Object visitSimple(Simple o) {
      // nothing to do; leaf node
      return null;
    }
    public Object visitSizeOf(SizeOf o) {
      return o.sequence.accept(this);
    }
    public Object visitFunctionOf(FunctionOf o) {
      return o.argument.accept(this);
    }

    /**
     * By default, return effect on first argument, but traverse all, backwards
     **/
    public Object visitFunctionOfN(FunctionOfN o) {
      Object retval = null;
      for (ListIterator i = o.args.listIterator(o.args.size()); i.hasPrevious();) {
	VarInfoName vin = (VarInfoName)i.previous();
	retval = vin.accept(this);
      }
      return retval;
    }

    public Object visitField(Field o) {
      return o.term.accept(this);
    }
    public Object visitTypeOf(TypeOf o) {
      return o.term.accept(this);
    }
    public Object visitPrestate(Prestate o) {
      return o.term.accept(this);
    }
    public Object visitPoststate(Poststate o) {
      return o.term.accept(this);
    }
    public Object visitAdd(Add o) {
      return o.term.accept(this);
    }
    public Object visitElements(Elements o) {
      return o.term.accept(this);
    }
    // leave abstract; traversal order and return values matter
    public abstract Object visitSubscript(Subscript o);

    // leave abstract; traversal order and return values matter
    public abstract Object visitSlice(Slice o);
  }

  /**
   * Use to report whether a node is in a pre- or post-state context.
   * Throws an assertion error if a given goal isn't present.
   **/
  public static class NodeFinder
    extends AbstractVisitor
  {
    /**
     * Creates a new NodeFinder but also tests if goal is in root or its children.
     * Throws an assertion error if not.
     * @param root The root of the tree to search
     * @param goal The goal to find
     **/

    public NodeFinder(VarInfoName root, VarInfoName goal) {
      this.goal = goal;
      Object o = root.accept(this);
      Assert.assert(o != null);
    }
    // state and accessors
    private final VarInfoName goal;
    private boolean pre;
    public boolean inPre() {
      return pre;
    }
    // visitor methods which get the job done
    public Object visitSimple(Simple o) {
      return (o == goal) ? goal : null;
    }
    public Object visitSizeOf(SizeOf o) {
      return (o == goal) ? goal : super.visitSizeOf(o);
    }
    public Object visitFunctionOf(FunctionOf o) {
      return (o == goal) ? goal : super.visitFunctionOf(o);
    }
    public Object visitFunctionOfN(FunctionOfN o) {
      Object retval = null;
      for (Iterator i = o.args.iterator(); i.hasNext();) {
	VarInfoName vin = (VarInfoName)i.next();
	retval = vin.accept(this);
	if (retval != null) return retval;
      }
      return retval;
    }
    public Object visitField(Field o) {
      return (o == goal) ? goal : super.visitField(o);
    }
    public Object visitTypeOf(TypeOf o) {
      return (o == goal) ? goal : super.visitTypeOf(o);
    }
    public Object visitPrestate(Prestate o) {
      pre = true;
      return super.visitPrestate(o);
    }
    public Object visitPoststate(Poststate o) {
      pre = false;
      return super.visitPoststate(o);
    }
    public Object visitAdd(Add o) {
      return (o == goal) ? goal : super.visitAdd(o);
    }
    public Object visitElements(Elements o) {
      return (o == goal) ? goal : super.visitElements(o);
    }
    public Object visitSubscript(Subscript o) {
      if (o == goal) return goal;
      if (o.sequence.accept(this) != null) return goal;
      if (o.index.accept(this) != null) return goal;
      return null;
    }
    public Object visitSlice(Slice o) {
      if (o == goal) return goal;
      if (o.sequence.accept(this) != null) return goal;
      if ((o.i != null) & (o.i.accept(this) != null)) return goal;
      if ((o.j != null) & (o.j.accept(this) != null)) return goal;
      return null;
    }
  }

  /**
   * Use to traverse a tree, find the first (elements ...) node, and
   * report whether it's in pre or post-state.
   **/
  public static class ElementsFinder
    extends AbstractVisitor
  {
    public ElementsFinder (VarInfoName name) {
      elems = (Elements) name.accept(this);
    }

    // state and accessors
    private boolean pre = false;
    private Elements elems = null;

    public boolean inPre() {
      return pre;
    }
    public Elements elems() {
      return elems;
    }

    // visitor methods which get the job done
    public Object visitFunctionOfN(FunctionOfN o) {
      Object retval = null;
      for (Iterator i = o.args.iterator(); i.hasNext();) {
	VarInfoName vin = (VarInfoName)i.next();
	retval = vin.accept(this);
	if (retval != null) return retval;
      }
      return retval;
    }
    public Object visitPrestate(Prestate o) {
      pre = true;
      return super.visitPrestate(o);
    }
    public Object visitPoststate(Poststate o) {
      pre = false;
      return super.visitPoststate(o);
    }
    public Object visitElements(Elements o) {
      return o;
    }
    public Object visitSubscript(Subscript o) {
      // skip the subscripted sequence
      Object tmp = o.sequence.term.accept(this);
      if (tmp == null) { tmp = o.index.accept(this); }
      return tmp;
    }
    public Object visitSlice(Slice o) {
      // skip the sliced sequence
      Object tmp = o.sequence.term.accept(this);
      if (tmp == null && o.i != null) { tmp = o.i.accept(this); }
      if (tmp == null && o.j != null) { tmp = o.j.accept(this); }
      return tmp;
    }
  }

  /**
   * A Replacer is a Visitor that makes a copy of a tree, but
   * replaces some node (and its children) with another.
   **/
  public static class Replacer
    extends AbstractVisitor
  {
    private final VarInfoName old;
    private final VarInfoName _new;
    public Replacer(VarInfoName old, VarInfoName _new) {
      this.old = old;
      this._new = _new;
    }

    public VarInfoName replace(VarInfoName root) {
      return (VarInfoName) root.accept(this);
    }

    public Object visitSimple(Simple o) {
      return (o == old) ? _new : o;
    }
    public Object visitSizeOf(SizeOf o) {
      return (o == old) ? _new :
	((VarInfoName) super.visitSizeOf(o)).applySize();
    }
    public Object visitFunctionOf(FunctionOf o) {
      return (o == old) ? _new :
	((VarInfoName) super.visitFunctionOf(o)).applyFunction(o.function);
    }
    public Object visitFunctionOfN(FunctionOfN o) {
      // If o is getting replaced, then just replace it
      // otherwise, create a new function and check if arguments get replaced
      if (o == old) return _new;
      ArrayList newArgs = new ArrayList();
      for (Iterator i = o.args.iterator(); i.hasNext();) {
	VarInfoName vin = (VarInfoName)i.next();
	Object retval = vin.accept(this);
	newArgs.add (retval);
      }
      return VarInfoName.applyFunctionOfN(o.function, newArgs);
    }
    public Object visitField(Field o) {
      return (o == old) ? _new :
	((VarInfoName) super.visitField(o)).applyField(o.field);
    }
    public Object visitTypeOf(TypeOf o) {
      return (o == old) ? _new :
	((VarInfoName) super.visitTypeOf(o)).applyTypeOf();
    }
    public Object visitPrestate(Prestate o) {
      return (o == old) ? _new :
	((VarInfoName) super.visitPrestate(o)).applyPrestate();
    }
    public Object visitPoststate(Poststate o) {
      return (o == old) ? _new :
	((VarInfoName) super.visitPoststate(o)).applyPoststate();
    }
    public Object visitAdd(Add o) {
      return (o == old) ? _new :
	((VarInfoName) super.visitAdd(o)).applyAdd(o.amount);
    }
    public Object visitElements(Elements o) {
      return (o == old) ? _new :
	((VarInfoName) super.visitElements(o)).applyElements();
    }
    public Object visitSubscript(Subscript o) {
      return (o == old) ? _new :
	((VarInfoName) o.sequence.accept(this)).
	applySubscript((VarInfoName) o.index.accept(this));
    }
    public Object visitSlice(Slice o) {
      return (o == old) ? _new :
	((VarInfoName) o.sequence.accept(this)).
	applySlice((o.i == null) ? null : ((VarInfoName) o.i.accept(this)),
		   (o.j == null) ? null : ((VarInfoName) o.j.accept(this)));
    }
  }

  /**
   * Use to collect all elements in a tree into an inorder-traversal
   * list.  Result includes the root element.
   **/
  public static class InorderFlattener
    extends AbstractVisitor
  {
    public InorderFlattener(VarInfoName root) {
      root.accept(this);
    }

    // state and accessors
    private final List result = new ArrayList();

    public List nodes() {
      return Collections.unmodifiableList(result);
    }

    // visitor methods which get the job done
    public Object visitSimple(Simple o) {
      result.add(o);
      return super.visitSimple(o);
    }
    public Object visitSizeOf(SizeOf o) {
      result.add(o);
      return super.visitSizeOf(o);
    }
    public Object visitFunctionOf(FunctionOf o) {
      result.add(o);
      return super.visitFunctionOf(o);
    }
    public Object visitFunctionOfN(FunctionOfN o) {
      result.add (o);
      for (Iterator i = o.args.iterator(); i.hasNext();) {
	VarInfoName vin = (VarInfoName)i.next();
	Object retval = vin.accept(this);
      }
      return null;
    }
    public Object visitField(Field o) {
      result.add(o);
      return super.visitField(o);
    }
    public Object visitTypeOf(TypeOf o) {
      result.add(o);
      return super.visitTypeOf(o);
    }
    public Object visitPrestate(Prestate o) {
      result.add(o);
      return super.visitPrestate(o);
    }
    public Object visitPoststate(Poststate o) {
      result.add(o);
      return super.visitPoststate(o);
    }
    public Object visitAdd(Add o) {
      result.add(o);
      return super.visitAdd(o);
    }
    public Object visitElements(Elements o) {
      result.add(o);
      return super.visitElements(o);
    }
    public Object visitSubscript(Subscript o) {
      result.add(o);
      o.sequence.accept(this);
      o.index.accept(this);
      return null;
    }
    public Object visitSlice(Slice o) {
      result.add(o);
      o.sequence.accept(this);
      if (o.i != null) o.i.accept(this);
      if (o.j != null) o.j.accept(this);
      return null;
    }
  }

  // ============================================================
  // Quantification for formatting in ESC or Simplify

  /**
   * A quantifier visitor can be used to search a tree and return all
   * unquantified sequences (e.g. a[] or a[i..j], and also all Simple
   * nodes (variable names).  This is useful for restating the name in
   * terms of a quantification.
   **/
  public static class QuantifierVisitor
    extends AbstractVisitor
  {
    public QuantifierVisitor(VarInfoName root) {
      Assert.assert(root != null);
      simples = new HashSet();
      unquant = new HashSet();
      root.accept(this);
    }

    // state and accessors
    private Set simples; // [Simple]
    private Set unquant; // [Elements || Slice]

    /**
     * @return Collection of simple identifiers used in this
     * expression (so that they can be checked for conflict with the
     * quantifier variable name).
     **/
    public Set simples() {
      return Collections.unmodifiableSet(simples);
    }
    /**
     * @return Collection of the nodes under the root which need
     * quantification.  (The values are either of type Elements or
     * Slice).
     **/
    public Set unquants() {
      if (QuantHelper.debug.isDebugEnabled()) {
	QuantHelper.debug.debug("unquants: " + unquant);
      }
      return Collections.unmodifiableSet(unquant);
    }

    // visitor methods which get the job done
    public Object visitSimple(Simple o) {
      simples.add(o);
      return super.visitSimple(o);
    }
    public Object visitElements(Elements o) {
      unquant.add(o);
      return super.visitElements(o);
    }
    public Object visitFunctionOfN(FunctionOfN o) {
      //o.arg1.accept(this);
      return ((VarInfoName) o.args.get(0)).accept(this); // Return value doesn't matter
      // We only use one of them because we don't want double quantifiers
    }
    public Object visitSizeOf(SizeOf o) {
      // don't visit the sequence; we aren't using the elements of it,
      // just the length, so we don't want to include it in the results
      return o.sequence.term.accept(this);
    }
    public Object visitSubscript(Subscript o) {
      o.index.accept(this);
      // don't visit the sequence; it is fixed with an exact
      // subscript, so we don't want to include it in the results
      return o.sequence.term.accept(this);
    }
    public Object visitSlice(Slice o) {
      unquant.add(o);
      if (o.i != null) { o.i.accept(this); }
      if (o.j != null) { o.j.accept(this); }
      // don't visit the sequence; we will replace the slice with the
      // subscript, so we want to leave the elements alone
      return o.sequence.term.accept(this);
    }
  }


  /**
   * Helper for writing parts of quantification expressions.
   * Formatting methods in invariants call the formatting methods in
   * this class to get commonly-used parts, like how universal
   * quanitifiers look like in the different formatting schemes.
   **/

  public static class QuantHelper {

    /**
     * Debugging category
     **/

    public static final Category debug = Category.getInstance ("daikon.inv.Invariant.print.QuantHelper");

    /**
     * A FreeVar is very much like a Simple, except that it doesn't
     * care if it's in prestate or poststate for simplify formatting.
     **/
    public static class FreeVar
      extends Simple
    {
      // We are Serializable, so we specify a version to allow changes to
      // method signatures without breaking serialization.  If you add or
      // remove fields, you should change this number to the current date.
      static final long serialVersionUID = 20020130L;

      public FreeVar(String name) {
	super(name);
      }
      protected String repr_impl() {
	return "Free[" + super.repr_impl() + "]";
      }
      protected String simplify_name_impl(boolean prestate) {
	return super.simplify_name_impl(false);
      }
    }

    // <root, needy, index> -> <root', lower, upper>
    /**
     * Replaces a needy (unquantified term) with its subscripted
     * equivalent, using the given index variable.
     *
     * @param root the root of the expression to be modified
     * @param needy the term to be subscripted (must be of type Elements or Slice)
     * @param index the variable to place in the subscript
     *
     * @return a 3-element array consisting of the new root, the lower
     * bound for the index (inclusive), and the upper bound for the
     * index (inclusive), in that order.
     **/
    public static VarInfoName[] replace(VarInfoName root, VarInfoName needy, VarInfoName index) {
      Assert.assert(root != null);
      Assert.assert(needy != null);
      Assert.assert(index != null);
      Assert.assert((needy instanceof Elements) || (needy instanceof Slice));

      // Figure out what to replace needy with, and the appropriate
      // bounds to use
      VarInfoName replace_with;
      VarInfoName lower, upper;
      if (needy instanceof Elements) {
	Elements sequence = (Elements) needy;
	replace_with = sequence.applySubscript(index);
	lower = ZERO;
	upper = sequence.applySize().applyDecrement();
      } else if (needy instanceof Slice) {
	Slice slice = (Slice) needy;
	replace_with = slice.sequence.applySubscript(index);
	lower = (slice.i != null) ? slice.i : ZERO;
	upper = (slice.j != null) ? slice.j :
	  slice.sequence.applySize().applyDecrement();
      } else {
	// unreachable; placate javac
	throw new IllegalStateException();
      }
      Assert.assert(replace_with != null);

      // If needy was in prestate, adjust bounds appropriately
      if (root.inPrestateContext(needy)) {
	if (!lower.isLiteralConstant()) {
	  if (lower instanceof Poststate) {
	    lower = ((Poststate) lower).term;
	  } else {
	    lower = lower.applyPrestate();
	  }
	}
	if (!upper.isLiteralConstant()) {
	  if (upper instanceof Poststate) {
	    upper = ((Poststate) upper).term;
	  } else {
	    upper = upper.applyPrestate();
	  }
	}
      }

      // replace needy
      VarInfoName root_prime = (new Replacer(needy, replace_with)).replace(root).intern();

      Assert.assert(root_prime != null);
      Assert.assert(lower != null);
      Assert.assert(upper != null);

      return new VarInfoName[] { root_prime, lower, upper };
    }

    /**
     * Record type for return value of the quantify method below
     **/
    public static class QuantifyReturn {
      public VarInfoName[] root_primes;
      public Vector bound_vars; // of VarInfoName[3] = <variable, lower, upper>
    }

    // <root*> -> <root'*, <index, lower, upper>*>
    /**
     * Given a list of roots, changes all Elements or Slice terms to
     * Subscripts by inserting a new free variable; also return bounds
     * for the new variables.
     **/
    public static QuantifyReturn quantify(VarInfoName[] roots) {
      Assert.assert(roots != null);

      if (QuantHelper.debug.isDebugEnabled()) {
	QuantHelper.debug.debug("roots: " + Arrays.asList(roots));
      }

      // create empty result
      QuantifyReturn result = new QuantifyReturn();
      result.root_primes = new VarInfoName[roots.length];
      result.bound_vars = new Vector();

      // all of the simple identifiers used by these roots
      Set simples = new HashSet(); // [Simple]

      // build helper for each roots; collect identifiers
      QuantifierVisitor[] helper = new QuantifierVisitor[roots.length];
      for (int i=0; i < roots.length; i++) {
	if (QuantHelper.debug.isDebugEnabled()) {
	  QuantHelper.debug.debug ("Calling quanthelper on: " + new Integer(i) + " " + roots[i]);
	}

	helper[i] = new QuantifierVisitor(roots[i]);
	simples.addAll(helper[i].simples());
      }

      // choose names for the indicies which don't conflict, and then
      // replace the right stuff in the term
      char tmp = 'i';
      for (int i=0; i < roots.length; i++) {
	List uq = new ArrayList(helper[i].unquants());
	if (uq.size() == 0) {
	  // nothing needs quantification
	  result.root_primes[i] = roots[i];
	} else {
	  if (QuantHelper.debug.isDebugEnabled()) {
	    QuantHelper.debug.debug("root: " + roots[i]);
	    QuantHelper.debug.debug("uq_elts: " + uq.toString());
	  }

	  Assert.assert(uq.size() == 1, "We can only handle 1D arrays for now");
	  VarInfoName uq_elt = (VarInfoName) uq.get(0);

	  VarInfoName idx = (new FreeVar(String.valueOf(tmp++))).intern();
	  Assert.assert(!simples.contains(idx), "Index variable unexpectedly used");

	  if (QuantHelper.debug.isDebugEnabled()) {
	    QuantHelper.debug.debug("idx: " + idx);
	  }

	  // call replace and unpack results
	  VarInfoName[] replace_result = replace(roots[i], uq_elt, idx);
	  VarInfoName root_prime = replace_result[0];
	  VarInfoName lower = replace_result[1];
	  VarInfoName upper = replace_result[2];

	  result.root_primes[i] = root_prime;
	  result.bound_vars.add(new VarInfoName[] { idx, lower, upper });
	}
      }

      return result;
    }


    /**
     * It's too complex (and error prone). to hold quantification
     * results for IOA in a string array; so we create a helper object
     * that has accessors.  Otherwise this works just like a
     * format_ioa method here would work.
     *
     **/
    public static class IOAQuantification {
      private static final String quantifierExistential = "\\E ";
      private static final String quantifierUniversal = "\\A ";

      private VarInfo[] sets;
      private VarInfoName[] setNames;
      private String quantifierExp;
      private QuantifyReturn qret;
      private int numVars;

      public IOAQuantification (VarInfo v1) {
	this (new VarInfo[] { v1 });
      }

      public IOAQuantification (VarInfo v1, VarInfo v2) {
	this (new VarInfo[] { v1, v2 });
      }

      public IOAQuantification (VarInfo[] sets) {
	Assert.assert(sets != null);

	this.sets = sets;
	numVars = sets.length;

	setNames = new VarInfoName[sets.length];
	for (int i=0; i<sets.length; i++)
	  setNames[i] = sets[i].name;

	qret = quantify(setNames);


	// Build the quantifier
	StringBuffer quantifier = new StringBuffer();
	for (int i=0; i < qret.bound_vars.size(); i++) {
	  // Assert.assert(v_roots[i].isIOASet() || v_roots[i].isIOAArray());
	  VarInfoName var = ((VarInfoName[]) qret.bound_vars.get(i))[0];
	  quantifier.append (quantifierUniversal);
	  quantifier.append (var.ioa_name());
	  quantifier.append (" : ");
	  quantifier.append (sets[i].domainTypeIOA());
	  quantifier.append (" ");

	}
	quantifierExp = quantifier.toString() + "(";
      }

      public String getQuantifierExp() {
	// \A i : DomainType
	return quantifierExp;
      }

      public String getMembershipRestrictions() {
	StringBuffer sb = new StringBuffer();
	for (int i = 0; i < numVars; i++) {
	  if (i != 0) sb.append(" /\\ ");
	  sb.append (getMembershipRestriction(i));
	}
	return sb.toString();
      }

      public String getMembershipRestriction(int num) {
	return getVarName(num) + " \\in " + setNames[num].ioa_name();
      }

      public String getClosingExp() {
	// This isn't very smart right now, but maybe later we can
	// pretty print based on whether we need parens or not
	return ")";
      }

      public VarInfoName getVarName (int num) {
	return ((VarInfoName[]) (qret.bound_vars.get(num))) [0];
      }

      public VarInfoName getVarIndexed (int num) {
	return qret.root_primes[num];
      }

    }


    // <root*> -> <string*>
    /**
     * Helper method used by invariant that quantify and then want to
     * print in IOA format.  This method is given a list of array-type
     * variables, and then outputs a set of strings that mean "a
     * quantification over the array variables".  For example, given
     * an array Arr of type [D->R], this method would return:
     * {"\A i : D (", "i", "Arr[i]", ")"}
     *
     * @return The return array is 2 + 2 * size(v_roots) because it
     * consists of: a quantifier plus variables (1); pair of two
     * varinfos representing a quantified variable and the list
     * that it is in indexed by the variable(2 * size); a closer
     * (1).
     * @param sets A list of array-type variables over which we will quantify.
     *
     **/

      /**

    public static String[] format_ioa(VarInfo[] sets) {


      Assert.assert(sets != null);


      VarInfoName[] setnames = new VarInfoName[sets.length];
      for (int i=0; i<sets.length; i++)
	setnames[i] = sets[i].name;

      QuantifyReturn qret = quantify(setnames);

      String[] result = new String[2*sets.length+2];

      // Build the quantifier
      StringBuffer quantifier = new StringBuffer();
      for (int i=0; i < qret.bound_vars.size(); i++) {
	// Assert.assert(v_roots[i].isIOASet() || v_roots[i].isIOAArray());
	VarInfoName var = ((VarInfoName[]) qret.bound_vars.get(i))[0];

	quantifier.append ("\\A ");
	quantifier.append (var.ioa_name());
	quantifier.append (" : ");
	quantifier.append (sets[i].domainTypeIOA());
	quantifier.append (" ");

	// Build the variables
	result[i * 2 + 1] = var.ioa_name();
	result[i * 2 + 2] = qret.root_primes[i].ioa_name();
      }
      result[0] = result[0] + "(";
      result[sets.length * 2 + 1] = ")";

      quantifier.append ("(");
      result[0] = quantifier.toString();


      return result;

      QuantifyReturn qret = quantify(roots);
      String[] result = new String[2*roots.length+2];
      StringBuffer ptr_list = new StringBuffer();
      StringBuffer conditions = new StringBuffer();
      int numConditions = 0;

      for (int i=0; i < qret.bound_vars.size(); i++) {
	// Assert.assert(v_roots[i].isIOASet() || v_roots[i].isIOAArray());
	VarInfoName ptr = ((VarInfoName[]) qret.bound_vars.get(i))[0];
	if (i != 0)
	  ptr_list.append(", ");

	// if the corresponding var is of type 'Set':
	//    - 'ptr' is an IOA Set element, of type elementTypeIOA()
	//    - the following condition must be added: (ptr /in set)
	// otherwise, if type is IOA Array, ptr is an integer index
	if (v_roots[i].isIOASet()) {
	  ptr_list.append(ptr + ":" + v_roots[i].elementTypeIOA());
	  if (conditions.length()>0)
	    conditions.append(" /\\ ");
	  conditions.append("("+ptr.ioa_name() +" \\in ");
	  conditions.append(roots[i].ioa_name()+")");
	  numConditions++;
	} else
	  ptr_list.append(ptr.ioa_name() + ":Int");
      }

      result[0] = "\\A " + ptr_list + " (";
      result[roots.length+1] = ")";

      if (numConditions>0) {
	  result[0] += (numConditions==1) ? conditions+" => (" :
	      "(" + conditions + ") => (";
	result[roots.length+1] += ")";
      }

      // stringify the terms.  If root is 'Set' type, the pointer is sent instead
      for (int i=0; i < roots.length; i++) {
	VarInfoName ptr = ((VarInfoName[]) qret.bound_vars.get(i))[0];
	if (v_roots[i].isIOASet()) {
	  result[i+1] = ptr.ioa_name();
	  result[roots.length+i+2] = result[i+1];
	} else {
	  result[i+1] = qret.root_primes[i].ioa_name();
	  result[roots.length+i+2] = ptr.ioa_name();
	}
      }
      return result;
    }
      **/

    /**
     * Takes return values from QuantHelper.format_ioa and returns
     * variable names from it.
     *
     **/

    public static String forma_ioa_var (String[] quantExp, int varNum) {
      return quantExp[1 + varNum * 2];
    }

    /**
     * Takes return values from QuantHelper.format_ioa and returns
     * the variable subscripted with respect to the expression's set.
     *
     **/

    public static String forma_ioa_subscript (String[] quantExp, int varNum) {
      return quantExp[varNum * 2 + 2];
    }

    /**
     * Takes return values from QuantHelper.format_ioa and returns
     * the variable subscripted with respect to the expression's set.
     *
     **/

    public static String forma_ioa_in_exp (String[] quantExp, int varNum) {
      return quantExp[varNum * 2 + 1] + " \\in " + "Ops";
    }


    // <root*> -> <string string*>
    /**
     * Given a list of roots, return a String array where the first
     * element is a ESC-style quantification over newly-introduced
     * bound variables, the last element is a closer, and the other
     * elements are esc-named strings for the provided roots (with
     * sequenced subscripted by one of the new bound variables).
     **/
    public static String[] format_esc(VarInfoName[] roots) {
      return format_esc(roots, false);
    }
    public static String[] format_esc(VarInfoName[] roots, boolean elementwise) {
      Assert.assert(roots != null);

      QuantifyReturn qret = quantify(roots);

      // build the "\forall ..." predicate
      String[] result = new String[roots.length+2];
      StringBuffer int_list, conditions;
      {
	// "i, j, ..."
	int_list = new StringBuffer();
	// "ai <= i && i <= bi && aj <= j && j <= bj && ..."
	// if elementwise, also do "(i-ai) == (b-bi) && ..."
	conditions = new StringBuffer();
	for (int i=0; i < qret.bound_vars.size(); i++) {
	  VarInfoName[] boundv = (VarInfoName[]) qret.bound_vars.get(i);
	  VarInfoName idx = boundv[0], low = boundv[1], high = boundv[2];
	  if (i != 0) {
	    int_list.append(", ");
	    conditions.append(" && ");
	  }
	  int_list.append(idx.esc_name());
	  conditions.append(low.esc_name());
	  conditions.append(" <= ");
	  conditions.append(idx.esc_name());
	  conditions.append(" && ");
	  conditions.append(idx.esc_name());
	  conditions.append(" <= ");
	  conditions.append(high.esc_name());
	  if (elementwise && (i >= 1)) {
	    VarInfoName[] _boundv = (VarInfoName[]) qret.bound_vars.get(i-1);
	    VarInfoName _idx = _boundv[0], _low = _boundv[1];
	    conditions.append(" && ");
	    if (ZERO.equals(_low)) {
	      conditions.append(_idx);
	    } else {
	      conditions.append("(");
	      conditions.append(_idx.esc_name());
	      conditions.append("-(");
	      conditions.append(_low.esc_name());
	      conditions.append("))");
	    }
	    conditions.append(" == ");
	    if (ZERO.equals(low)) {
	      conditions.append(idx.esc_name());
	    } else {
	      conditions.append("(");
	      conditions.append(idx.esc_name());
	      conditions.append("-(");
	      conditions.append(low.esc_name());
	      conditions.append("))");
	    }
	  }
	}
      }
      result[0] = "(\\forall int " + int_list + "; (" + conditions + ") ==> ";
      result[result.length-1] = ")";

      // stringify the terms
      for (int i=0; i < roots.length; i++) {
	result[i+1] = qret.root_primes[i].esc_name();
      }

      return result;
    }

    // <root*> -> <string string*>
    /**
     * Given a list of roots, return a String array where the first
     * element is a simplify-style quantification over
     * newly-introduced bound variables, the last element is a closer,
     * and the other elements are simplify-named strings for the
     * provided roots (with sequenced subscripted by one of the new
     * bound variables).
     **/
    public static String[] format_simplify(VarInfoName[] roots) {
      return format_simplify(roots, false);
    }
    public static String[] format_simplify(VarInfoName[] roots, boolean elementwise) {
      Assert.assert(roots != null);

      QuantifyReturn qret = quantify(roots);

      // build the forall predicate
      String[] result = new String[roots.length+2];
      StringBuffer int_list, conditions;
      {
	// "i j ..."
	int_list = new StringBuffer();
	// "(AND (<= ai i) (<= i bi) (<= aj j) (<= j bj) ...)"
	// if elementwise, also insert "(EQ (- i ai) (- j aj)) ..."
	conditions = new StringBuffer();
	for (int i=0; i < qret.bound_vars.size(); i++) {
	  VarInfoName[] boundv = (VarInfoName[]) qret.bound_vars.get(i);
	  VarInfoName idx = boundv[0], low = boundv[1], high = boundv[2];
	  if (i != 0) {
	    int_list.append(" ");
	    conditions.append(" ");
	  }
	  int_list.append(idx.simplify_name());
	  conditions.append( "(<= " + low.simplify_name() + " " + idx.simplify_name() + ")");
	  conditions.append(" (<= " + idx.simplify_name() + " " + high.simplify_name() + ")");
	  if (elementwise && (i >= 1)) {
	    VarInfoName[] _boundv = (VarInfoName[]) qret.bound_vars.get(i-1);
	    VarInfoName _idx = _boundv[0], _low = _boundv[1];
	    conditions.append(" (EQ (- " + _idx.simplify_name() + " " + _low.simplify_name() + ")");
	    conditions.append(    " (- " + idx.simplify_name() + " " + low.simplify_name() + "))");
	  }
	}
      }
      result[0] = "(FORALL (" + int_list + ") (IMPLIES (AND " + conditions + ") ";
      result[result.length-1] = "))"; // close IMPLIES, FORALL

      // stringify the terms
      for (int i=0; i < roots.length; i++) {
	result[i+1] = qret.root_primes[i].simplify_name();
      }

      return result;
    }


    // <root*> -> <string string*>
    /**
     * Given a list of roots, return a String array where the first
     * element is a JAVA-style quantification over newly-introduced
     * bound variables, the last element is a closer, and the other
     * elements are java-named strings for the provided roots (with
     * sequences subscripted by one of the new bound variables).
     **/
    public static String[] format_java(VarInfoName[] roots) {
      return format_java(roots, false);
    }
    public static String[] format_java(VarInfoName[] roots, boolean elementwise) {
      Assert.assert(roots != null);
      QuantifyReturn qret = quantify(roots);

      // build the "\forall ..." predicate
      String[] result = new String[roots.length+2];
      StringBuffer int_list, conditions, closing;
      {
	// "i, j, ..."
	int_list = new StringBuffer();
	// "ai <= i && i <= bi && aj <= j && j <= bj && ..."
	// if elementwise, also do "(i-ai) == (b-bi) && ..."
	conditions = new StringBuffer();
	closing = new StringBuffer();
	for (int i=0; i < qret.bound_vars.size(); i++) {
	  VarInfoName[] boundv = (VarInfoName[]) qret.bound_vars.get(i);
	  VarInfoName idx = boundv[0], low = boundv[1], high = boundv[2];
	  if (i != 0) {
	    int_list.append(", ");
	    conditions.append(" && ");
	    closing.append(", ");
	    closing.append(idx.java_name());
	    closing.append(" ++");
	  } else {
	    closing.append(idx.java_name());
	    closing.append("++");
	  }
	  int_list.append(idx.java_name());
	  int_list.append(" == ");
	  int_list.append(low.java_name());

	  conditions.append(idx.java_name());
	  conditions.append(" <= ");
	  conditions.append(high.java_name());

	  if (elementwise && (i >= 1)) {
	    VarInfoName[] _boundv = (VarInfoName[]) qret.bound_vars.get(i-1);
	    VarInfoName _idx = _boundv[0], _low = _boundv[1];
	    conditions.append(" || ");
	    if (ZERO.equals(_low)) {
	      conditions.append(_idx);
	    } else {
	      conditions.append("(");
	      conditions.append(_idx.java_name());
	      conditions.append("-(");
	      conditions.append(_low.java_name());
	      conditions.append("))");
	    }
	    conditions.append(" == ");
	    if (ZERO.equals(low)) {
	      conditions.append(idx.java_name());
	    } else {
	      conditions.append("(");
	      conditions.append(idx.java_name());
	      conditions.append("-(");
	      conditions.append(low.java_name());
	      conditions.append("))");
	    }
	  }
	}
      }
      result[0] = "(for (int " + int_list + " ; (" + conditions + "; " + closing + ")";
      result[result.length-1] = ")";

      // stringify the terms
      for (int i=0; i < roots.length; i++) {
	result[i+1] = qret.root_primes[i].java_name();
      }

      return result;
    }
    
  } // QuantHelper


  // ============================================================
  // Transformation framework

  /**
   * Specifies a function that performs a transformation on VarInfoNames.
   **/
  public interface Transformer
  {
    /** Perform a transformation on the argument */
    public VarInfoName transform(VarInfoName v);
  }

  /**
   * A pass-through transformer.
   **/
  public static final Transformer IDENTITY_TRANSFORMER
    = new Transformer() {
	public VarInfoName transform(VarInfoName v) {
	  return v;
	}
      };

}

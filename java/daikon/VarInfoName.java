package daikon;

import daikon.inv.OutputFormat;
import daikon.derive.*;         // see dbc_name_impl(VarInfo v)
import daikon.derive.unary.*;   // see dbc_name_impl(VarInfo v)
import daikon.derive.binary.*;  // see dbc_name_impl(VarInfo v)
import daikon.derive.ternary.*; // see dbc_name_impl(VarInfo v)
import daikon.inv.OutputFormat.Repair;

import utilMDE.*;

import java.util.logging.Logger;
import java.util.logging.Level;

import java.lang.ref.WeakReference;
import java.io.Serializable;
import java.io.ObjectInputStream;
import java.io.IOException;
import java.util.*;

// If you change this file, also change class daikon.test.VarInfoNameTest.

/**
 * VarInfoName represents the "name" of a variable.
 * Calling it a "name", however, is somewhat misleading.  It can be
 * some expression that includes more than one variable, term, etc.
 * We separate this from the VarInfo itself because clients wish to
 * manipulate names into new expressions independent of the VarInfo
 * that they might be associated with.  VarInfoName's child classes
 * are specific types of names, like applying a function to something.
 * For example, "a" is a name, and "sin(a)" is a name that is the name
 * "a" with the function "sin" applied to it.
 **/
public abstract class VarInfoName
  implements Serializable, Comparable
{

  /** Debugging Logger. **/
  public static Logger debug = Logger.getLogger("daikon.VarInfoName");

  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020614L;

  /**
   * Given the standard String representation of a variable name (like
   * what appears in the normal output format), return the
   * corresponding VarInfoName. This method can't parse all the
   * strings that the VarInfoName name() method might produce, but it
   * should be able to handle anything that appears in a decls
   * file. Specifically, it can only handle a subset of the grammar of
   * derived variables. For some values of "name",
   * "name.equals(parse(e.name()))" might throw an exception, but if
   * it completes normally, the result should be true.
   **/
  public static VarInfoName parse(String name) {
    // x.class
    if (name.endsWith(".class")) {
      return parse(name.substring(0, name.length()-6)).applyTypeOf();
    }

    // a quoted string
    if (name.startsWith("\"") && name.endsWith("\"")) {
      String content = name.substring(1, name.length()-1);
      if (content.equals(UtilMDE.escapeNonJava
                         (UtilMDE.unescapeNonJava(content)))) {
        return (new Simple(name)).intern();
      }
    }

    // x or this.x
    if ((name.indexOf('[') == -1) && (name.indexOf('(') == -1)) {
      // checking for only legal characters would be more robust
      int dot = name.lastIndexOf('.');
      int arrow = name.lastIndexOf("->");
      if (dot >= 0 && dot > arrow) {
        String first = name.substring(0, dot);
        String field = name.substring(dot+1);
        return parse(first).applyField(field);
      } else if (arrow >= 0 && arrow > dot) {
        String first = name.substring(0, arrow);
        String field = name.substring(arrow+2);
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
      // This isn't quite right:  we really want the matching open bracket,
      // not the last open bracket.
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
      int arrow = name.lastIndexOf("->");
      if (dot >= brackets && dot > arrow) {
        String first = name.substring(0, dot);
        String field = name.substring(dot+1);
        return parse(first).applyField(field);
      } else if (arrow >= brackets && arrow > dot) {
        String first = name.substring(0, arrow);
        String field = name.substring(arrow+2);
        return parse(first).applyField(field);
      }
    }

    // orig(x)
    if (name.startsWith("orig(")) {
      throw new Error("orig() variables shouldn't appear in .decls files");
      // Assert.assertTrue(name.endsWith(")"));
      // return parse(name.substring(5, name.length() - 1)).applyPrestate();
    }

    // A.B, where A is complex: foo(x).y, x[7].y, etc.
    if (name.indexOf('.') != -1) {
      int dot = name.lastIndexOf('.');
      String first = name.substring(0, dot);
      String field = name.substring(dot+1);
      return parse(first).applyField(field);
    }

    // A->B, where A is complex: foo(x)->y, x[7]->y, etc.
    if (name.indexOf("->") != -1) {
      int arrow = name.lastIndexOf("->");
      String first = name.substring(0, arrow);
      String field = name.substring(arrow+2);
      return parse(first).applyField(field);
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
  private String name_cached = null; // interned
  protected abstract String name_impl();

  /**
   * Return the String representation of this name in the esc style
   * output format.
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
  private String esc_name_cached = null; // interned
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
  private String[] simplify_name_cached = new String[2]; // each interned
  protected abstract String simplify_name_impl(boolean prestate);


  /**
   * Return the string representation of this name in IOA format.
   * @return the string representation (interned) of this name, in the
   * IOA style output format
   **/
  public String ioa_name() {
    if (debug.isLoggable(Level.FINE)) {
      debug.fine ("ioa_name: " + this.toString());
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
  private String ioa_name_cached = null; // interned

  /**
   * Called in subclasses to return the internal implementation of
   * ioa_name.
   **/
  protected abstract String ioa_name_impl();

  /**
   * Return the String representation of this name in the java style
   * output format.
   *
   * @return the string representation (interned) of this name, in the
   * java style output format
   **/
  public String java_name(VarInfo v) {
    if (java_name_cached == null) {
      try {
        java_name_cached = java_name_impl(v).intern();
      } catch (RuntimeException e) {
        System.err.println("repr = " + repr());
        throw e;
      }
    }
    return java_name_cached;
  }
  private String java_name_cached = null; // interned
  protected abstract String java_name_impl(VarInfo v);

  /**
   * Return the String representation of this name in the repair style
   * output format.
   *
   * @return the string representation (interned) of this name, in the
   * java style output format
   **/
  public String repair_name(VarInfo v) {
      return repair_name_impl(v).intern();
  }
  private String repair_name_cached = null; // interned
  protected abstract String repair_name_impl(VarInfo v);

   /**
   * Return the String representation of this name in the JML style output format
   */
  public String jml_name(VarInfo v) {
    if (jml_name_cached == null) {
      try {
        jml_name_cached = jml_name_impl(v).intern();
      } catch (RuntimeException e) {
        System.err.println("repr = " + repr());
        throw e;
      }
    }
    // System.out.println("jml_name = " + jml_name_cached + " for " + name() + " of class " + this.getClass().getName());
    return jml_name_cached;
  }
  private String jml_name_cached = null; // interned
  protected abstract String jml_name_impl(VarInfo v);


  // For DBC, Java and JML formats, the formatting methods take as
  // argument the VarInfo related to the VarInfoName in question. This
  // causes trouble with VarInfoNameDriver (a class for testing
  // VarInfoName), because there are no VarInfo's to pass to the
  // formatting methods. So in places where the VarInfo argument is
  // required by the formatting code, we use this variable to check
  // whether we're doing testing. If we are, then we know that the
  // VarInfo being passed is probably null, and we do something other
  // than the normal thing (which would probably be to signal an
  // error). Unfortunately, this prevents testing of some those
  // formats that make use of VarInfo information.
  public static boolean testCall = false;

  /**
   * Return the String representation of this name in the dbc style
   * output format.
   *
   * @param var the VarInfo which goes along with this VarInfoName.
   *            Used to determine the type of the variable.
   *
   *        If var is null, the only repercussion is that if `this' is an
   *        "orig(x)" expression, it will be formatted in jml-style,
   *        something like "\old(x)".
   *
   *        If var is not null and `this' is an "orig(x)" expressions, it
   *        will be formatted in Jtest's DBC style, as "$pre(<type>, x)".
   *
   * @return the string representation (interned) of this name, in the
   * dbc style output format.
   **/
  public String dbc_name(VarInfo var) {
    if (dbc_name_cached == null) {
      try {
        dbc_name_cached = dbc_name_impl(var).intern();
      } catch (RuntimeException e) {
        System.err.println("repr = " + repr());
        throw e;
      }
    }
    return dbc_name_cached;
  }

  private String dbc_name_cached = null; // interned
  protected abstract String dbc_name_impl(VarInfo v);


  /**
   * Return the String representation of this name using only letters,
   * numbers, and underscores.
   */
  public String identifier_name() {
    if (identifier_name_cached == null) {
      try {
        identifier_name_cached = identifier_name_impl().intern();
      } catch (RuntimeException e) {
        System.err.println("repr = " + repr());
        throw e;
      }
    }
    // System.out.println("identifier_name = " + identifier_name_cached + " for " + name() + " of class " + this.getClass().getName());
    return identifier_name_cached;
  }
  private String identifier_name_cached = null; // interned
  protected abstract String identifier_name_impl();

  /**
   * @return name of this in the specified format
   **/
  public String name_using(OutputFormat format, VarInfo vi) {

    if (format == OutputFormat.DAIKON) return name();
    if (format == OutputFormat.REPAIR) return repair_name(vi);
    if (format == OutputFormat.SIMPLIFY) return simplify_name();
    if (format == OutputFormat.ESCJAVA) return esc_name();
    if (format == OutputFormat.JAVA) return java_name(vi);
    if (format == OutputFormat.JML) return jml_name(vi);
    if (format == OutputFormat.DBCJAVA) return dbc_name(vi);
    if (format == OutputFormat.IOA) return ioa_name();
    if (format == OutputFormat.IDENTIFIER) return identifier_name();
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
      VarInfoName result = (VarInfoName)ref.get();
      return result;
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
  public static final VarInfoName THIS = parse("this");
  public static final VarInfoName ORIG_THIS = parse("this").applyPrestate();

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

  /**
   * @return true if every variable in the name is an orig(...)
   * variable.
   **/
  public boolean isAllPrestate() {
    return new IsAllPrestateVisitor(this).result();
  }

  /**
   * @return true if this VarInfoName contains a simple variable whose
   * name is NAME.
   **/
  public boolean includesSimpleName(String name) {
    return new SimpleNamesVisitor(this).simples().contains(name);
  }

  // ============================================================
  // Special producers, or other helpers

  /**
   * Replace the first instances of node by replacement, in the data
   * structure rooted at this.
   **/
  public VarInfoName replace(VarInfoName node, VarInfoName replacement) {
    if (node == replacement)
      return this;
    Replacer r = new Replacer(node, replacement);
    return r.replace(this).intern();
  }

  /**
   * Replace all instances of node by replacement, in the data structure
   * rooted at this.
   **/
  public VarInfoName replaceAll(VarInfoName node, VarInfoName replacement) {
    if (node == replacement)
      return this;

    // Assert.assertTrue(! replacement.hasNode(node)); // no infinite loop

    // It doesn't make sense to assert this as we have plenty of times when
    // we want to replace x by y where y may contain x.

    VarInfoName result = this;
    Replacer r = new Replacer(node, replacement);

    // This code used to loop as long as node was in result, but this isn't
    // necessary -- all occurances are replaced by replacer.

    result = r.replace(result).intern();
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

  // This should be safe even in the absence of caching, because "repr()"
  // returns a new string each time, but it is equal() to any other
  // returned string, so their hashCode()s should be the same.
  public int hashCode() {
    return repr().hashCode();
  }

  public int compareTo(Object o) {
    int nameCmp = name().compareTo(((VarInfoName) o).name());
    if (nameCmp != 0) return nameCmp;
    int reprCmp = repr().compareTo(((VarInfoName) o).repr());
    return reprCmp;
  }

  // This is a debugging method, not intended for ordinary output.
  // Code producing output should usually call name() rather than
  // calling toString (perhaps implicitly).
  public String toString() {
    return repr();
  }


  // ============================================================
  // IOA

  /**
   * Format this in IOA format.
   **/
  public String ioaFormatVar(String varname) {
    /*
    int this_index = varname.indexOf("this.");
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
    return ioa_name;
    */
    return varname;
  }


  // Interning is lost when an object is serialized and deserialized.
  // Manually re-intern any interned fields upon deserialization.
  private void readObject(ObjectInputStream in)
    throws IOException, ClassNotFoundException
  {
    in.defaultReadObject();
    if (name_cached != null)
      name_cached = name_cached.intern();
    if (esc_name_cached != null)
      esc_name_cached = esc_name_cached.intern();
    if (simplify_name_cached[0] != null)
      simplify_name_cached[0] = simplify_name_cached[0].intern();
    if (simplify_name_cached[1] != null)
      simplify_name_cached[1] = simplify_name_cached[1].intern();
    if (ioa_name_cached != null)
      ioa_name_cached = ioa_name_cached.intern();
    if (java_name_cached != null)
      java_name_cached = java_name_cached.intern();
    if (repair_name_cached != null)
      repair_name_cached = repair_name_cached.intern();
    if (jml_name_cached != null)
      jml_name_cached = jml_name_cached.intern();
    if (dbc_name_cached != null)
     dbc_name_cached = dbc_name_cached.intern();
  }


  // ============================================================
  // Static inner classes that form the expression langugage

  /** A simple identifier like "a", "this.foo", etc. **/
  public static class Simple extends VarInfoName {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20020130L;

    public final String name;
    public Simple(String name) {
      Assert.assertTrue(name != null);
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
    // Names must be either a legal C/Java style identifier, or
    // surrounded by vertical bars (Simplify's quoting mechanism);
    // other than that, they only have to be consistent within one
    // execution of Daikon.
    protected static String simplify_name_impl(String s, boolean prestate) {
      if (s.startsWith("~") && s.endsWith("~")) {
        s = s.substring(1, s.length()-2) + ":closure";
      }
      if (prestate) {
        s = "__orig__" + s;
      }
      return "|" + s + "|";
    }
    protected String java_name_impl(VarInfo v) {
      return "return".equals(name) ? "\\result" : name;
    }

    protected String repair_name_impl(VarInfo v) {
      return repair_name(v,true);
    }

    public String repair_name(VarInfo v, boolean needrelation) {
	if ("return".equals(name))
	    return "$noprint(return)";
	if (v.name==this&&needrelation) {
	    Repair.getRepair().addSpecial();
	    return "s_quant."+Repair.getRepair().getRelation(name,v.ppt);
	} else
	    return Repair.getRepair().getSet(name,v.ppt);
    }
    protected String jml_name_impl(VarInfo v) {
      return "return".equals(name) ? "\\result" : name;
    }
    protected String dbc_name_impl(VarInfo v) {
      if (name.equals("return")) {
        return "$result";
      } else {
        return name;
      }
    }

    protected String identifier_name_impl() {
      if (name.equals("return"))
        return "Daikon_return";
      else if (name.equals("this"))
        return "Daikon_this";
      else {
        StringBuffer buf = new StringBuffer();
        for (int i = 0; i < name.length(); i++) {
          char c = name.charAt(i);
          if (Character.isLetterOrDigit(c))
            buf.append(c);
          else if (c == '_')
            buf.append("__");
          else if (c == '$')
            buf.append("_dollar_");
          else if (c == ':')
            buf.append("_colon_");
          else if (c == '*')
            buf.append("star_");
          else
            Assert.assertTrue(false,
                              "Unexpected character in VarInfoName$Simple");
        }
        return buf.toString();
      }
    }

    protected Class resolveType(PptTopLevel ppt) {
      // System.out.println("" + repr() + " resolveType(" + ppt.name() + ")");
      // Also see Ast.getClass(String s)
      if (name.equals("this")) {
        try {
          String classname = ppt.ppt_name.getFullClassName();
          // System.out.println("classname = " + classname);
          Class result = Class.forName(classname);
          // System.out.println("resolveType => " + result);
          return result;
        } catch (Throwable e) {
        }
      }
      return null;
    }
    protected java.lang.reflect.Field resolveField(PptTopLevel ppt) {
      // System.out.println("" + repr() + " resolveField(" + ppt.name() + ")");
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
    // The simple approach:
    //   return (new SizeOf((Elements) this)).intern();
    // is wrong because this might be "orig(a[])".
    Elements elems = (new ElementsFinder(this)).elems();
    Assert.assertTrue(elems != null,
     "applySize should have elements to use in " + name() + ";" + Global.lineSep
       + "that is, " + name() + " does not appear to be a sequence/collection." + Global.lineSep
       + "Perhaps its name should be suffixed by \"[]\"?" + Global.lineSep
       + " this.class = " + getClass().getName());
    Replacer r = new Replacer(elems, (new SizeOf(elems)).intern());
    return r.replace(this).intern();
  }

  /**
   * If this is a slice, (potentially in pre-state), return its lower
   * and upper bounds, which can be subtracted to get one less than
   * its size.
   */
  public VarInfoName[] getSliceBounds() {
    VarInfoName vin = this;
    boolean inPrestate = false;
    if (vin instanceof Prestate) {
      inPrestate = true;
      vin = ((Prestate)vin).term;
    }
    while (vin instanceof Field) {
      vin = ((Field)vin).term;
    }
    if (!(vin instanceof Slice))
      return null;
    Slice slice = (Slice)vin;
    VarInfoName[] ret = new VarInfoName[2];
    if (slice.i != null)
      ret[0] = slice.i;
    else
      ret[0] = ZERO;
    if (slice.j != null)
      ret[1] = slice.j;
    else
      ret[1] = slice.sequence.applySize().applyAdd(-1);
    if (inPrestate) {
      ret[0] = ret[0].applyPrestate();
      ret[1] = ret[1].applyPrestate();
    }
    return ret;
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
      Assert.assertTrue(sequence != null);
      this.sequence = sequence;
    }
    protected String repr_impl() {
      return "SizeOf[" + sequence.repr() + "]";
    }
    protected String name_impl() {
      return "size(" + sequence.name() + ")";
    }
    protected String repair_name_impl(VarInfo v) {
      Repair.getRepair().addSpecial();
      return "s_quant."+Repair.getRepair().getRelation("size("+sequence.name()+")",v.ppt);
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
    protected String java_name_impl(VarInfo v) {
      return java_family_impl(OutputFormat.JAVA, v);
    }
    protected String jml_name_impl(VarInfo v) {
      return java_family_impl(OutputFormat.JML, v);
    }
    protected String dbc_name_impl(VarInfo v) {
      return java_family_impl(OutputFormat.DBCJAVA, v);
    }
    protected String java_family_impl(OutputFormat format, VarInfo v) {

      // See declaration of testCall for explanation of this flag.
      if (testCall) { return "no format when testCall."; }

      Assert.assertTrue(v != null);
      Assert.assertTrue(v.isDerived());
      Derivation derived = v.derived;
      Assert.assertTrue(derived instanceof SequenceLength);
      VarInfo seqVarInfo = ((SequenceLength)derived).base;
      String prefix = sequence.term.name_using(format, seqVarInfo);
      return "daikon.Quant.size(" + prefix + ")";
//       if (seqVarInfo.type.pseudoDimensions() > seqVarInfo.type.dimensions()) {
//         if (prefix.startsWith("daikon.Quant.collect")) {
//           // Quant collect methods returns an array
//           return prefix  + ".length";
//         } else {
//           // It's a Collection
//           return prefix  + ".size()";
//         }
//       } else {
//         // It's an array
//         return prefix + ".length";
//       }
    }
    protected String identifier_name_impl() {
      return "size_of" + sequence.identifier_name() + "___";
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
   * Returns a name for a unary function applied to this object.
   * The result is like "sum(this)".
   **/
  public VarInfoName applyFunction(String function) {
    return (new FunctionOf(function, this)).intern();
  }

  /**
   * Returns a name for a function applied to more than one argument.
   * The result is like "sum(var1, var2)".
   * @param function the name of the function
   * @param vars The arguments to the function, of type VarInfoName
   **/
  public static VarInfoName applyFunctionOfN(String function, List vars) {
    return (new FunctionOfN(function, vars)).intern();
  }

  /**
   * Returns a name for a function of more than one argument.
   * The result is like "sum(var1, var2)".
   * @param function the name of the function
   * @param vars The arguments to the function
   **/
  public static VarInfoName applyFunctionOfN(String function, VarInfoName[] vars) {
    return applyFunctionOfN(function, Arrays.asList(vars));
  }

  /** A function over a term, like "sum(argument)". **/
  public static class FunctionOf extends VarInfoName {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20020130L;

    public final String function;
    public final VarInfoName argument;
    public FunctionOf(String function, VarInfoName argument) {
      Assert.assertTrue(function != null);
      Assert.assertTrue(argument != null);
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
    protected String java_name_impl(VarInfo v) {
      return java_family_name_impl(OutputFormat.JAVA, v);
    }
    protected String repair_name_impl(VarInfo v) {
      return function + "(" + argument.repair_name(v) + ")";
    }
    protected String jml_name_impl(VarInfo v) {
      return java_family_name_impl(OutputFormat.JML, v);
    }
    protected String dbc_name_impl(VarInfo v) {
      return java_family_name_impl(OutputFormat.DBCJAVA, v);
    }
    protected String java_family_name_impl(OutputFormat format, VarInfo v) {

      // See declaration of testCall for explanation of this flag.
      if (testCall) { return "no format when testCall."; }

      Assert.assertTrue(v != null);
      Assert.assertTrue(v.isDerived());
      Derivation derived = v.derived;
      Assert.assertTrue(derived instanceof UnaryDerivation);
      VarInfo argVarInfo = ((UnaryDerivation)derived).base;
      return "daikon.Quant." + function + "(" + argument.name_using(format, argVarInfo) + ")";
    }

    protected String identifier_name_impl() {
      return function + "_of_" + argument.identifier_name() + "___";
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


  /** A function of multiple parameters. **/
  public static class FunctionOfN extends VarInfoName {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20020130L;

    public final String function;
    public final List args;

    /**
     * Construct a new function of multiple arguments.
     * @param function the name of the function
     * @param args the arguments to the function, of type VarInfoName
     **/
    public FunctionOfN(String function, List args) {
      Assert.assertTrue(function != null);
      Assert.assertTrue(args != null);
      this.args = args;
      this.function = function;
    }

    protected String repr_impl() {
      StringBuffer sb = new StringBuffer();
      for (Iterator i = args.iterator(); i.hasNext(); ) {
        sb.append (((VarInfoName) i.next()).repr());
        if (i.hasNext()) sb.append (", ");
      }
      return "FunctionOfN{" + function + "}[" + sb.toString() + "]";
    }
    protected String name_impl() {
      StringBuffer sb = new StringBuffer();
      for (Iterator i = args.iterator(); i.hasNext(); ) {
        sb.append (((VarInfoName) i.next()).name());
        if (i.hasNext()) sb.append (", ");
      }
      return function + "(" + sb.toString() + ")";
    }

    protected String repair_name_impl(VarInfo vi) {
      StringBuffer sb = new StringBuffer();
      for (Iterator i = args.iterator(); i.hasNext(); ) {
        sb.append (((VarInfoName) i.next()).name());
        if (i.hasNext()) sb.append (", ");
      }
      return function + "(" + sb.toString() + ")";
    }
    protected String esc_name_impl() {
      StringBuffer sb = new StringBuffer();
      for (Iterator i = args.iterator(); i.hasNext(); ) {
        sb.append (((VarInfoName) i.next()).repr());
        if (i.hasNext()) sb.append (", ");
      }
      return "(warning: format_esc() needs to be implemented: " +
        function + " on " + sb.toString() + ")";
    }
    protected String simplify_name_impl(boolean prestate) {
      StringBuffer sb = new StringBuffer();
      for (Iterator i = args.iterator(); i.hasNext(); ) {
        sb.append (((VarInfoName) i.next()).repr());
        if (i.hasNext()) sb.append (", ");
      }
      return "(warning: format_simplify() needs to be implemented: " +
        function + " on " + sb.toString() + ")";
    }
    protected String ioa_name_impl() {
      StringBuffer sb = new StringBuffer();
      for (Iterator i = args.iterator(); i.hasNext(); ) {
        sb.append (((VarInfoName) i.next()).ioa_name());
        if (i.hasNext()) sb.append (", ");
      }
      return function + "(" + sb.toString() + ")";
    }

    protected String java_name_impl(VarInfo v) {
      return java_family_name_impl(OutputFormat.JAVA, v);
    }

    protected String jml_name_impl(VarInfo v) {
      return java_family_name_impl(OutputFormat.JML, v);
    }

    protected String dbc_name_impl(VarInfo v) {
      return java_family_name_impl(OutputFormat.DBCJAVA, v);
    }

    // Only works for two-argument functions.  There are currently no
    // ternary (or greater) function applications in Daikon.
    protected String java_family_name_impl(OutputFormat format, VarInfo v) {

      // See declaration of testCall for explanation of this flag.
      if (testCall) { return "no format when testCall."; }

      Assert.assertTrue(v != null);
      Assert.assertTrue(v.isDerived());
      Derivation derived = v.derived;
      Assert.assertTrue(derived instanceof BinaryDerivation);
      //|| derived instanceof TernaryDerivation);
      Assert.assertTrue(args.size() == 2);
      VarInfo arg1VarInfo = ((BinaryDerivation)derived).base1;
      VarInfo arg2VarInfo = ((BinaryDerivation)derived).base2;
      return "daikon.Quant." + function + "("
        + ((VarInfoName)args.get(0)).name_using(format, arg1VarInfo)  + ", "
        + ((VarInfoName)args.get(1)).name_using(format, arg2VarInfo)  + ")";
    }

    protected String identifier_name_impl() {
      StringBuffer sb = new StringBuffer(function);
      sb.append("_of_");
      for (Iterator i = args.iterator(); i.hasNext(); ) {
        sb.append (((VarInfoName) i.next()).identifier_name());
        if (i.hasNext()) sb.append ("_comma_");
      }
      sb.append("___");
      return sb.toString();
    }

    /**
     * Shortcut getter to avoid repeated type casting.
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
   * "intersect(a[], b[])".
   **/
  public VarInfoName applyIntersection(VarInfoName seq2) {
    Assert.assertTrue(seq2 != null);
    return (new Intersection(this, seq2)).intern();
  }

  /**
   * Intersection of two sequences.  Extends FunctionOfN, and the
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
   * Returns a name for the union of this with another sequence, like
   * "union(a[], b[])".
   **/
  public VarInfoName applyUnion(VarInfoName seq2) {
    Assert.assertTrue(seq2 != null);
    return (new Union(this, seq2)).intern();
  }

  /**
   * Union of two sequences.  Extends FunctionOfN, and the
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
   * a.foo if this is a.
   **/
  public VarInfoName applyField(String field) {
    return (new Field(this, field)).intern();
  }

  /** A 'getter' operation for some field, like a.foo. **/
  public static class Field extends VarInfoName {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20020130L;

    public final VarInfoName term;
    public final String field;
    public Field(VarInfoName term, String field) {
      Assert.assertTrue(term != null);
      Assert.assertTrue(field != null);
      this.term = term;
      this.field = field;
    }
    protected String repr_impl() {
      return "Field{" + field + "}[" + term.repr() + "]";
    }
    protected String name_impl() {
      return term.name() + "." + field;
    }

    /** This method returns a set that contains the given VarInfoName.
     */

    protected String getRealSet(VarInfo v, VarInfoName term) {
      if (term instanceof Simple) {
	Simple simple=(Simple)term;
	return Repair.getRepair().getRealSet(simple.name,v.ppt);
      } else if (term instanceof Field) {
	Field nfield=(Field)term;
	String r=Repair.getRepair().getRelation(v.ppt,getRealSet(v,nfield.term),nfield.field,nfield.name());
	return Repair.getRepair().getRange(v.ppt,r);
      } else return "$error";
    }

    protected String repair_name_impl(VarInfo v) {
      Repair.getRepair().noForceSet();
      String base=term.repair_name(v);
      String set=getRealSet(v,term);
      String relation=Repair.getRepair().getRelation(v.ppt,set,field,this.name());
      return base + "." + relation;
    }
    protected String esc_name_impl() {
      return term.esc_name() + "." + field;
    }
    protected String simplify_name_impl(boolean prestate) {
      return "(select " + Simple.simplify_name_impl(field, false) + " "
        + term.simplify_name(prestate) + ")";
    }
    protected String ioa_name_impl() {
      return term.ioa_name() + "." + field;
    }
    protected String java_name_impl(VarInfo v) {
      return java_family_name(OutputFormat.JAVA, v);
    }
    protected String jml_name_impl(VarInfo v) {
      return java_family_name(OutputFormat.JML, v);
    }
    protected String dbc_name_impl(VarInfo v) {
      return java_family_name(OutputFormat.DBCJAVA, v);
    }

    // Special case: the fake "toString" field in Daikon is output
    // with parens, so that it's legal Java.
    protected String java_field(String f) {
      if (f.equals("toString")) {
        return "toString()";
      } else {
        return f;
      }
    }

    // For JAVA, JML and DBC formats.
    protected String java_family_name(OutputFormat format, VarInfo v) {

      // See declaration of testCall for explanation of this flag.
      if (testCall) { return "no format when testCall."; }

      if (term.name().indexOf("..") != -1) {
        // We cannot translate arr[i..].x because this translates into
        //
        //    "daikon.Quant.collect(daikon.Quant.slice(arr,i,arr.length),"x")"
        //
        // but slice() returns an array of Objects, so an error will
        // occur when method collect() tries to query for field "x".
        return "(warning: " + format + " format cannot express a field applied to a slice:"
          + " [repr=" + repr() + "])";
      }

      boolean hasBrackets = (term.name().indexOf("[]") != -1);

      if (format == OutputFormat.JAVA) {
        Assert.assertTrue(! hasBrackets || v.type.dimensions() > 0,
                          "hasBrackets:" + hasBrackets
                          + ", dimensions:" + v.type.dimensions() + ", v:" + v);
      }

      if (!hasBrackets && format != OutputFormat.JAVA) {
        // Case 1: Not an array collection
        String term_name = null;
        if (format == OutputFormat.JML) {
          term_name = term.jml_name(v);
        } else if (format == OutputFormat.JAVA) {
          term_name = term.java_name(v);
        } else {
          term_name = term.dbc_name(v);
        }
        return term_name + "." + java_field(field);
      }


      // Case 2: An array collection

      // How to translate foo[].f, which is the array obtained from
      // collecting o.f for every element o in the array foo?  Just as
      // we did for slices, we'll translate this into a call of an
      // external function to do the job:

      // x.y.foo[].bar.f
      // ---translates-into--->
      // daikon.Quant.collect_TYPE_(x, "y.foo[].bar.f")

      // The method Quant.collect takes care of the "y.foo[].bar.f"
      // mess for object x.

      if (field.equals("toString")) {
        return "(warning: " + format + " format cannot express a slice with String objects:"
          + " obtained by toString():  [repr=" + repr() + "])";
      }

      String term_name_no_brackets = term.name().replaceAll("\\[\\]", "") + "." + field;

      String object = null;

      String packageName = v.aux.getValue(VarInfoAux.PACKAGE_NAME);
      if (packageName.equals(VarInfoAux.NO_PACKAGE_NAME)) {
        packageName = "";
      }


      String fields = null;

      String[] splits = null;
      boolean isStatic = false;
      String packageNamePrefix = null;
      //if (isStatic) {
      if (term_name_no_brackets.startsWith(packageName + ".")) {
//           throw new Error("packageName=" + packageName + ", term_name_no_brackets=" + term_name_no_brackets);
//         }
        // Before splitting, remove the package name.
        packageNamePrefix = (packageName.equals("") ? "" : packageName + ".");
        isStatic = true;
        splits = term_name_no_brackets.substring(packageNamePrefix.length()).split("\\.");
      } else {
        packageNamePrefix = "";
        isStatic = false;
        splits = term_name_no_brackets.split("\\.");
      }

      object = splits[0];
      if (isStatic) {
        object += ".class";
      }
      if (object.equals("return")) {
        if (format == OutputFormat.DBCJAVA) {
          object = "$return";
        } else {
          object = "\\result";
        }
      }

      fields = "";
      for (int j = 1 ; j < splits.length ; j++) {
        if (j != 1) { fields += "."; }
        fields += splits[j];
      }

      String collectType =  (v.type.baseIsPrimitive() ? v.type.base() : "Object");

      if (format == OutputFormat.JAVA) {
        return
          "daikon.Quant.collect"
          + collectType
          + (v.type.pseudoDimensions() == 0 ? "_field" : "")
          + "(" + packageNamePrefix + object + ", " + "\"" + fields + "\"" + ")";
      } else {
        return
          "daikon.Quant.collect" + collectType + "(" + packageNamePrefix + object + ", " + "\"" + fields + "\"" + ")";
      }
    }

    protected String identifier_name_impl() {
      return term.identifier_name() + "_dot_" + field;
    }
    protected Class resolveType(PptTopLevel ppt) {
      // System.out.println("" + repr() + " resolveType(" + ppt.name() + ")");
      java.lang.reflect.Field f = resolveField(ppt);
      if (f != null) {
        // System.out.println("resolveType => " + f.getType());
        return f.getType();
      }
      return null;
    }
    protected java.lang.reflect.Field resolveField(PptTopLevel ppt) {
      // System.out.println("" + repr() + " resolveField(" + ppt.name() + ")");
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
   * Returns a name for the type of this object; form is like
   * "this.class" or "\typeof(this)".
   **/
  public VarInfoName applyTypeOf() {
    return (new TypeOf(this)).intern();
  }

  /** The type of the term, like "term.class" or "\typeof(term)". **/
  public static class TypeOf extends VarInfoName {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20020130L;

    public final VarInfoName term;
    public TypeOf(VarInfoName term) {
      Assert.assertTrue(term != null);
      this.term = term;
    }
    protected String repr_impl() {
      return "TypeOf[" + term.repr() + "]";
    }
    protected String name_impl() {
      return term.name() + ".class";
    }
    protected String repair_name_impl(VarInfo vi) {
      return term.repair_name(vi);
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

    protected String javaFamilyFormat(String varname, boolean isArray) {
      if (isArray) {
        return "daikon.Quant.typeArray(" + varname + ")";
      } else {
        return varname + ".getClass().getName()";
      }
    }

    protected String java_name_impl(VarInfo v) {
      if (testCall) { return "no format when testCall."; }
      return javaFamilyFormat(term.java_name(v), v.type.isArray());
    }

    protected String jml_name_impl(VarInfo v) {
      if (testCall) { return "no format when testCall."; }
      return javaFamilyFormat(term.jml_name(v), v.type.isArray());
    }
    protected String dbc_name_impl(VarInfo v) {
      if (testCall) { return "no format when testCall."; }
      return javaFamilyFormat(term.dbc_name(v), v.type.isArray());
    }
    protected String identifier_name_impl() {
      return "type_of_" + term.identifier_name() + "___";
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

  /** The prestate value of a term, like "orig(term)" or "\old(term)". **/
  public static class Prestate extends VarInfoName {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20020130L;

    public final VarInfoName term;
    public Prestate(VarInfoName term) {
      Assert.assertTrue(term != null);
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
    protected String repair_name_impl(VarInfo v) {
      return "$noprint(old(" + term.repair_name(v) + "))";
    }
    protected String java_name_impl(VarInfo v) {
      return "\\old(" + term.java_name(v) + ")";
    }
    protected String jml_name_impl(VarInfo v) {
      return "\\old(" + term.jml_name(v) + ")";
    }
    protected String dbc_name_impl(VarInfo v) {

      // See declaration of testCall for explanation of this flag.
      if (testCall) { return "no format when testCall."; }

      String brackets = "";
      Assert.assertTrue(v != null);
      String preType = v.type.base();
      if ((term instanceof Slice)
          // Slices are obtained by calling daikon.Quant.slice(...)
          // which returns things of type java.lang.Object
          && (v.type.dimensions()) > 0
          && (v.type.base().equals("java.lang.Object"))) {
        preType = "java.lang.Object";
      }
      for (int i = 0 ; i < v.type.dimensions(); i++) { brackets += "[]"; }
      return "$pre(" + preType + brackets + ", " + term.dbc_name(v) + ")";
    }
    protected String identifier_name_impl() {
      return "orig_of_" + term.identifier_name() + "___";
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
  //      Assert.assertTrue(origpos != -1);
  //      int rparenpos = s.lastIndexOf(")");
  //      return s.substring(0, origpos)
  //        + s.substring(origpos+5, rparenpos)
  //        + s.substring(rparenpos+1);

  //      int origpos = s.indexOf("\\old(");
  //      Assert.assertTrue(origpos != -1);
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
      Assert.assertTrue(term != null);
      this.term = term;
    }
    protected String repr_impl() {
      return "Poststate[" + term.repr() + "]";
    }
    protected String name_impl() {
      return "post(" + term.name() + ")";
    }
    protected String repair_name_impl(VarInfo vi) {
      return "post(" + term.repair_name(vi) + ")";
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
    protected String java_name_impl(VarInfo v) {
      return "\\post(" + term.java_name(v) + ")";
    }
    protected String jml_name_impl(VarInfo v) {
      return "(warning: JML format cannot express a Poststate"
        + " [repr=" + repr() + "])";
    }
    protected String dbc_name_impl(VarInfo v) {
      return "(warning: DBC format cannot express a Poststate"
        + " [repr=" + repr() + "])";
    }
    protected String identifier_name_impl() {
      return "post_of_" + term.identifier_name() + "___";
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
    if (amount == 0) {
      return this;
    } else {
      return (new Add(this, amount)).intern();
    }
  }

  /** An integer amount more or less than some other value, like "x+2". **/
  public static class Add extends VarInfoName {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20020130L;

    public final VarInfoName term;
    public final int amount;
    public Add(VarInfoName term, int amount) {
      Assert.assertTrue(term != null);
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
    protected String repair_name_impl(VarInfo v) {
      return term.repair_name(v) + amount();
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
    protected String java_name_impl(VarInfo v) {
      return term.java_name(v) + amount();
    }
    protected String jml_name_impl(VarInfo v) {
      return term.jml_name(v) + amount();
    }
    protected String dbc_name_impl(VarInfo v) {
      return term.dbc_name(v) + amount();
    }
    protected String identifier_name_impl() {
      if (amount >= 0)
        return term.identifier_name() + "_plus" + amount;
      else
        return term.identifier_name() + "_minus" + (-amount);
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

  /** Returns a name for the decrement of this term, like "this-1". **/
  public VarInfoName applyDecrement() {
    return applyAdd(-1);
  }

  /** Returns a name for the increment of this term, like "this+1". **/
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

  /** The elements of a container, like "term[]". **/
  public static class Elements extends VarInfoName {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20020130L;

    public final VarInfoName term;
    public Elements(VarInfoName term) {
      Assert.assertTrue(term != null);
      this.term = term;
    }
    protected String repr_impl() {
      return "Elements[" + term.repr() + "]";
    }
    protected String name_impl() {
      return name_impl("");
    }
    protected String repair_name_impl(VarInfo vi) {
      // Figure out what to replace needy with, and the appropriate
      // bounds to use
      VarInfoName lower, upper;

      lower = getLowerBound();
      upper = getUpperBound();


      String set=Repair.getRepair().generateRangeSet(vi.ppt,lower,upper);
      String relation=Repair.getRepair().convertArraytoRelation(vi.ppt,this,vi);
      String index=Repair.getRepair().getQuantifierVar();
      Repair.getRepair().appendQuantifier(index,set);
      return index+"."+relation;
    }

    protected String repair_name_impl(String index,VarInfo vi) {
      return term.repair_name(vi)+"["+index+"]";
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
    protected String java_name_impl(VarInfo v) {
      return term.java_name(v);
    }
    protected String java_name_impl(String index, VarInfo v) {
      return java_family_impl(OutputFormat.JAVA, v, index);
    }
    protected String jml_name_impl(VarInfo v) {
      return term.jml_name(v);
    }
    protected String jml_name_impl(String index, VarInfo v) {
      return java_family_impl(OutputFormat.JML, v, index);
    }
    protected String dbc_name_impl(VarInfo v) {
      return term.dbc_name(v);
    }
    protected String dbc_name_impl(String index, VarInfo v) {
      return java_family_impl(OutputFormat.DBCJAVA, v, index);
    }

    // XXX temporary fix: sometimes long is passed as index (utilMDE.StopWatch).
    // I can't find where the VarInfo for "index" is found. Wherever that is,
    // we should check if its type is long, and do the casting only for that
    // case.
    protected String java_family_impl(OutputFormat format, VarInfo v, String index) {

      // If the collection goes through daikon.Quant.collect___, then
      // it will be returned as an array no matter what.
      String formatted = term.name_using(format, v);
      String collectType =  (v.type.baseIsPrimitive() ? v.type.base() : "Object");
      return "daikon.Quant.getElement_" + collectType + "(" + formatted + ", " + index + ")";
//       if (formatted.startsWith("daikon.Quant.collect")) {
//         return formatted + "[(int)" + index + "]";
//       } else {
//         if (v.type.pseudoDimensions() > v.type.dimensions()) {
//           // it's a collection
//           return formatted + ".get((int)" + index + ")";
//         } else {
//           // it's an array
//           return formatted + "[(int)" + index + "]";
//         }
//       }
    }

    protected String identifier_name_impl(String index) {
      if (index.equals(""))
        return term.identifier_name() + "_elems";
      else
        return term.identifier_name() + "_in_" + index + "_dex_";
    }
    protected String identifier_name_impl() {
      return identifier_name_impl("");
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
    public VarInfoName getLowerBound() {
      return ZERO;
    }
    public VarInfoName getUpperBound() {
      return applySize().applyDecrement();
    }
    public VarInfoName getSubscript(VarInfoName index) {
      return applySubscript(index);
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
   * "this[i]".
   **/
  public VarInfoName applySubscript(VarInfoName index) {
    Assert.assertTrue(index != null);
    ElementsFinder finder = new ElementsFinder(this);
    Elements elems = finder.elems();
    Assert.assertTrue(elems != null, "applySubscript should have elements to use in " + this);
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

  /** An element from a sequence, like "sequence[index]". **/
  public static class Subscript extends VarInfoName {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20020130L;

    public final Elements sequence;
    public final VarInfoName index;
    public Subscript(Elements sequence, VarInfoName index) {
      Assert.assertTrue(sequence != null);
      Assert.assertTrue(index != null);
      this.sequence = sequence;
      this.index = index;
    }
    protected String repr_impl() {
      return "Subscript{" + index.repr() + "}[" + sequence.repr() + "]";
    }
    protected String name_impl() {
      return sequence.name_impl(index.name());
    }
    protected String repair_name_impl(VarInfo v) {
      return sequence.repair_name_impl(index.repair_name(v),v);
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
    protected String java_name_impl(VarInfo v) {
      return java_family_impl(OutputFormat.JAVA, v);
    }
    protected String jml_name_impl(VarInfo v) {
      return java_family_impl(OutputFormat.JML, v);
    }
    protected String dbc_name_impl(VarInfo v) {
      return java_family_impl(OutputFormat.DBCJAVA, v);
    }
    protected String java_family_impl(OutputFormat format, VarInfo v) {

      // See declaration of testCall for explanation of this flag.
      if (testCall) { return "no format when testCall."; }

      Assert.assertTrue(v != null);
      Assert.assertTrue(v.isDerived());
      Derivation derived = v.derived;
      Assert.assertTrue(derived instanceof  SequenceScalarSubscript
                        || derived instanceof  SequenceStringSubscript
                        || derived instanceof  SequenceFloatSubscript);
      VarInfo indexVarInfo = ((BinaryDerivation)derived).base2;
      VarInfo seqVarInfo = ((BinaryDerivation)derived).base1;
      if (format == OutputFormat.JAVA) {
        return sequence.java_name_impl(index.java_name_impl(indexVarInfo), seqVarInfo);
      } else if (format == OutputFormat.JML) {
        return sequence.jml_name_impl(index.jml_name_impl(indexVarInfo), seqVarInfo);
      } else { // format == OutputFormat.DBCJAVA
        return sequence.dbc_name_impl(index.dbc_name_impl(indexVarInfo), seqVarInfo);
      }
    }
    protected String identifier_name_impl() {
      return sequence.identifier_name_impl(index.identifier_name());
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
    Assert.assertTrue(elems != null);
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

  /** A slice of elements from a sequence, like "sequence[i..j]". **/
  public static class Slice extends VarInfoName {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20020130L;

    public final Elements sequence;
    public final VarInfoName i, j;
    public Slice(Elements sequence, VarInfoName i, VarInfoName j) {
      Assert.assertTrue(sequence != null);
      Assert.assertTrue((i != null) || (j != null));
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

    protected String repair_name_impl(VarInfo v) {
      // Figure out what to replace needy with, and the appropriate
      // bounds to use
      VarInfoName lower, upper;

      lower = getLowerBound();
      upper = getUpperBound();

      String set=Repair.getRepair().generateRangeSet(v.ppt,lower,upper);
      String relation=Repair.getRepair().convertArraytoRelation(v.ppt,this,v);
      String index=Repair.getRepair().getQuantifierVar();
      Repair.getRepair().appendQuantifier(index,set);
      return index+"."+relation;
    }

    protected String esc_name_impl() {
      // return the default implementation for now.
      // return name_impl();
      throw new UnsupportedOperationException("ESC cannot format an unquantified slice of elements");
    }
    protected String simplify_name_impl(boolean prestate) {
      System.out.println(" seq: " + sequence + " " + i + " " + j);
      throw new UnsupportedOperationException("Simplify cannot format an unquantified slice of elements");
    }
    protected String ioa_name_impl() {
      // Need to be in form: \A e (i <= e <= j) => seq[e]"
      String result = "\\A e:Int (";
      result += ((i == null) ? "0" : i.ioa_name()) + " <= e <= ";
      result += ((j == null) ? "size(" + sequence.ioa_name_impl() + ")" :
                 j.ioa_name()) + ") => ";
      result += sequence.ioa_name_impl("e");
      return result;
    }
    protected String java_name_impl(VarInfo v) {
      return slice_helper(OutputFormat.JAVA, v);
    }
    protected String jml_name_impl(VarInfo v) {
      return slice_helper(OutputFormat.JML, v);
    }
    protected String dbc_name_impl(VarInfo v) {
      return slice_helper(OutputFormat.DBCJAVA, v);
    }

    // Helper for JML, Java and DBC formats
    protected String slice_helper(OutputFormat format, VarInfo v) {

      // See declaration of testCall for explanation of this flag.
      if (testCall) { return "no format when testCall."; }

      Assert.assertTrue(v != null);
      Assert.assertTrue(v.isDerived());
      Derivation derived = v.derived;
      Assert.assertTrue(derived instanceof SequenceSubsequence ||
                        derived instanceof SequenceScalarArbitrarySubsequence ||
                        derived instanceof SequenceFloatArbitrarySubsequence ||
                        derived instanceof SequenceStringArbitrarySubsequence);
      if (derived instanceof SequenceSubsequence) {
        Assert.assertTrue(i == null || j == null);
        if (i == null) { // sequence[0..j]
          Assert.assertTrue(j != null);
          return
            "daikon.Quant.slice("
            + sequence.name_using(format, ((SequenceSubsequence)derived).seqvar())
            + ", 0,  "
            + j.name_using(format, ((SequenceSubsequence)derived).sclvar())
            + ")";
        } else {
          VarInfo seqVarInfo = ((SequenceSubsequence)derived).seqvar();
          String lastIdxString = null;
          String prefix = sequence.name_using(format, seqVarInfo);
          lastIdxString = "daikon.Quant.size(" + prefix + ")";
//           if (seqVarInfo.type.pseudoDimensions() > seqVarInfo.type.dimensions()) {
//             if (prefix.startsWith("daikon.Quant.collect")) {
//               // Quant collect methods returns an array
//               lastIdxString = prefix + ".length-1";
//             } else {
//               // it's a collection
//               lastIdxString = prefix + ".size()-1";
//             }
//           } else {
//             // it's an array
//             lastIdxString = prefix + ".length-1";
//           }
          return
            "daikon.Quant.slice("
            + sequence.name_using(format, ((SequenceSubsequence)derived).seqvar())
            + ", " + i.name_using(format, ((SequenceSubsequence)derived).sclvar())
            + ", " + lastIdxString
            + ")";
        }
      } else {
        Assert.assertTrue(i != null && j != null);
        if (derived instanceof SequenceScalarArbitrarySubsequence) {
          SequenceScalarArbitrarySubsequence derived2 = (SequenceScalarArbitrarySubsequence)derived;
          return
            "daikon.Quant.slice("
            + sequence.name_using(format, derived2.seqvar())
            + ", " + i.name_using(format, derived2.startvar())
            + ", " + j.name_using(format, derived2.endvar())
            + ")";
        } else if (derived instanceof SequenceFloatArbitrarySubsequence) {
          SequenceFloatArbitrarySubsequence derived2 = (SequenceFloatArbitrarySubsequence)derived;
          return
            "daikon.Quant.slice("
            + sequence.name_using(format, derived2.seqvar())
            + ", " + i.name_using(format, derived2.startvar())
            + ", " + j.name_using(format, derived2.endvar())
            + ")";
        } else {
          SequenceStringArbitrarySubsequence derived2 = (SequenceStringArbitrarySubsequence)derived;
          return
            "daikon.Quant.slice("
            + sequence.name_using(format, derived2.seqvar())
            + ", " + i.name_using(format, derived2.startvar())
            + ", " + j.name_using(format, derived2.endvar())
            + ")";
        }
      }
    }


    protected String identifier_name_impl() {
      String start = (i == null) ? "0" : i.identifier_name();
      String end   = (j == null) ? ""  : j.identifier_name();
      return sequence.identifier_name_impl(start + "_to_" + end);
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
    public VarInfoName getLowerBound() {
      return (i != null) ? i : ZERO;
    }
    public VarInfoName getUpperBound() {
      return (j != null) ? j : sequence.getUpperBound();
    }
    public VarInfoName getSubscript(VarInfoName index) {
      return sequence.getSubscript(index);
    }
  }


  /** Accept the actions of a visitor. **/
  public abstract Object accept(Visitor v);

  /** Visitor framework for processing of VarInfoNames. **/
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
   * Traverse the tree elements that have exactly one branch (so the
   * traversal order doesn't matter).  Visitors need to implement
   * methods for traversing elements (e.g. FunctionOfN) with more
   * than one branch.
   **/
  public abstract static class AbstractVisitor
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
     * By default, return effect on first argument, but traverse all, backwards.
     **/
    public Object visitFunctionOfN(FunctionOfN o) {
      Object retval = null;
      for (ListIterator i = o.args.listIterator(o.args.size()); i.hasPrevious(); ) {
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
     * Creates a new NodeFinder.
     * @param root the root of the tree to search
     * @param goal the goal to find
     **/
    public NodeFinder(VarInfoName root, VarInfoName goal) {
      this.goal = goal;
      Assert.assertTrue(root.accept(this) != null);
    }
    // state and accessors
    private final VarInfoName goal;
    private boolean pre;
    public boolean inPre() {
      return pre;
    }
    // visitor methods that get the job done
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
      for (Iterator i = o.args.iterator(); i.hasNext(); ) {
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
      if ((o.i != null) && (o.i.accept(this) != null)) return goal;
      if ((o.j != null) && (o.j.accept(this) != null)) return goal;
      return null;
    }
  }

  /**
   * Finds if a given VarInfoName is contained in a set of nodes
   * in the VarInfoName tree using == comparison.  Recurse through
   * everything except fields, so in x.a, we don't look at a.
   **/
  public static class Finder
    extends AbstractVisitor
  {
    // state and accessors
    private final Set/*VarInfoName*/ goals;


    /**
     * Creates a new Finder.  Uses equals() to find.
     * @param argGoals The goals to find
     **/
    public Finder(Set argGoals) {
      goals = new HashSet();
      for (Iterator i = argGoals.iterator(); i.hasNext(); ) {
        this.goals.add (((VarInfoName) i.next()).intern());
      }
    }


    /**
     * Returns true iff some part of root is contained in this.goals.
     **/
    public boolean contains (VarInfoName root) {
      Object o = getPart(root);
      return (o != null);
    }


    /**
     * Returns the part of root that is contained in this.goals, or
     * null if not found.
     **/
    public Object getPart (VarInfoName root) {
      Object o = root.intern().accept(this);
      return o;
    }

    // visitor methods that get the job done
    public Object visitSimple(Simple o) {
      return (goals.contains(o)) ? o : null;
    }
    public Object visitSizeOf(SizeOf o) {
      return (goals.contains(o)) ? o : o.sequence.intern().accept(this);
    }
    public Object visitFunctionOf(FunctionOf o) {
      return (goals.contains(o)) ? o : super.visitFunctionOf(o);
    }
    public Object visitFunctionOfN(FunctionOfN o) {
      Object result = null;
      if (goals.contains(o)) return o;
      for (Iterator i = o.args.iterator(); i.hasNext(); ) {
        VarInfoName vin = (VarInfoName)i.next();
        result = vin.accept(this);
        if (result != null) return result;
      }
      return result;
    }
    public Object visitField(Field o) {
      return (goals.contains(o)) ? o : super.visitField(o);
    }
    public Object visitTypeOf(TypeOf o) {
      return (goals.contains(o)) ? o : super.visitTypeOf(o);
    }
    public Object visitPrestate(Prestate o) {
      if (goals.contains(o)) return o;
      return super.visitPrestate(o);
    }
    public Object visitPoststate(Poststate o) {
      if (goals.contains(o)) return o;
      return super.visitPoststate(o);
    }
    public Object visitAdd(Add o) {
      return (goals.contains(o)) ? o : super.visitAdd(o);
    }
    public Object visitElements(Elements o) {
      return (goals.contains(o)) ? o : super.visitElements(o);
    }
    public Object visitSubscript(Subscript o) {
      if (goals.contains(o)) return o;
      Object temp = o.sequence.accept(this);
      if (temp != null) return temp;
      temp = o.index.accept(this);
      if (temp != null) return temp;
      return null;
    }
    public Object visitSlice(Slice o) {
      if (goals.contains(o)) return o;
      Object temp = o.sequence.accept(this);
      if (temp != null) return temp;
      if (o.i != null) {
        temp = o.i.accept(this);
        if (temp != null) return temp;
      }
      if (o.j != null) {
        temp = o.j.accept(this);
        if (temp != null) return temp;
      }
      return null;
    }
  }

  // An abstract base class for visitors that compute some predicate
  // of a conjunctive nature (true only if true on all subparts),
  // returning null for false and an arbitrary non-null Object for
  // true.
  public abstract static class BooleanAndVisitor
    extends AbstractVisitor
  {
    private boolean result;

    public BooleanAndVisitor(VarInfoName name) {
      result = (name.accept(this) != null);
    }

    public boolean result() {
      return result;
    }

    public Object visitFunctionOfN(FunctionOfN o) {
      Object retval = null;
      for (ListIterator i = o.args.listIterator(o.args.size());
           i.hasPrevious(); ) {
        VarInfoName vin = (VarInfoName)i.previous();
        retval = vin.accept(this);
        if (retval != null)
          return null;
      }
      return retval;
    }

    public Object visitSubscript(Subscript o) {
      Object temp = o.sequence.accept(this);
      if (temp == null) return temp;
      temp = o.index.accept(this);
      return temp;
    }

    public Object visitSlice(Slice o) {
      Object temp = o.sequence.accept(this);
      if (temp == null) return temp;
      if (o.i != null) {
        temp = o.i.accept(this);
        if (temp == null) return temp;
      }
      if (o.j != null) {
        temp = o.j.accept(this);
        if (temp == null) return temp;
      }
      return temp;
    }
  }

  public static class IsAllPrestateVisitor
    extends BooleanAndVisitor
  {

    public IsAllPrestateVisitor(VarInfoName vin) { super(vin); }

    public Object visitSimple(Simple o) {
      // Any var not inside an orig() isn't prestate
      return null;
    }
    public Object visitPrestate(Prestate o) {
      // orig(...) is all prestate unless it contains post(...)
      return (new IsAllNonPoststateVisitor(o).result())
        ? Boolean.valueOf(true) : null;
    }
  }

  public static class IsAllNonPoststateVisitor
    extends BooleanAndVisitor
  {
    public IsAllNonPoststateVisitor(VarInfoName vin) { super(vin); }

    public Object visitSimple(Simple o) {
      // Any var not inside a post() isn't poststate
      return Boolean.valueOf(true);
    }
    public Object visitPoststate(Poststate o) {
      // If we see a post(...), we aren't all poststate.
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

    // visitor methods that get the job done
    public Object visitFunctionOfN(FunctionOfN o) {
      Object retval = null;
      for (Iterator i = o.args.iterator(); i.hasNext(); ) {
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
      for (Iterator i = o.args.iterator(); i.hasNext(); ) {
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
   * Replace pre states by normal variables, and normal variables by
   * post states.  We should do this for all variables except for
   * variables derived from return.  This piggybacks on replacer but
   * the actual replacement is done elsewhere.
   **/
  public static class PostPreConverter
    extends Replacer
  {

    public PostPreConverter() {
      super(null, null);
    }

    public Object visitSimple(Simple o) {
      if (o.name.equals("return")) return o;
      return o.applyPoststate();
    }

    public Object visitPrestate(Prestate o) {
      return o.term;
    }

  }


  /**
   * Use to collect all elements in a tree into an inorder-traversal
   * list.  Result includes the root element.
   * All methods return null; to obtain the result, call nodes().
   **/
  public static class InorderFlattener
    extends AbstractVisitor
  {
    public InorderFlattener(VarInfoName root) {
      root.accept(this);
    }

    // state and accessors
    private final List result = new ArrayList();

    /** Method returning the actual results (the nodes in order). **/
    public List nodes() {
      return Collections.unmodifiableList(result);
    }

    // visitor methods that get the job done
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
      for (Iterator i = o.args.iterator(); i.hasNext(); ) {
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

  public static class SimpleNamesVisitor
    extends AbstractVisitor
  {
    public SimpleNamesVisitor(VarInfoName root) {
      Assert.assertTrue(root != null);
      simples = new HashSet();
      root.accept(this);
    }

    /** @see #simples() **/
    private Set simples; // [String]

    /**
     * @return Collection of simple identifiers used in this
     * expression, as Strings. (Used, for instance, to check for
     * conflict with a quantifier variable name).
     **/
    public Set simples() {
      return Collections.unmodifiableSet(simples);
    }

    // visitor methods that get the job done
    public Object visitSimple(Simple o) {
      simples.add(o.name);
      return super.visitSimple(o);
    }
    public Object visitElements(Elements o) {
      return super.visitElements(o);
    }
    public Object visitFunctionOf(FunctionOf o) {
      simples.add(o.function);
      return super.visitFunctionOf(o);
    }
    public Object visitFunctionOfN(FunctionOfN o) {
      simples.add(o.function);
      return super.visitFunctionOfN(o);
    }
    public Object visitSubscript(Subscript o) {
      o.sequence.accept(this);
      return o.index.accept(this);
    }
    public Object visitSlice(Slice o) {
      if (o.i != null) { o.i.accept(this); }
      if (o.j != null) { o.j.accept(this); }
      return o.sequence.accept(this);
    }
  }


  /**
   * A quantifier visitor can be used to search a tree and return all
   * unquantified sequences (e.g. a[] or a[i..j]).
   **/
  public static class QuantifierVisitor
    extends AbstractVisitor
  {
    public QuantifierVisitor(VarInfoName root) {
      Assert.assertTrue(root != null);
      unquant = new HashSet();
      root.accept(this);
    }

    // state and accessors
    /** @see #unquants() **/
    private Set unquant; // [Elements || Slice]

    /**
     * @return Collection of the nodes under the root that need
     * quantification.  Each node represents an array; in particular,
     * the values are either of type Elements or Slice.
     **/
    // Here are some inputs and the corresponding output sets:
    //  terms[index].elts[num]   ==> { }
    //  terms[index].elts[]      ==> { terms[index].elts[] }
    //  terms[].elts[]           ==> { terms[], terms[].elts[] }
    //  ary[row][col]            ==> { }
    //  ary[row][]               ==> { ary[row][] }
    //  ary[][]                  ==> { ary[], ary[][] }
    public Set unquants() {
      if (QuantHelper.debug.isLoggable(Level.FINE)) {
        QuantHelper.debug.fine ("unquants: " + unquant);
      }
      return Collections.unmodifiableSet(unquant);
    }

    // visitor methods that get the job done
    public Object visitSimple(Simple o) {
      return super.visitSimple(o);
    }
    public Object visitElements(Elements o) {
      unquant.add(o);
      return super.visitElements(o);
    }

    public Object visitFunctionOf(FunctionOf o) {
      return null;
      // return ((VarInfoName) o.args.get(0)).accept(this); // Return value doesn't matter
      // We only use one of them because we don't want double quantifiers
    }
    /**
     * We do *not* want to pull out array members of FunctionOfN
     * because a FunctionOfN creates a black-box array with respect to
     * quantification.  (Also, otherwise, there may be two or more
     * arrays that are returned, making the quantification engine
     * think it's working with 2-d arrays.)
     **/
    public Object visitFunctionOfN(FunctionOfN o) {
      return null;
      // return ((VarInfoName) o.args.get(0)).accept(this); // Return value doesn't matter
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

  // ============================================================
  // Quantification for formatting in ESC or Simplify:  QuantHelper

  /**
   * Helper for writing parts of quantification expressions.
   * Formatting methods in invariants call the formatting methods in
   * this class to get commonly-used parts, like how universal
   * quanitifiers look in the different formatting schemes.
   **/
  public static class QuantHelper {

    /**
     * Debug tracer
     **/
    public static final Logger debug = Logger.getLogger("daikon.inv.Invariant.print.QuantHelper");

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
      protected String jml_name_impl(VarInfo v) {
        return super.jml_name_impl(v);
      }
      // protected String esc_name_impl() {
      //   return super.esc_name_impl();
      // }
      protected String simplify_name_impl(boolean prestate) {
        return super.simplify_name_impl(false);
      }
    }

    // <root, needy, index> -> <root', lower, upper>
    /**
     * Replaces a needy (unquantified term) with its subscripted
     * equivalent, using the given index variable.
     *
     * @param root the root of the expression to be modified.
     * Substitution occurs only in the subtree reachable from root.
     * @param needy the term to be subscripted (must be of type Elements or
     * Slice)
     * @param index the variable to place in the subscript
     *
     * @return a 3-element array consisting of the new root, the lower
     * bound for the index (inclusive), and the upper bound for the
     * index (inclusive), in that order.
     **/
    public static VarInfoName[] replace(VarInfoName root, VarInfoName needy, VarInfoName index) {
      Assert.assertTrue(root != null);
      Assert.assertTrue(needy != null);
      Assert.assertTrue(index != null);
      Assert.assertTrue((needy instanceof Elements) || (needy instanceof Slice));

      // Figure out what to replace needy with, and the appropriate
      // bounds to use
      VarInfoName replace_with;
      VarInfoName lower, upper;
      if (needy instanceof Elements) {
        Elements sequence = (Elements) needy;
        replace_with = sequence.getSubscript(index);
        lower = sequence.getLowerBound();
        upper = sequence.getUpperBound();
      } else if (needy instanceof Slice) {
        Slice slice = (Slice) needy;
        replace_with = slice.getSubscript(index);
        lower = slice.getLowerBound();
        upper = slice.getUpperBound();
      } else {
        // unreachable; placate javac
        throw new IllegalStateException();
      }
      Assert.assertTrue(replace_with != null);

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

      Assert.assertTrue(root_prime != null);
      Assert.assertTrue(lower != null);
      Assert.assertTrue(upper != null);

      return new VarInfoName[] { root_prime, lower, upper };
    }

    /**
     * Assuming that root is a sequence, return a VarInfoName
     * representing the (index_base+index_off)-th element of that
     * sequence. index_base may be null, to represent 0.
     **/
    public static VarInfoName selectNth(VarInfoName root,
                                        VarInfoName index_base,
                                        int index_off) {
      QuantifierVisitor qv = new QuantifierVisitor(root);
      List unquants = new ArrayList(qv.unquants());
      if (unquants.size() == 0) {
        // Nothing to do?
        return null;
      } else if (unquants.size() == 1) {
        VarInfoName index_vin;
        if (index_base != null) {
          index_vin = index_base;
          if (index_off != 0)
            index_vin = index_vin.applyAdd(index_off);
        } else {
          index_vin = new Simple(index_off + "");
        }
        VarInfoName to_replace = (VarInfoName)unquants.get(0);
        VarInfoName[] replace_result = replace(root, to_replace, index_vin);
        return replace_result[0];
      } else {
        Assert.assertTrue(false, "Can't handle multi-dim array in " +
                          "VarInfoName.QuantHelper.select_nth()");
        return null;
      }
    }

    // Return a string distinct from any of the strings in "taken"
    private static String freshDistinctFrom(Set taken) {
      char c = 'a';
      String name;
      do {
        name = String.valueOf(c++);
      } while (taken.contains(name));
      return name;
    }

    /**
     * Return a fresh variable name that doesn't appear in the given
     * variable name.
     **/
    public static VarInfoName getFreeIndex(VarInfoName vin) {
      Set simples = new SimpleNamesVisitor(vin).simples();
      return new FreeVar(freshDistinctFrom(simples));
    }

    /**
     * Return a fresh variable name that doesn't appear in the given
     * variable names.
     **/
    public static VarInfoName getFreeIndex(VarInfoName vin1,
                                           VarInfoName vin2) {
      Set simples = new HashSet(new SimpleNamesVisitor(vin1).simples());
      simples.addAll(new SimpleNamesVisitor(vin2).simples());
      return new FreeVar(freshDistinctFrom(simples));
    }

    /**
     * Return a fresh variable name that doesn't appear in the given
     * variable names.
     **/
    public static VarInfoName getFreeIndex(VarInfoName vin1,
                                           VarInfoName vin2,
                                           VarInfoName vin3) {
      Set simples = new HashSet(new SimpleNamesVisitor(vin1).simples());
      simples.addAll(new SimpleNamesVisitor(vin2).simples());
      simples.addAll(new SimpleNamesVisitor(vin3).simples());
      return new FreeVar(freshDistinctFrom(simples));
    }

    /**
     * Record type for return value of the quantify method below.
     **/
    public static class QuantifyReturn {
      public VarInfoName[] root_primes;
      public Vector bound_vars; // of VarInfoName[3] = <variable, lower, upper>
    }

    // <root*> -> <root'*, <index, lower, upper>*>
    // (The lengths of root* and root'* are the same; not sure about <i,l,u>*.)
    /**
     * Given a list of roots, changes all Elements and Slice terms to
     * Subscripts by inserting a new free variable; also return bounds
     * for the new variables.
     **/
    public static QuantifyReturn quantify(VarInfoName[] roots) {
      Assert.assertTrue(roots != null);

      if (QuantHelper.debug.isLoggable(Level.FINE)) {
        QuantHelper.debug.fine ("roots: " + Arrays.asList(roots));
      }

      // create empty result
      QuantifyReturn result = new QuantifyReturn();
      result.root_primes = new VarInfoName[roots.length];
      result.bound_vars = new Vector();

      // all of the simple identifiers used by these roots
      Set simples = new HashSet(); // [String]

      // build helper for each roots; collect identifiers
      QuantifierVisitor[] helper = new QuantifierVisitor[roots.length];
      for (int i=0; i < roots.length; i++) {
        if (QuantHelper.debug.isLoggable(Level.FINE)) {
          QuantHelper.debug.fine ("Calling quanthelper on: " + new Integer(i) + " " + roots[i]);
        }

        helper[i] = new QuantifierVisitor(roots[i]);
        simples.addAll(new SimpleNamesVisitor(roots[i]).simples());
      }

      // choose names for the indices that don't conflict, and then
      // replace the right stuff in the term
      char tmp = 'i';
      for (int i=0; i < roots.length; i++) {
        List uq = new ArrayList(helper[i].unquants());
        if (uq.size() == 0) {
          // nothing needs quantification
          result.root_primes[i] = roots[i];
        } else {
          if (QuantHelper.debug.isLoggable(Level.FINE)) {
            QuantHelper.debug.fine ("root: " + roots[i]);
            QuantHelper.debug.fine ("uq_elts: " + uq.toString());
          }

          // We assume that the input was one unquantified sequence
          // variable.  If uq has more than one element, then the
          // sequence had more than one dimension.
          Assert.assertTrue(uq.size() == 1, "We can only handle 1D arrays for now");

          VarInfoName uq_elt = (VarInfoName) uq.get(0);

          String idx_name;
          do {
            idx_name = String.valueOf(tmp++);
          } while (simples.contains(idx_name));
          Assert.assertTrue(tmp <= 'z',
                            "Ran out of letters in quantification");
          VarInfoName idx = (new FreeVar(idx_name)).intern();

          if (QuantHelper.debug.isLoggable(Level.FINE)) {
            QuantHelper.debug.fine ("idx: " + idx);
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
     * It's too complex (and error prone) to hold quantification
     * results for IOA in a string array; so we create a helper object
     * that has accessors.  Otherwise this works just like a
     * format_ioa method here would work.
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
        Assert.assertTrue(sets != null);

        this.sets = sets;
        numVars = sets.length;

        setNames = new VarInfoName[sets.length];
        for (int i=0; i<sets.length; i++)
          setNames[i] = sets[i].name;

        qret = quantify(setNames);


        // Build the quantifier
        StringBuffer quantifier = new StringBuffer();
        for (int i=0; i < qret.bound_vars.size(); i++) {
          // Assert.assertTrue(v_roots[i].isIOASet() || v_roots[i].isIOAArray());
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
        return getVarName(num).ioa_name() + " \\in " + setNames[num].ioa_name();
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

      public String getVarIndexedString (int num) {
        return getVarIndexed(num).ioa_name();
      }

    }


    /// NOTE: this method is commented out
    // <root*> -> <string*>
    /* *
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
     **/
    /*
    public static String[] format_ioa(VarInfo[] sets) {


      Assert.assertTrue(sets != null);


      VarInfoName[] setnames = new VarInfoName[sets.length];
      for (int i=0; i<sets.length; i++)
        setnames[i] = sets[i].name;

      QuantifyReturn qret = quantify(setnames);

      String[] result = new String[2*sets.length+2];

      // Build the quantifier
      StringBuffer quantifier = new StringBuffer();
      for (int i=0; i < qret.bound_vars.size(); i++) {
        // Assert.assertTrue(v_roots[i].isIOASet() || v_roots[i].isIOAArray());
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
        // Assert.assertTrue(v_roots[i].isIOASet() || v_roots[i].isIOAArray());
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
    */ // ... format_ioa

    /**
     * Takes return values from QuantHelper.format_ioa and returns
     * variable names from it.
     **/
    public static String forma_ioa_var (String[] quantExp, int varNum) {
      return quantExp[1 + varNum * 2];
    }

    /**
     * Takes return values from QuantHelper.format_ioa and returns
     * the variable subscripted with respect to the expression's set.
     **/
    public static String forma_ioa_subscript (String[] quantExp, int varNum) {
      return quantExp[varNum * 2 + 2];
    }

    /**
     * Takes return values from QuantHelper.format_ioa and returns
     * the variable subscripted with respect to the expression's set.
     **/
    public static String forma_ioa_in_exp (String[] quantExp, int varNum) {
      return quantExp[varNum * 2 + 1] + " \\in " + "Ops";
    }


    // <root*> -> <string string* string>
    /**
     * Given a list of roots, return a String array where the first
     * element is a ESC-style quantification over newly-introduced
     * bound variables, the last element is a closer, and the other
     * elements are esc-named strings for the provided roots (with
     * sequences subscripted by one of the new bound variables).
     **/
    public static String[] format_esc(VarInfoName[] roots) {
      return format_esc(roots, false);
    }
    public static String[] format_esc(VarInfoName[] roots, boolean elementwise) {
      // The call to format_esc is now handled by the combined formatter format_java_style
      return format_java_style(roots, elementwise, true, OutputFormat.ESCJAVA);
    }

    // <root*> -> <string string*>
    /**
     * Given a list of roots, return a String array where the first
     * element is a JML-style quantification over newly-introduced
     * bound variables, the last element is a closer, and the other
     * elements are jml-named strings for the provided roots (with
     * sequenced subscripted by one of the new bound variables).
     **/
//     public static String[] format_jml(VarInfoName[] roots) {
//       return format_jml(roots, false);
//     }
//     public static String[] format_jml(VarInfoName[] roots, boolean elementwise) {
//       return format_jml(roots, elementwise, true);
//     }
//     public static String[] format_jml(VarInfoName[] roots, boolean elementwise, boolean forall) {
//       return format_java_style(roots, elementwise, forall, OutputFormat.JML);
//     }

    /* CP: Quantification for DBC: We would like quantified expression
     * to always return a boolean value, and in the previous
     * implementation (commented out below), quantification was
     * expressed as a for-loop, which does not return boolean
     * values. An alternative solution would be to use Jtest's $forall
     * and $exists constrcuts, but testing showed that Jtest does not
     * allow these constructs in @post annotations (!). The current
     * implementation uses helper methods defined in a separate class
     * daikon.Quant (not currently included with Daikon's
     * distribution). These methods always return a boolean value and
     * look something like this:
     *
     *   Quant.eltsEqual(this.theArray, null)
     *   Quant.eltsOneOf(this.arr, new int[] { 1, 2, 3 })
     *
     */
    //     /**
    //      * vi is the Varinfo corresponding to the VarInfoName var. Uses
    //      * the variable i to iterate. This could mean a conflict with the
    //      * name of the argument var.
    //      */
    //     public static String dbcForalli(VarInfoName.Elements var, VarInfo vi,
    //                                     String condition) {
    //       if (vi.type.isArray()) {
    //         return
    //           "(java.util.Arrays.asList(" + var.term.dbc_name(vi)
    //           + ")).$forall(" + vi.type.base() + " i, "
    //           + condition + ")";
    //       }
    //       return var.term.dbc_name(vi)
    //         + ".$forall(" + vi.type.base() + " i, "
    //         + condition + ")";
    //     }

    //     /**
    //      * vi is the Varinfo corresponding to the VarInfoName var. Uses
    //       * the variable i to iterate. This could mean a conflict with the
    //       * name of the argument var.
    //      */
    //     public static String dbcExistsi(VarInfoName.Elements var, VarInfo vi,
    //                                     String condition) {
    //       if (vi.type.isArray()) {
    //         return
    //           "(java.util.Arrays.asList(" + var.term.dbc_name(vi)
    //           + ")).$exists(" + vi.type.base() + " i, "
    //           + condition + ")";
    //       }
    //       return var.term.dbc_name(vi)
    //         + ".$exists(" + vi.type.base() + " i, "
    //         + condition + ")";
    //     }

    //     //@tx
    //     public static String[] format_dbc(VarInfoName[] roots, VarInfo[] varinfos) {
    //       return format_dbc(roots, true, varinfos);
    //     }
    //     public static String[] format_dbc(VarInfoName[] roots, boolean elementwise, VarInfo[] varinfos) {
    //       return format_dbc(roots, elementwise, true, varinfos);
    //     }
    //     public static String[] format_dbc(VarInfoName[] roots, boolean elementwise, boolean forall, VarInfo[] varinfos) {
    //       Assert.assertTrue(roots != null);

    //       QuantifyReturn qret = quantify(roots);

    //       // build the "\forall ..." predicate
    //       String[] result = new String[roots.length + 2];
    //       StringBuffer int_list, conditions, closing;
    //       StringBuffer tempResult;
    //       {
    //         tempResult = new StringBuffer();
    //         // "i, j, ..."
    //         int_list = new StringBuffer();
    //         // "ai <= i && i <= bi && aj <= j && j <= bj && ..."
    //         // if elementwise, also do "(i-ai) == (b-bi) && ..."
    //         conditions = new StringBuffer();
    //         closing = new StringBuffer();
    //         for (int i = 0; i < qret.bound_vars.size(); i++) {
    //           int_list.setLength(0);
    //           conditions.setLength(0);
    //           closing.setLength(0);

    //           VarInfoName[] boundv = (VarInfoName[]) qret.bound_vars.get(i);
    //           VarInfoName idx = boundv[0], low = boundv[1], high = boundv[2];
    //           if (i != 0) {
    //             //int_list.append(", ");
    //             //conditions.append(" && ");
    //             //closing.append(", ");
    //             closing.append(idx.dbc_name(null));
    //             closing.append("++");
    //           } else {
    //             closing.append(idx.dbc_name(null));
    //             closing.append("++");
    //           }
    //           int_list.append(idx.dbc_name(null));
    //           int_list.append(" = "); //@TX
    //           int_list.append(low.dbc_name(null));

    //           conditions.append(idx.dbc_name(null));
    //           conditions.append(" <= ");
    //           conditions.append(high.dbc_name(null));

    //           if (elementwise && (i >= 1)) {
    //             VarInfoName[] _boundv = (VarInfoName[]) qret.bound_vars.get(i - 1);
    //             VarInfoName _idx = _boundv[0], _low = _boundv[1];
    //             conditions.append(" && ");
    //             if (ZERO.equals(_low)) {
    //               conditions.append(_idx);
    //             } else {
    //               conditions.append("(");
    //               conditions.append(_idx.dbc_name(null));
    //               conditions.append("-(");
    //               conditions.append(_low.dbc_name(null));
    //               conditions.append("))");
    //             }
    //             conditions.append(" == ");
    //             if (ZERO.equals(low)) {
    //               conditions.append(idx.dbc_name(null));
    //             } else {
    //               conditions.append("(");
    //               conditions.append(idx.dbc_name(null));
    //               conditions.append("-(");
    //               conditions.append(low.dbc_name(null));
    //               conditions.append("))");
    //             }
    //           }
    //           tempResult.append(" for (int " + int_list + " ; " + conditions + "; " + closing + ") ");
    //         }
    //       }
    //       //result[0] = "{ for (int " + int_list + " ; " + conditions + "; " + closing + ") $assert ("; //@TX
    //       result[0] = "{ " + tempResult + " $assert ("; //@TX
    //       result[result.length - 1] = "); }";

    //       // stringify the terms
    //       for (int i = 0; i < roots.length; i++) {
    //         result[i + 1] = qret.root_primes[i].dbc_name(null);
    //       }

    //       return result;
    //     }


    //////////////////////////

    public static String[] simplifyNameAndBounds(VarInfoName name) {
      String[] results = new String[3];
      boolean preState = false;
      if (name instanceof Prestate) {
        Prestate wrapped = (Prestate)name;
        name = wrapped.term;
        preState = true;
      }
      if (name instanceof Elements) {
        Elements sequence = (Elements)name;
        VarInfoName array = sequence.term;
        results[0] = array.simplify_name(preState);
        results[1] = sequence.getLowerBound().simplify_name(preState);
        results[2] = sequence.getUpperBound().simplify_name(preState);
        return results;
      } else if (name instanceof Slice) {
        Slice slice = (Slice)name;
        VarInfoName array = ((Elements)slice.sequence).term;
        results[0] = array.simplify_name(preState);
        results[1] = slice.getLowerBound().simplify_name(preState);
        results[2] = slice.getUpperBound().simplify_name(preState);
        return results;
      } else {
        // There are some other cases this scheme can't handle.
        // For instance, if every Book has an ISBN, a front-end
        // might distribute the access to that field over an array
        // of books, so that "books[].isbn" is an array of ISBNs,
        // though its name has type Field.
        return null;
      }
    }

    // <root*> -> <string string*>
    /**
     * Given a list of roots, return a String array where the first
     * element is a simplify-style quantification over
     * newly-introduced bound variables, the last element is a closer,
     * and the other elements are simplify-named strings for the
     * provided roots (with sequences subscripted by one of the new
     * bound variables).
     *
     * If elementwise is true, include the additional contraint that
     * the indices (there must be exactly two in this case) refer to
     * corresponding positions. If adjacent is true, include the
     * additional constraint that the second index be one more than
     * the first. If distinct is true, include the constraint that the
     * two indices are different. If includeIndex is true, return
     * additional strings, after the roots but before the closer, with
     * the names of the index variables.
     **/
    // XXX This argument list is starting to get out of hand. -smcc
    public static String[] format_simplify(VarInfoName[] roots) {
      return format_simplify(roots, false, false, false, false);
    }
    public static String[] format_simplify(VarInfoName[] roots,
                                           boolean eltwise) {
      return format_simplify(roots, eltwise, false, false, false);
    }
    public static String[] format_simplify(VarInfoName[] roots,
                                           boolean eltwise,
                                           boolean adjacent) {
      return format_simplify(roots, eltwise, adjacent, false, false);
    }
    public static String[] format_simplify(VarInfoName[] roots,
                                           boolean eltwise,
                                           boolean adjacent,
                                           boolean distinct) {
      return format_simplify(roots, eltwise, adjacent, distinct, false);
    }
    public static String[] format_simplify(VarInfoName[] roots,
                                           boolean elementwise,
                                           boolean adjacent,
                                           boolean distinct,
                                           boolean includeIndex) {
      Assert.assertTrue(roots != null);

      if (adjacent || distinct)
        Assert.assertTrue(roots.length == 2);

      QuantifyReturn qret = quantify(roots);

      // build the forall predicate
      String[] result = new String[(includeIndex ? 2 : 1) * roots.length + 2];
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
            if (_low.simplify_name().equals(low.simplify_name())) {
              conditions.append(" (EQ " + _idx.simplify_name() + " "
                                + idx.simplify_name() + ")");
            } else {
              conditions.append(" (EQ (- " + _idx.simplify_name() + " " + _low.simplify_name() + ")");
              conditions.append(    " (- " + idx.simplify_name() + " " + low.simplify_name() + "))");
            }
          }
          if (i == 1 && (adjacent || distinct)) {
            VarInfoName[] _boundv = (VarInfoName[]) qret.bound_vars.get(i-1);
            VarInfoName prev_idx = _boundv[0];
            if (adjacent)
              conditions.append(" (EQ (+ " + prev_idx.simplify_name() + " 1) "
                                + idx.simplify_name() + ")");
            if (distinct)
              conditions.append(" (NEQ " + prev_idx.simplify_name() + " "
                                + idx.simplify_name() + ")");
          }
        }
      }
      result[0] = "(FORALL (" + int_list + ") " +
        "(IMPLIES (AND " + conditions + ") ";

      // stringify the terms
      for (int i=0; i < qret.root_primes.length; i++) {
        result[i+1] = qret.root_primes[i].simplify_name();
      }

      // stringify the indices, if requested
      if (includeIndex) {
        for (int i=0; i < qret.root_primes.length; i++) {
          VarInfoName[] boundv = (VarInfoName[]) qret.bound_vars.get(i);
          VarInfoName idx_var = boundv[0];
          result[i + qret.root_primes.length + 1] = idx_var.simplify_name();
        }
      }

      result[result.length-1] = "))"; // close IMPLIES, FORALL

      return result;
    }

    // Important Note: The Java quantification style actually makes no
    // sense as is.  The resultant quantifications are statements as
    // opposed to expressions, and thus no value can be derived from
    // them. This must be fixed before the java statements are of any
    // value. However, the ESC and JML quantifications are fine because
    // they actually produce expressions with values.

    // <root*> -> <string string* string>
    /**
     * Given a list of roots, return a String array where the first
     * element is a Java-style quantification over newly-introduced
     * bound variables, the last element is a closer, and the other
     * elements are java-named strings for the provided roots (with
     * sequences subscripted by one of the new bound variables).
     **/
//     public static String[] format_java(VarInfoName[] roots) {
//       return format_java(roots, false);
//     }
//     public static String[] format_java(VarInfoName[] roots, boolean elementwise) {
//       return format_java_style(roots, false, true, OutputFormat.JAVA);
//     }

    // This set of functions quantifies in the same manner to the ESC quantification, except that
    // JML names are used instead of ESC names, and minor formatting changes are incorporated
//     public static String[] format_jml(QuantifyReturn qret) {
//       return format_java_style(qret, false, true, OutputFormat.JML);
//     }
//     public static String[] format_jml(QuantifyReturn qret, boolean elementwise) {
//       return format_java_style(qret, elementwise, true, OutputFormat.JML);
//     }
//     public static String[] format_jml(QuantifyReturn qret, boolean elementwise, boolean forall) {
//       return format_java_style(qret, elementwise, forall, OutputFormat.JML);
//     }

    // This set of functions assists in quantification for all of the java style
    // output formats, that is, Java, ESC, and JML. It does the actual work behind
    // those formatting functions. This function was meant to be called only through
    // the other public formatting functions.
    //
    // The OutputFormat must be one of those three previously mentioned.
    // Also, if the Java format is selected, forall must be true.

    protected static String[] format_java_style(VarInfoName[] roots, OutputFormat format) {
      return format_java_style(roots, false, true, format);
    }
    protected static String[] format_java_style(VarInfoName[] roots, boolean elementwise, OutputFormat format) {
      return format_java_style(roots, elementwise, true, format);
    }
    protected static String[] format_java_style(VarInfoName[] roots, boolean elementwise, boolean forall, OutputFormat format) {
      Assert.assertTrue(roots != null);

      QuantifyReturn qret = quantify(roots);

      return format_java_style(qret, elementwise, forall, format);
    }

    // This form allows the indicies and bounds to be modified before quantification
    protected static String[] format_java_style(QuantifyReturn qret, OutputFormat format) {
      return format_java_style(qret, false, true, format);
    }
    protected static String[] format_java_style(QuantifyReturn qret, boolean elementwise, OutputFormat format) {
      return format_java_style(qret, elementwise, true, format);
    }
    protected static String[] format_java_style(QuantifyReturn qret, boolean elementwise, boolean forall, OutputFormat format) {
      // build the "\forall ..." predicate
      String[] result = new String[qret.root_primes.length+2];
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
          }
          closing.append(quant_increment(idx, i, format));

          int_list.append(quant_var_initial_state(idx, low, format));
          conditions.append(quant_execution_condition(low, idx, high, format));

          if (elementwise && (i >= 1)) {
            VarInfoName[] _boundv = (VarInfoName[]) qret.bound_vars.get(i-1);
            VarInfoName _idx = _boundv[0], _low = _boundv[1];
            if (format == OutputFormat.JAVA)
               conditions.append(" || ");
            else
               conditions.append(" && ");

            conditions.append(quant_element_conditions(_idx, _low, idx, low, format));
          }
        }
      }

      if (forall)
         result[0] = quant_format_forall(format);
      else
         result[0] = quant_format_exists(format);

      result[0] += (int_list + quant_separator1(format) +
                    conditions + quant_separator2(format) +
                    closing + quant_step_terminator(format));
      result[result.length-1] = ")";

      // stringify the terms
      for (int i=0; i < qret.root_primes.length; i++) {
        result[i+1] = qret.root_primes[i].name_using(format, null);
      }

      return result;
    }

    // This set of functions are helper functions to the quantification function.

    /**
     * This function creates a string that represents how to increment the variables involved
     * in quantification. Since the increment is not stated explicitly in the JML and ESC
     * formats this function merely returns an empty string for those formats.
     */
    protected static String quant_increment(VarInfoName idx, int i, OutputFormat format) {
      if (format != OutputFormat.JAVA) {
        return "";
      } else {
        if (i != 0) {
          return (", " + idx.esc_name() + "++");
        } else {
          return (idx.esc_name() + "++");
        }
      }
    }

    /**
     * This function returns a string that represents the initial condition for the index
     * variable.
     */
    protected static String quant_var_initial_state(VarInfoName idx,
                                                    VarInfoName low,
                                                    OutputFormat format) {
      if (format == OutputFormat.JAVA) {
        return idx.esc_name() + " == " + low.esc_name();
      } else {
        return idx.name_using(format, null);
      }
    }

    /**
     * This function returns a string that represents the execution condition for the
     * quantification.
     */
    protected static String quant_execution_condition(VarInfoName low,
                                                      VarInfoName idx,
                                                      VarInfoName high,
                                                      OutputFormat format) {
      if (format == OutputFormat.JAVA) {
        return idx.esc_name() + " <= " + high.esc_name();
      } else {
        return low.name_using(format, null) + " <= " + idx.name_using(format, null) + " && " +
          idx.name_using(format, null) + " <= " + high.name_using(format, null);
      }
    }

    /**
     * This function returns a string representing the extra conditions necessary if the
     * quantification is element-wise.
     */
    protected static String quant_element_conditions(VarInfoName _idx,
                                                     VarInfoName _low,
                                                     VarInfoName idx,
                                                     VarInfoName low,
                                                     OutputFormat format) {
      StringBuffer conditions = new StringBuffer();

      if (ZERO.equals(_low)) {
        conditions.append(_idx.name_using(format, null));
      } else {
        conditions.append("(");
        conditions.append(_idx.name_using(format, null));
        conditions.append("-(");
        conditions.append(_low.name_using(format, null));
        conditions.append("))");
      }
      conditions.append(" == ");
      if (ZERO.equals(low)) {
        conditions.append(idx.name_using(format, null));
      } else {
        conditions.append("(");
        conditions.append(idx.name_using(format, null));
        conditions.append("-(");
        conditions.append(low.name_using(format, null));
        conditions.append("))");
      }

      return conditions.toString();
    }

    /**
     * This function returns a string representing how to format a forall statement in a
     * given output mode.
     */
    protected static String quant_format_forall(OutputFormat format) {
      if (format == OutputFormat.JAVA) {
        return "(for (int ";
      } else {
        return "(\\forall int ";
      }
    }

    /**
     * This function returns a string representing how to format an exists statement in a
     * given output mode.
     */
    protected static String quant_format_exists(OutputFormat format) {
      return "(\\exists int ";
    }

    /**
     * This function returns a string representing how to format the first seperation in
     * the quantification, that is, the one between the intial condition and the execution
     * condition.
     */
    protected static String quant_separator1(OutputFormat format) {
      if (format == OutputFormat.JML) {
        return "; ";
      } else {
        return "; (";
      }
    }

    /**
     * This function returns a string representing how to format the second seperation in
     * the quantification, that is, the one between the execution condition and the
     * assertion.
     */
    protected static String quant_separator2(OutputFormat format) {
      if (format == OutputFormat.ESCJAVA) {
        return ") ==> ";
      } else {
        return "; ";
      }
    }

    /**
     * This function returns a string representing how to format the final seperation in
     * the quantification, that is, the one between the assertion and any closing symbols.
     */
    protected static String quant_step_terminator(OutputFormat format) {
      if (format == OutputFormat.JAVA) {
        return ")";
      }
      return "";
    }

  } // QuantHelper

  // Special JML capability, since JML cannot format a sequence of elements,
  // often what is wanted is the name of the reference (we have a[], we want
  // a. This function provides the appropriate name for these circumstances.
  public VarInfoName JMLElementCorrector() {
    if (this instanceof Elements) {
      return ((Elements)this).term;
    } else if (this instanceof Slice) {
      return ((Slice)this).sequence.term;
    } else if (this instanceof Prestate) {
      return ((Prestate)this).term.JMLElementCorrector().applyPrestate();
    } else if (this instanceof Poststate) {
      return ((Poststate)this).term.JMLElementCorrector().applyPoststate();
    }
    return this;
  }


  // ============================================================
  // Transformation framework

  /**
   * Specifies a function that performs a transformation on VarInfoNames.
   **/
  public interface Transformer
  {
    /** Perform a transformation on the argument. */
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


  /**
   * Compare VarInfoNames alphabetically.
   **/
  public static class LexicalComparator implements Comparator {
    public int compare(Object o1, Object o2) {
      VarInfoName name1 = ((VarInfoName)o1);
      VarInfoName name2 = ((VarInfoName)o2);
      return name1.compareTo(name2);
    }
  }

}

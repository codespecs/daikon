package daikon;

import utilMDE.*;

import java.lang.ref.WeakReference;
import java.io.Serializable;
import java.util.*;

/**
 * VarInfoName is an type which represents the "name" of a variable.
 * Calling it a "name", however, is somewhat misleading.  It can be
 * some expression which includes more than one variable, term, etc.
 * We separate this from the VarInfo itself because clients wish to
 * manipulate names into new expressions independent of the VarInfo
 * which they might be associated with.
 **/
public abstract class VarInfoName
  implements Serializable, Comparable
{

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

  /**
   * Given the standard String representation of a variable name (from
   * a decls file), return the corresponding VarInfoName.  This cannot
   * handle generalized expressions, to name.equals(parse(e.name()))
   * is not certain to be true.
   **/
  public static VarInfoName parse(String name) {
    // x or this.x
    if ((name.indexOf('[') < 0) && (name.indexOf('(') < 0)) {
      // checking for only leagal characters would be more robust
      return (new Simple(name)).intern();
    }

    // a[]
    if (name.endsWith("[]")) {
      return parse(name.substring(0, name.length()-2)).applyElements();
    }

    // x.class
    if (name.endsWith(".class")) {
      return parse(name.substring(0, name.length()-6)).applyTypeOf();
    }

    // ??
    throw new UnsupportedOperationException("parse error: '" + name + "'");
  }

  /**
   * @return the string representation of this name, in the default
   * output format
   **/
  public String name() {
    if (name_cached == null) {
      name_cached = name_impl();
    }
    return name_cached;
  }
  private String name_cached = null;
  protected abstract String name_impl();

  /**
   * @return the string representation of this name, in the esc style
   * output format
   **/
  public String esc_name() {
    if (esc_name_cached == null) {
      esc_name_cached = esc_name_impl();
    }
    return esc_name_cached;
  }
  private String esc_name_cached = null;
  protected abstract String esc_name_impl();

  /**
   * @return the string representation of this name, in the Simplify
   * tool output format
   **/
  public String simplify_name() {
    if (simplify_name_cached == null) {
      simplify_name_cached = simplify_name_impl();
    }
    return simplify_name_cached;
  }
  private String simplify_name_cached = null;
  protected abstract String simplify_name_impl();

  /**
   * A simple identifier like "a", "this.foo", etc.
   **/
  public static class Simple extends VarInfoName {
    public final String name;
    public Simple(String name) {
      this.name = name;
    }
    protected String name_impl() {
      return name;
    }
    protected String esc_name_impl() {
      return "return".equals(name) ? "\\result" : name;
    }
    protected String simplify_name_impl() {
      String working = name;
      String prefix = "";
      String suffix = "";
      int dot = working.indexOf('.');
      while (dot >= 0) {
	String rest = working.substring(dot+1);
	working = working.substring(0, dot);
	prefix = "(select " + rest + " " + prefix;
	suffix = suffix + ")";
	dot = working.indexOf('.');
      }
      return prefix + working + suffix;
    }
  }

  /**
   * Returns a name for the size of this object (this object should be
   * a sequence).  Form is like "size(this)" or "this.length".
   **/
  public VarInfoName applySize() {
    return (new SizeOf(this)).intern();
  }

  /**
   * The size of a contained sequence; form is like "size(sequence)"
   * or "sequence.length".
   **/
  public static class SizeOf extends VarInfoName {
    public final VarInfoName sequence;
    public SizeOf(VarInfoName sequence) {
      this.sequence = sequence;
    }
    protected String name_impl() {
      return "size(" + sequence.name() + ")";
    }
    protected String esc_name_impl() {
      return sequence.esc_name() + ".length";
    }
    protected String simplify_name_impl() {
      return "(arrayLength " + sequence.simplify_name() + ")";
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
   * A function over a term, like "sum(argument)"
   **/
  public static class FunctionOf extends VarInfoName {
    public final String function;
    public final VarInfoName argument;
    public FunctionOf(String function, VarInfoName argument) {
      this.function = function;
      this.argument = argument;
    }
    protected String name_impl() {
      return function + "(" + argument.name() + ")";
    }
    protected String esc_name_impl() {
      return name_impl();
    }
    protected String simplify_name_impl() {
      return "(" + function + " " + argument.simplify_name() + ")";
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
    public final VarInfoName term;
    public TypeOf(VarInfoName term) {
      this.term = term;
    }
    protected String name_impl() {
      return term.name() + ".class";
    }
    protected String esc_name_impl() {
      return "\\typeof(" + term.esc_name() + ")";
    }
    protected String simplify_name_impl() {
      return "(typeof " + term.simplify_name() + ")";
    }
  }

  /**
   * Returns a name for a the prestate value of this object; form is
   * like "orig(this)" or "\old(this)".
   **/
  public VarInfoName applyPrestate() {
    return (new Prestate(this)).intern();
  }

  /**
   * The prestate value of a term, like "orig(term)"
   **/
  public static class Prestate extends VarInfoName {
    public final VarInfoName term;
    public Prestate(VarInfoName term) {
      this.term = term;
    }
    protected String name_impl() {
      return "orig(" + term.name() + ")";
    }
    protected String esc_name_impl() {
      return "\\old(" + term.esc_name() + ")";
    }
    protected String simplify_name_impl() {
       // XXX Is this right ???
      return "(orig " + term.simplify_name() + ")";
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
   * Returns a name for the decrement of this term, like "this-1".
   **/
  public VarInfoName applyDecrement() {
    return (new Decrement(this)).intern();
  }

  /**
   * One less than some other value
   **/
  public static class Decrement extends VarInfoName {
    public final VarInfoName term;
    public Decrement(VarInfoName term) {
      this.term = term;
    }
    protected String name_impl() {
      return term.name() + "-1";
    }
    protected String esc_name_impl() {
      return term.name() + "-1";
    }
    protected String simplify_name_impl() {
      return "(- " + term.name() + " 1)";
    }
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
    public final VarInfoName term;
    public Elements(VarInfoName term) {
      this.term = term;
    }
    protected String name_impl() {
      return term.name() + "[]";
    }
    protected String esc_name_impl() {
       // XXX Is this right ???
      return "\\elements(" + term.esc_name() + ")";
    }
    protected String simplify_name_impl() {
       // XXX Is this right ???
      return "(elements " + term.simplify_name() + ")";
    }
  }

  /**
   * Returns a name for an element selected from a sequence, like
   * "this[i]"
   **/
  public VarInfoName applySubscript(VarInfoName index) {
    // a[] -> a[index]
    return (new Subscript(this, index)).intern();
  }

  /**
   * An element from a sequence, like "sequence[index]"
   **/
  public static class Subscript extends VarInfoName {
    public final VarInfoName sequence;
    public final VarInfoName index;
    public Subscript(VarInfoName sequence, VarInfoName index) {
      this.sequence = sequence;
      this.index = index;
    }
    protected String name_impl() {
       // XXX Is this right ???
      return null;
    }
    protected String esc_name_impl() {
      // XXX Is this right ???
      return null;
    }
    protected String simplify_name_impl() {
      // XXX Is this right ???
      return null;
    }
  }

//    public static String addSubscript(String base, String subscript) {
//      // This logic may belong elsewhere (and the heuristics are incorrect anyway).
//      String suffix = "";
//      if (base.endsWith("[]")) {
//        base = base.substring(0, base.length()-2);
//      } else if (base.startsWith("orig(") && base.endsWith("[])")) {
//        // This is heuristic; I think it's probably OK.
//        base = base.substring(0, base.length()-3);
//        suffix = ")";
//      } else if (base.startsWith("\\old(") && base.endsWith(")")) {
//        // This is even more heuristic; I think it's probably also OK.
//        base = base.substring(0, base.length()-1);
//        suffix = ")";
//        // Even more heuristic; starting to get scary.
//        int subold = subscript.indexOf("\\old(");
//        while (subold != -1) {
//          int oldcloseparen = subscript.indexOf(")", subold);
//          Assert.assert(oldcloseparen != -1);
//          subscript = (subscript.substring(0, subold)
//                       + subscript.substring(subold+5, oldcloseparen)
//                       + subscript.substring(oldcloseparen+1));
//          subold = subscript.indexOf("\\old(");
//        }
//      } else {
//        throw new Error("what base? addSubscript(" + base + ", " + subscript + ")");
//      }
//      Assert.assert(subscript.indexOf("]") == -1);
//      Assert.assert(suffix.indexOf("]") == -1);
//      return base + "[" + subscript + "]" + suffix;
//    }

//    // The caller must assure that if the base is \old, so is the subscript.
//    public static String addSubscript_esc(String base, String subscript) {
//      // This logic may belong elsewhere (and the heuristics are incorrect anyway).
//      String suffix = "";
//      Assert.assert (! base.endsWith("[]"));

//      if (base.startsWith("\\old(") && base.endsWith(")")) {
//        base = base.substring(0, base.length()-1);
//        suffix = ")";
//        int subold = subscript.indexOf("\\old(");
//        while (subold != -1) {
//          int oldcloseparen = subscript.indexOf(")", subold);
//          Assert.assert(oldcloseparen != -1);
//          subscript = (subscript.substring(0, subold)
//                       + subscript.substring(subold+5, oldcloseparen)
//                       + subscript.substring(oldcloseparen+1));
//          subold = subscript.indexOf("\\old(");
//        }
//      }
//      Assert.assert(subscript.indexOf("]") == -1);
//      Assert.assert(suffix.indexOf("]") == -1);
//      return base + "[" + subscript + "]" + suffix;
//    }

  /**
   * Return an array of two strings:
   * an esc forall quantifier, and
   * the expression for the element at index i of the array
   **/
  public String[] esc_forall() {
    String[] index_range = null; // XXX index_range();
    if (index_range.length != 3) {
      throw new Error("index_range failed for " + name());
    }
    return new String[] {
      "\\forall int i; (" + index_range[0] + " <= i && i <= " + index_range[1] + ") ==> ",
      index_range[2],
    };
  }

  /**
   * Return an array of three strings:
   * an esc forall quantifier, and
   * the expressions for the elements at index i of the two arrays
   **/
  public static String[] esc_forall_2(VarInfo var1, VarInfo var2) {
    String[] index_range1 = var1.index_range();
    String[] index_range2 = var2.index_range();
    Assert.assert(index_range1.length == 3, "no index_range: " + var1.name);
    Assert.assert(index_range2.length == 3, "no index_range: " + var2.name);
    String[] esc_forall1 = var1.esc_forall();
    String elt2 = index_range2[2];
    if (! index_range1[0].equals(index_range2[0])) {
      int i_pos = elt2.lastIndexOf("[i]");
      elt2 = elt2.substring(0, i_pos+2)
        + "-" + index_range1[0] + "+" + index_range2[0] + "]"
        + elt2.substring(i_pos+3);
    }
    return new String[] {
      esc_forall1[0],
      esc_forall1[1],
      elt2,
    };
  }

  // public static boolean isOrigVarName(String s) {
  //   return ((s.startsWith("orig(") && s.endsWith(")"))
  //           || (s.startsWith("\\old(") && s.endsWith(")")));
  // }

//    // takes an "orig()" var and gives a pair of [name, esc_name] for a
//    // variable or expression in the post-state which is equal to this one.
//    public String[] postStateEquivalent() {
//      return otherStateEquivalent(true);
//    }

//    public String[] preStateEquivalent() {
//      return otherStateEquivalent(false);
//    }

//    public String[] otherStateEquivalent(boolean post) {

//      // Below is equivalent to:
//      // Assert.assert(post == isOrigVar());
//      if (post != isOrigVar()) {
//        throw new Error("Shouldn't happen (should it?): "
//                        + (post ? "post" : "pre") + "StateEquivalent(" + name + ")");
//        // return new String[] { name, esc_name };
//      }

//      Assert.assert(isCanonical());
//      Vector equal_vars = null; // XXX equalTo();
//      for (int i=0; i<equal_vars.size(); i++) {
//        VarInfo vi = (VarInfo)equal_vars.elementAt(i);
//        if (post != vi.isOrigVar()) {
//          // System.out.println("postStateEquivalent(" + name + ") = " + vi.name);
//          return new String[] { vi.name.name(), vi.name.esc_name() };
//        }
//      }

//      // Didn't find an exactly equal variable; try LinearBinary.
//      // (Should also try other exact invariants.)
//      {
//        Vector lbs = LinearBinary.findAll(this);
//        for (int i=0; i<lbs.size(); i++) {
//          LinearBinary lb = (LinearBinary) lbs.elementAt(i);
//          String lb_format = null;
//          String lb_format_esc = null;
//          if (this.equals(lb.var2())
//              && (post != lb.var1().isOrigVar())) {
//            lb_format = lb.format();
//            lb_format_esc = lb.format_esc();
//          } else if (this.equals(lb.var1())
//                     && (post != lb.var2().isOrigVar())) {
//            if ((lb.core.a == 1) || (lb.core.a == -1)) {
//              lb_format = lb.format_reversed();
//              lb_format_esc = lb.format_esc_reversed();
//            }
//          }
//          if (lb_format != null) {
//            int eq_pos;
//            eq_pos = lb_format.indexOf(" == "); // "interned"
//            Assert.assert(eq_pos != -1);
//            lb_format = lb_format.substring(eq_pos + 4);
//            eq_pos = lb_format_esc.indexOf(" == "); // "interned"
//            Assert.assert(eq_pos != -1);
//            lb_format_esc = lb_format_esc.substring(eq_pos + 4);
//            return new String[] { lb_format, lb_format_esc };
//          }
//        }
//      }

//      // Can't find post-state equivalent.
//      return null;
//    }


  public boolean equals(Object o) {
    return (o instanceof VarInfoName) && equals((VarInfoName) o);
  }

  public boolean equals(VarInfoName other) {
    return (other == this) || ((other != null) && (this.name().equals(other.name())));
  }

  public int hashCode() {
    return name().hashCode();
  }

  public int compareTo(Object o) {
    return name().compareTo((VarInfoName) o);
  }

  public String toString() {
    return name();
  }

}

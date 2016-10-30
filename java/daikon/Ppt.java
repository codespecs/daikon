// "Ppt" stands for "Program point" (but is easier to type).

package daikon;

import daikon.inv.Invariant; // for emptyInvList
import java.io.Serializable;
import java.util.*;
import plume.*;

/*>>>
import org.checkerframework.checker.initialization.qual.*;
import org.checkerframework.checker.interning.qual.*;
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

// Types of Ppt (program point) objects:
//  Ppt:  abstract base class
//  PptTopLevel:  pointed to by top-level PptMap object.  Contains all variables
//    and all data for those variables.
//  PptConditional:  contains only value tuples satisfying some condition.
//    Probably doesn't make sense for parent to be a PptSlice.
//  PptSlice:  contains a subset of variables.  Probably doesn't contain its
//    own data structure with all the values, but depends on its parent
//    (which may be any type of Ppt except a PptSlice, which wouldn't
//    make good sense).
// Originally, both PptConditional and PptSlice were called "Views"; but
// presently (6/2002), only Slices are called Views.

// Ppt is an abstract base class rather than an interface in part because
// interfaces cannot declare member variables.  I suspect that using
// members directly will be more efficient than calling accessor
// functions such as num_vars() and var_info_iterator().

// The common interface for all Ppt objects.
/*@UsesObjectEquals*/
public abstract class Ppt implements Serializable {
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20040914L;

  public VarInfo[] var_infos;

  protected Ppt(VarInfo[] var_infos) {
    this.var_infos = var_infos;
  }

  // The "name" and "ppt_name" fields were moved to PptTopLevel:  they take
  // up too much space in PptSlice objects.
  // This is safe if the receiver is @UnknownInitialization(PptTopLevel.class) OR
  // @UnknownInitialization(PptSlice.class), but annotations cannot express that.
  public abstract String name(
      /*>>>@GuardSatisfied @UnknownInitialization(PptTopLevel.class) Ppt this*/);

  /** Trim the collections used in this Ppt. */
  public void trimToSize() {
    for (VarInfo vi : var_infos) {
      vi.trimToSize();
    }
  }

  protected static final List<Invariant> emptyInvList = new ArrayList<Invariant>();

  /** Returns a string rep of the specified variable names */
  @SuppressWarnings("purity") // Impure side effects do not escape (string creation)
  /*@SideEffectFree*/
  public static String varNames(VarInfo[] infos) {
    StringBuffer sb = new StringBuffer();
    sb.append("(");
    if (infos.length == 0) {
      sb.append("<implication slice>");
    } else {
      sb.append(infos[0].name());
      for (int i = 1; i < infos.length; i++) {
        sb.append(", ");
        sb.append(infos[i].name());
      }
    }
    sb.append(")");
    return sb.toString();
  }

  /** Return a string representation of the variable names. */
  /*@SideEffectFree*/
  public String varNames(
      /*>>>@GuardSatisfied @UnknownInitialization(Ppt.class) @Raw(Ppt.class) Ppt this*/) {
    return (varNames(var_infos));
  }

  /**
   * Returns the varinfo_index of the variable whose name is varname. Returns -1 if there is no such
   * variable.
   */
  /*@Pure*/
  public int indexOf(String varname) {
    for (int i = 0; i < var_infos.length; i++) {
      if (var_infos[i].name().equals(varname)) {
        return i;
      }
    }
    return -1;
  }

  /** Returns the VarInfo with the specified name. Null if the name is not found. */
  /*@Pure*/
  public /*@Nullable*/ VarInfo find_var_by_name(String varname) {
    // System.out.printf ("Ppt.find_var_by_name(%s): %s%n", varname, this);
    int i = indexOf(varname);
    if (i == -1) {
      if (varname.contains("[]")) {
        return find_var_by_name(varname.replace("[]", "[..]"));
      }
      // System.out.printf ("Ppt.find_var_by_name: Didn't find %s or %s in %s%n", varname, varname.replace ("[]", "[..]"), this);
      return null;
    } else {
      return var_infos[i];
    }
  }

  public boolean containsVar(VarInfo vi) {
    // There's gotta be a faster way of doing this.  I don't want to
    // use a HashSet for var_infos because various things clobber
    // this.var_infos.
    for (VarInfo elt : var_infos) {
      if (elt == vi) {
        return true;
      }
    }
    return false;
  }

  // It might make more sense to put the sorting into
  // PptMap.sortedIterator(), for example, but it's in here for now

  // Check if o1 and o2 are both main exits (combined or only exits)
  // If so, compare their name without the EXIT[line]
  // If the name is the same, return 0, otherwise
  // Orders ppts by the name, except . and : are swapped
  //   so that Foo:::OBJECT and Foo:::CLASS are processed before Foo.method.
  public static final class NameComparator implements Comparator<PptTopLevel> {
    /*@Pure*/
    public int compare(PptTopLevel p1, PptTopLevel p2) {
      if (p1 == p2) {
        return 0;
      }

      String name1 = p1.name();
      String name2 = p2.name();

      String swapped1 = swap(name1, '.', ':');
      String swapped2 = swap(name2, '.', ':');

      return swapped1.compareTo(swapped2);
    }

    static String swap(String s, char a, char b) {
      final char magic = '\255';
      return s.replace(a, magic).replace(b, a).replace(magic, b);
    }
  }
}

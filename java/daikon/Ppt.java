// "Ppt" stands for "Program point" (but is easier to type).

package daikon;

import java.util.*;
import java.io.Serializable;
import utilMDE.*;

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
public abstract class Ppt
  implements Serializable
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20030929L;

  // The "name" and "ppt_name" fields were moved to PptTopLevel:  they take
  // up too much space in PptSlice objects.
  public abstract String name();

  protected Ppt() {
  }

  public VarInfo[] var_infos;

  /** Trim the collections used in this Ppt. */
  public void trimToSize() {
    for (int i=0; i < var_infos.length; i++) {
      var_infos[i].trimToSize();
    }
  }

  /**
   * Give sample data to this ppt to process.
   * @return a List of Invariants that have weakened due to the
   * processing of the sample.
   **/
  abstract List add(ValueTuple vt, int count);

  protected static final List emptyList = new ArrayList();

  public static String varNames(VarInfo[] infos) {
    StringBuffer sb = new StringBuffer();
    sb.append("(");
    if (infos.length == 0) {
      sb.append("<implication slice>");
    } else {
      sb.append(infos[0].name.name());
      for (int i=1; i<infos.length; i++) {
        sb.append(", ");
        sb.append(infos[i].name.name());
      }
    }
    sb.append(")");
    return sb.toString();
  }

  // Cache, so the value doesn't have to be repeatedly recomputed.
  private String varNames = null;

  /** Return a string representation of the variable names. */
  public String varNames() {
    if (varNames == null) {
      varNames = varNames(var_infos);
    }
    return varNames;
  }

  public VarInfo findVar(VarInfoName viname) {
    for (int i=0; i<var_infos.length; i++) {
      if (viname.equals(var_infos[i].name))
        return var_infos[i];
    }
    return null;
  }

  public VarInfo findVar_debugging(VarInfoName viname) {
    for (int i=0; i<var_infos.length; i++) {
      System.out.println("Checking " + viname.name() + " against " + var_infos[i].name.name());
      System.out.println("  Checking " + viname + " against " + var_infos[i].name);
      if (viname.equals(var_infos[i].name))
        return var_infos[i];
    }
    return null;
  }

  public VarInfo findVarByRepr(String repr) {
    for (int i=0; i<var_infos.length; i++) {
      if (repr.equals(var_infos[i].name.repr())) {
        return var_infos[i];
      }
    }
    return null;
  }

  /**
   * This should never be used if the VarInfoName is available; it is a
   * convenience method used when parsing from programs or user input.
   **/
  public VarInfo findVar(String name) {
    return findVar(VarInfoName.parse(name));
  }

  /**
   * Return a list of all variables that appear in every Ppt in ppts.
   * The result does NOT include static constants, as it will be used to
   * index into ValueTuple, which omits static constants.
   * @param ppts a vector of PptTopLevel objects.
   **/
  public static final VarInfo[] common_vars(List ppts) {
    Vector result = new Vector();
    Assert.assertTrue(ppts.size() > 1);
    // First, get all the variables from the first program point.
    {
      PptTopLevel ppt = (PptTopLevel) ppts.get(0);
      VarInfo[] vars = ppt.var_infos;
      for (int i=0; i<vars.length; i++) {
        if (vars[i].isStaticConstant())
          continue;
        result.add(vars[i]);
      }
    }
    // Now, for each subsequent program point, remove variables that don't
    // appear in it.
    for (int i=1; i<ppts.size(); i++) {
      PptTopLevel ppt = (PptTopLevel) ppts.get(i);
      VarInfo[] vars = ppt.var_infos;
      // Remove from result any variables that do not occur in vars
      VARLOOP:
      for (int rindex=result.size()-1; rindex>=0; rindex--) {
        VarInfo rvar = (VarInfo) result.get(rindex);
        for (int vindex=0; vindex<vars.length; vindex++) {
          VarInfo vvar = vars[vindex];
          if (rvar.isSimilarVarInfo(vvar)) {
            // do not remove
            continue VARLOOP;
          }
        }
        result.remove(rindex);
      }
    }
    return (VarInfo[]) result.toArray(new VarInfo[result.size()]);
  }

  public boolean containsVar (VarInfo vi) {
    // There's gotta be a faster way of doing this.  I don't want to
    // use a HashSet for var_infos because various things clobber
    // this.var_infos.
    int result = 0;
    for (int i = 0; i < var_infos.length; i++) {
      if (var_infos[i] == vi) {
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
  public static final class NameComparator implements Comparator {
    public int compare(Object o1, Object o2) {

      if ((o1 instanceof PptTopLevel) && (o2 instanceof PptTopLevel)) {
        PptTopLevel p1 = (PptTopLevel) o1;
        PptTopLevel p2 = (PptTopLevel) o2;
      }

      String name1 = ((Ppt) o1).name();
      String name2 = ((Ppt) o2).name();

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

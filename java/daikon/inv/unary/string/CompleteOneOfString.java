package daikon.inv.unary.string;

import daikon.*;
import daikon.inv.*;
import java.io.*;
import java.util.*;
import plume.*;

/*>>>
import org.checkerframework.checker.interning.qual.*;
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.dataflow.qual.*;
import org.checkerframework.framework.qual.*;
import typequals.*;
*/

/**
 * Tracks every unique value and how many times it occurs. Prints as either {@code x has no values}
 * or as {@code x has values: "v1" "v2" "v3" ...}.
 */
public final class CompleteOneOfString extends SingleString {
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20091210L;

  /** Information about each value encountered */
  public static class Info implements Serializable {
    static final long serialVersionUID = 20091210L;
    public String val;
    public int cnt;

    public Info(String val, int cnt) {
      this.val = val.intern();
      this.cnt = cnt;
    }

    private void readObject(ObjectInputStream in) throws IOException, ClassNotFoundException {
      in.defaultReadObject();
      if (val != null) val = val.intern();
    }
  }

  /** List of values seen */
  /*@Unused(when=Prototype.class)*/
  public List<Info> vals;

  /** Boolean. True iff PrintableString invariants should be considered. */
  public static boolean dkconfig_enabled = false;

  public CompleteOneOfString(PptSlice slice) {
    super(slice);
    vals = new ArrayList<Info>();
  }

  public /*@Prototype*/ CompleteOneOfString() {
    super();
  }

  private static /*@Prototype*/ CompleteOneOfString proto =
      new /*@Prototype*/ CompleteOneOfString();

  /** Returns the prototype invariant for CompleteOneOFString */
  public static /*@Prototype*/ CompleteOneOfString get_proto() {
    return proto;
  }

  /** returns whether or not this invariant is enabled */
  @Override
  public boolean enabled() {
    return dkconfig_enabled;
  }

  /** instantiate an invariant on the specified slice */
  @Override
  public CompleteOneOfString instantiate_dyn(
      /*>>> @Prototype CompleteOneOfString this,*/ PptSlice slice) {
    return new CompleteOneOfString(slice);
  }

  /** Return description of invariant. Only Daikon format is implemented. */
  /*@SideEffectFree*/
  @Override
  public String format_using(/*>>>@GuardSatisfied CompleteOneOfString this,*/ OutputFormat format) {
    if (format == OutputFormat.DAIKON) {
      if (vals.size() == 0) {
        return var().name() + "has no values";
      }
      StringBuilder out = new StringBuilder(vals.get(0).val.length() * vals.size());
      out.append(var().name() + " has values: ");
      for (Info val : vals) {
        out.append(String.format(" %s[%d]", val.val, val.cnt));
      }
      return out.toString();
    } else {
      return format_unimplemented(format);
    }
  }

  /** Check to see if a only contains printable ascii characters */
  @Override
  public InvariantStatus add_modified(/*@Interned*/ String a, int count) {
    return check_modified(a, count);
  }

  /** Check to see if a only contains printable ascii characters */
  @Override
  public InvariantStatus check_modified(/*@Interned*/ String a, int count) {
    for (Info val : vals) {
      if (val.val.equals(a)) {
        val.cnt += count;
        return InvariantStatus.NO_CHANGE;
      }
    }
    vals.add(new Info(a, count));
    return InvariantStatus.NO_CHANGE;
  }

  @Override
  protected double computeConfidence() {
    ValueSet vs = ppt.var_infos[0].get_value_set();
    if (vs.size() > 0) {
      return Invariant.CONFIDENCE_JUSTIFIED;
    } else {
      return Invariant.CONFIDENCE_UNJUSTIFIED;
    }
  }

  /**
   * Returns whether or not this is obvious statically. The only check is for static constants which
   * are obviously printable (or not) from their values.
   */
  /*@Pure*/
  @Override
  public /*@Nullable*/ DiscardInfo isObviousStatically(VarInfo[] vis) {
    return super.isObviousStatically(vis);
  }

  /**
   * Same formula if each value is the same and has the same count. Not implemented for now, just
   * presumed to be false.
   */
  /*@Pure*/
  @Override
  public boolean isSameFormula(Invariant o) {
    return false;
  }
}

package daikon.inv;

import daikon.*;
import java.util.*;
import plume.UtilMDE;

/*>>>
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.dataflow.qual.*;
import typequals.*;
*/

/**
 * This is a special invariant used internally by Daikon to represent an antecedent invariant in an
 * implication where that antecedent consists of two invariants anded together.
 */
public class AndJoiner extends Joiner {
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20030822L;

  private AndJoiner(PptSlice ppt, Invariant left, Invariant right) {
    super(ppt, left, right);
  }

  public AndJoiner(PptTopLevel ppt, Invariant left, Invariant right) {
    super(ppt, left, right);
  }

  @Override
  protected double computeConfidence() {
    return Invariant.confidence_and(left.computeConfidence(), right.computeConfidence());
  }

  @Override
  public String repr(/*>>>@GuardSatisfied AndJoiner this*/) {
    return "[" + left.repr() + " and " + right.repr() + "]";
  }

  /*@SideEffectFree*/
  @Override
  public String format_using(/*>>>@GuardSatisfied AndJoiner this,*/ OutputFormat format) {
    List<Invariant> invs = conjuncts();
    List<String> invStrings = new ArrayList<String>(invs.size());
    for (Invariant inv : invs) {
      invStrings.add(inv.format_using(format));
    }
    if (format == OutputFormat.DAIKON) {
      return UtilMDE.join(invStrings, " and ");
    } else if (format == OutputFormat.ESCJAVA
        || format.isJavaFamily()
        || format == OutputFormat.CSHARPCONTRACT) {
      // Forrest
      return "(" + UtilMDE.join(invStrings, ") && (") + ")";
    } else if (format == OutputFormat.SIMPLIFY) {
      return "(AND" + UtilMDE.join(invStrings, " ") + ")";
    } else {
      return format_unimplemented(format);
    }
  }

  public List<Invariant> conjuncts(/*>>>@GuardSatisfied AndJoiner this*/) {
    List<Invariant> result = new ArrayList<Invariant>(2);
    if (left instanceof AndJoiner) {
      result.addAll(((AndJoiner) left).conjuncts());
    } else {
      result.add(left);
    }
    if (right instanceof AndJoiner) {
      result.addAll(((AndJoiner) right).conjuncts());
    } else {
      result.add(right);
    }
    return result;
  }

  /*@Pure*/
  @Override
  public /*@Nullable*/ DiscardInfo isObviousDynamically(VarInfo[] vis) {
    // Don't call super.isObviousDynamically(vis);

    DiscardInfo leftObvious = left.isObviousDynamically(vis);
    DiscardInfo rightObvious = right.isObviousDynamically(vis);
    if (leftObvious != null && rightObvious != null) {
      return new DiscardInfo(
          this,
          DiscardCode.obvious,
          "Left obvious: "
              + leftObvious.discardString()
              + Global.lineSep
              + "Right obvious: "
              + rightObvious.discardString());
    }
    return null;
  }

  /*@Pure*/
  @Override
  public /*@Nullable*/ DiscardInfo isObviousStatically(VarInfo[] vis) {
    DiscardInfo leftObvious = left.isObviousStatically(vis);
    DiscardInfo rightObvious = right.isObviousStatically(vis);
    if (leftObvious != null && rightObvious != null) {
      DiscardInfo result =
          new DiscardInfo(
              this,
              DiscardCode.obvious,
              "Left obvious: "
                  + leftObvious.discardString()
                  + Global.lineSep
                  + "Right obvious: "
                  + rightObvious.discardString());
      return result;
    } else {
      return null;
    }
  }

  /*@Pure*/
  @Override
  public boolean isSameInvariant(Invariant other) {
    return super.isSameInvariant(other);
  }

  @Override
  public boolean enabled(/*>>> @Prototype AndJoiner this*/) {
    throw new Error("do not invoke " + getClass() + ".enabled()");
  }

  @Override
  public boolean valid_types(/*>>> @Prototype AndJoiner this,*/ VarInfo[] vis) {
    throw new Error("do not invoke " + getClass() + ".valid_types()");
  }

  @Override
  protected /*@NonPrototype*/ AndJoiner instantiate_dyn(
      /*>>> @Prototype AndJoiner this,*/ PptSlice slice) {
    throw new Error("do not invoke " + getClass() + ".instantiate_dyn()");
  }
}

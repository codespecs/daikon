package daikon.inv;

import daikon.*;

/*>>>
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

public abstract class Joiner extends Invariant {

  static final long serialVersionUID = 20030822L;

  public Invariant left;
  public Invariant right;

  @SuppressWarnings("nullness") // initialization: checker weakness:  unused constructor
  protected Joiner(PptSlice ppt) {
    super(ppt);
    throw new Error("Don't instantiate a Joiner this way.");
  }

  Joiner(PptSlice ppt, Invariant left, Invariant right) {
    super(ppt);
    assert ppt instanceof PptSlice0;

    this.left = left;
    this.right = right;
  }

  public Joiner(PptTopLevel ppt, Invariant left, Invariant right) {
    // Need a duplicate check

    this(ppt.joiner_view, left, right);
  }

  public abstract String repr(/*>>>@GuardSatisfied Joiner this*/);

  // I think we don't resurrect joiners
  protected Invariant resurrect_done(int[] permutation) {
    throw new UnsupportedOperationException();
  }

  /*@SideEffectFree*/
  public abstract String format_using(/*>>>@GuardSatisfied Joiner this,*/ OutputFormat format);

  /*@Pure*/
  public boolean isValidEscExpression() {
    return left.isValidEscExpression() && right.isValidEscExpression();
  }

  /*@Pure*/
  public boolean isObviousDerived() {
    return false;
  }

  public /*@Nullable*/ DiscardInfo isObviousImplied() {
    return null;
  }

  /*@Pure*/
  public boolean isSameInvariant(Invariant other) {
    if (!getClass().equals(other.getClass())) {
      return false;
    }

    Joiner otherAsJoiner = (Joiner) other;

    if (left == otherAsJoiner.left && right == otherAsJoiner.right) {
      return true;
    }

    return left.isSameInvariant(otherAsJoiner.left) && right.isSameInvariant(otherAsJoiner.right);
  }

  /*@Pure*/
  public boolean isSameFormula(Invariant other) {
    if (!getClass().equals(other.getClass())) return false;
    Joiner other_joiner = (Joiner) other;
    // Guards are necessary because the contract of isSameFormula states
    // that the argument is of the same class as the receiver.
    // Also use isSameInvariant because the joined parts might be over
    // distinct slices; don't make "a=b => c=d" be isSameFormula as
    // "e=f => g=h".
    return ((left.getClass() == other_joiner.left.getClass())
        // && left.isSameFormula(other_joiner.left)
        && left.isSameInvariant(other_joiner.left)
        && (right.getClass() == other_joiner.right.getClass())
        // && right.isSameFormula(other_joiner.right)
        && right.isSameInvariant(other_joiner.right));
  }

  /*@Pure*/
  public boolean isInteresting() {
    return (left.isInteresting() && right.isInteresting());
  }
}

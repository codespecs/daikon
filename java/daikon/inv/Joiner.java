package daikon.inv;

import daikon.*;
import daikon.inv.DiscardInfo;

import utilMDE.Assert;

public abstract class Joiner
  extends Invariant {

  static final long serialVersionUID = 20030822L;

  public Invariant left;
  public Invariant right;

  protected Joiner(PptSlice ppt) {
    super(ppt);
    throw new Error("Don't instantiate a Joiner this way.");
  }

  Joiner(PptSlice ppt, Invariant left, Invariant right) {
    super(ppt);
    Assert.assertTrue(ppt instanceof PptSlice0);

    this.left = left;
    this.right = right;
  }

  public Joiner(PptTopLevel ppt,
                Invariant left,
                Invariant right) {
    // Need a duplicate check

    this(ppt.joiner_view, left, right);
  }

  public abstract String repr();

    // I think we don't resurrect joiners
  protected Invariant resurrect_done(int[] permutation) {
    throw new UnsupportedOperationException();
  }

  public abstract String format_using(OutputFormat format);

  public boolean isValidEscExpression() {
    return left.isValidEscExpression() &&
      right.isValidEscExpression();
  }

  public boolean isObviousDerived() {
    return false;
  }

  public DiscardInfo isObviousImplied() {
    return null;
  }

  public boolean isSameInvariant(Invariant other) {
    if (!getClass().equals(other.getClass()))
      return false;

    Joiner otherAsJoiner = (Joiner)other;

    return left.isSameInvariant(otherAsJoiner.left) &&
      right.isSameInvariant(otherAsJoiner.right);
  }

  public boolean isInteresting() {
    return (left.isInteresting() && right.isInteresting());
  }
}

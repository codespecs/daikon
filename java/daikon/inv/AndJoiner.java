package daikon.inv;

import daikon.*;

public class AndJoiner
  extends Joiner
{
  static final long serialVersionUID = 20020722L;

  private AndJoiner(PptSlice ppt, Invariant left, Invariant right) {
    super(ppt, left, right);
  }

  public AndJoiner(PptTopLevel ppt,
                   Invariant left,
                   Invariant right) {
    super(ppt, left, right);
  }

  protected double computeProbability() {
    return left.computeProbability() * right.computeProbability();
  }

  public String repr() {
    return "[" + left.repr() + " and " +
      right.repr() + "]";
  }

  public String format_using(OutputFormat format) {
    String leftFormat = left.format_using(format);
    String rightFormat = right.format_using(format);
    if (format == OutputFormat.DAIKON) {
      return leftFormat + " and " + rightFormat;
    } else if (format == OutputFormat.JML || format == OutputFormat.ESCJAVA) {
      return "(" + leftFormat + ") && (" + rightFormat + ")";
    } else {
      return format_unimplemented(format);
    }
  }

  public boolean isObviousImplied() {
    return left.isObvious() && right.isObvious();
  }

  public boolean isSameInvariant(Invariant other) {
    return super.isSameInvariant(other);
  }
}

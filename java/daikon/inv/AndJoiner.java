package daikon.inv;

import daikon.*;

public class AndJoiner
  extends Joiner
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
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

  public boolean isObviousDynamically() {
    return left.isObviousDynamically() && right.isObviousDynamically();
  }

  public boolean isSameInvariant(Invariant other) {
    return super.isSameInvariant(other);
  }
}

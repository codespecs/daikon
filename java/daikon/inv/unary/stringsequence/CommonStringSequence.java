package daikon.inv.unary.stringsequence;

import daikon.*;
import daikon.inv.*;
import utilMDE.*;


/**
 * Represents string sequences that contain a common subset.  Prints as
 * "{s1, s2, s3, ...} subset of x[]"
 **/
public class CommonStringSequence
  extends SingleStringSequence
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /**
   * Boolean.  True iff CommonStringSequence invariants should be considered.
   **/
  public static boolean dkconfig_enabled = false;

  private int elts;
  private String[] intersect = null;

  protected CommonStringSequence(PptSlice ppt) {
    super(ppt);
  }

  public static CommonStringSequence instantiate(PptSlice ppt) {
    if (!dkconfig_enabled) return null;
    return new CommonStringSequence(ppt);
  }

  // Don't write clone, because this.intersect is read-only
  // protected Object clone();

  public String repr() {
    return "CommonStringSequence " + varNames() + ": "
      + "elts=\"" + elts;
  }

  private String printIntersect() {
    if (intersect==null)
      return "{}";

    String result = "{";
    for (int i=0; i<intersect.length; i++) {
      result += intersect[i];
      if (i!=intersect.length-1)
        result += ", ";
    }
    result += "}";
    return result;
  }

  public String format_using(OutputFormat format) {
    if (format == OutputFormat.DAIKON) return format_daikon();
    if (format == OutputFormat.IOA) return format_ioa();

    return format_unimplemented(format);
  }

  public String format_daikon() {
    return (printIntersect() + " subset of " + var().name);
  }

  /* IOA */
  public String format_ioa() {
    String vname = var().name.ioa_name();
    return (printIntersect() + " \\in " + vname);
  }

  public void add_modified(String[] a, int count) {
    if (intersect==null)
      intersect = a;
    else {
      String[] tmp = new String[intersect.length];
      int    size = 0;
      for (int i=1; i<a.length; i++)
        if ((ArraysMDE.indexOf(intersect, a[i])!=-1) &&
            ((size==0) ||
             (ArraysMDE.indexOf(ArraysMDE.subarray(tmp,0,size), a[i])==-1)))
          tmp[size++] = a[i];

      if (size==0) {
        destroyAndFlow();
        VarInfo var = var();

        discardCode = DiscardCode.bad_sample;
        discardString = printIntersect()+" not a subset of "+var.name.name();
        return;
      }
      intersect = ArraysMDE.subarray(tmp, 0, size);
    }
    intersect = (String[]) Intern.intern(intersect);
    elts++;
  }

  protected double computeProbability() {
    if (falsified) {
      return Invariant.PROBABILITY_NEVER;
    } else {
      return Math.pow(.9, elts);
    }
  }

  public boolean isObviousImplied() {
    return false;
  }

  public boolean isSameFormula(Invariant other) {
    Assert.assertTrue(other instanceof CommonStringSequence);
    return true;
  }
}

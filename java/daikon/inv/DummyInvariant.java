package daikon.inv;

import daikon.*;
import utilMDE.Assert;
import java.util.HashSet;
import java.util.Iterator;

/**
 * This is a special invariant used internally by Daikon to represent
 * invariants whose meaning Daikon doesn't understand. The only
 * operation that can be performed on a DummyInvariant is to print it.
 * For instance, dummy invariants can be created to correspond to
 * splitting conditions, when no other invariant in Daikon's grammar
 * is equivalent to the condition.
 *
 * To use dummy invariants for splitting conditions, the configuration
 * option daikon.PptTopLevel.dummy_invariant_level must be set, and
 * formatting information must be supplied in the splitter info file.
 **/
public class DummyInvariant
  extends Invariant
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20030220L;

  public String daikonFormat;
  public String javaFormat;
  public String escFormat;
  public String simplifyFormat;
  public String ioaFormat;
  public String jmlFormat;
  public String dbcFormat;

  private boolean negated = false;
  public boolean valid = false;

  public DummyInvariant(PptSlice ppt) {
    super(ppt);

    daikonFormat = "<dummy>";
    javaFormat = "format_java not implemented for dummy invariant";
    escFormat = "format_esc not implemented for dummy invariant";
    simplifyFormat = "format_simplify not implemented for dummy invariant";
    ioaFormat = "format_ioa not implemented for dummy invariant";
    jmlFormat = "format_jml not implemented for dummy invariant";
    dbcFormat = "format_dbc not implemented for dummy invariant";
  }

  public void setFormats(String daikon, String java, String esc,
                         String simplify, String ioa, String jml, String dbc) {
    if (daikon != null)
      daikonFormat = daikon;
    if (java != null)
      javaFormat = java;
    if (esc != null)
      escFormat = esc;
    if (simplify != null)
      simplifyFormat = simplify;
    if (ioa != null)
      ioaFormat = ioa;
    if (jml != null)
      jmlFormat = jml;
    if (dbc != null)
      dbcFormat = jml;

    // Note that java is missing from this disjuction on purpose
    if (daikon != null || esc != null || simplify != null || ioa != null)
      valid = true;
  }

  public DummyInvariant instantiate(PptTopLevel parent, VarInfo[] vars) {
    DummyInvariant inv = new DummyInvariant(ppt);
    inv.daikonFormat = this.daikonFormat;
    inv.javaFormat = this.javaFormat;
    inv.escFormat = this.escFormat;
    inv.simplifyFormat = this.simplifyFormat;
    inv.ioaFormat = this.ioaFormat;
    inv.valid = false; // Not valid until we find a slice for it
    Assert.assertTrue(!this.negated, "Only instantiated invariants " +
                      "should be negated");

    // Find between 1 and 3 unique variables, to pick a slice to put
    // this in.
    HashSet uniqVarsSet = new HashSet();
    for (int i = 0; i < vars.length; i++)
      uniqVarsSet.add(vars[i].canonicalRep());
    int sliceSize = uniqVarsSet.size();
    if (sliceSize > 3)
      sliceSize = 3;
    VarInfo[] newVars = new VarInfo[sliceSize];
    {
      Iterator it = uniqVarsSet.iterator();
      int i = 0;
      while (it.hasNext()) {
        newVars[i++] = (VarInfo)it.next();
        if (i == sliceSize)
          break;
      }
    }
    vars = newVars;
    Assert.assertTrue(vars.length >= 1 && vars.length <= 3);
    if (vars.length == 1) {
      PptSlice1 slice = parent.findSlice(vars[0]);
      if (slice == null) {
        slice = new PptSlice1(parent, vars);
        parent.addSlice(slice);
      }
      inv.ppt = slice;
    } else if (vars.length == 2) {
      if (vars[0] == vars[1])
        return inv;
      else if (vars[0].varinfo_index > vars[1].varinfo_index) {
        VarInfo tmp = vars[0];
        vars[0] = vars[1];
        vars[1] = tmp;
      }
      PptSlice2 slice = parent.findSlice(vars[0], vars[1]);
      if (slice == null) {
        slice = new PptSlice2(parent, vars);
        parent.addSlice(slice);
      }
      inv.ppt = slice;
    } else if (vars.length == 3) {
      if (vars[0] == vars[1] || vars[1] == vars[2] || vars[0] == vars[2])
        return inv;
      // bubble sort
      VarInfo tmp;
      if (vars[0].varinfo_index > vars[1].varinfo_index) {
        tmp = vars[0]; vars[0] = vars[1]; vars[1] = tmp;
      }
      if (vars[1].varinfo_index > vars[2].varinfo_index) {
        tmp = vars[1]; vars[1] = vars[2]; vars[2] = tmp;
      }
      if (vars[0].varinfo_index > vars[1].varinfo_index) {
        tmp = vars[0]; vars[0] = vars[1]; vars[1] = tmp;
      }
      PptSlice3 slice = parent.findSlice(vars[0], vars[1], vars[2]);
      if (slice == null) {
        slice = new PptSlice3(parent, vars);
        parent.addSlice(slice);
      }
      inv.ppt = slice;
    }
    inv.valid = true;
    return inv;
  }

  protected double computeProbability() {
    return 0;
  }

  public void negate() {
    negated = !negated;
  }

  public String format_using(OutputFormat format) {
    if (format == OutputFormat.DAIKON) return format_daikon();
    if (format == OutputFormat.IOA) return format_ioa();
    if (format == OutputFormat.JAVA) return format_java();
    if (format == OutputFormat.ESCJAVA) return format_esc();
    if (format == OutputFormat.SIMPLIFY) return format_simplify();
    if (format == OutputFormat.JML) return format_jml();
    if (format == OutputFormat.DBCJAVA) return format_dbc();

    return format_unimplemented(format);
  }

  public String format_daikon() {
    if (negated)
      return "not " + daikonFormat;
    else
      return daikonFormat;
  }

  public String format_java() {
    if (negated)
      return "!(" + javaFormat + ")";
    else
      return javaFormat;
  }

  public String format_esc() {
    if (negated)
      return "!(" + escFormat + ")";
    else
      return escFormat;
  }

  public String format_simplify() {
    if (negated)
      return "(NOT " + simplifyFormat + ")";
    else
      return simplifyFormat;
  }

  public String format_ioa() {
    if (negated)
      return "~(" + ioaFormat + ")";
    else
      return ioaFormat;
  }

  protected Invariant resurrect_done(int[] permutation) {
    throw new Error("Not implemented");
  }

  public String format_jml() {
    if (negated)
      return "!(" + jmlFormat + ")";
    else
      return ioaFormat;
  }

  public String format_dbc() {
    if (negated)
      return "!(" + dbcFormat + ")";
    else
      return ioaFormat;
  }

}

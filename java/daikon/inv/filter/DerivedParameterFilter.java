package daikon.inv.filter;

import daikon.PrintInvariants;
import daikon.VarInfo;
import daikon.inv.*;

/**
 * Filter for not printing an Invariant if its VarInfos return isDerivedParameterAndUninteresting ==
 * true.
 */
public class DerivedParameterFilter extends InvariantFilter {
  @Override
  public String getDescription() {
    return "Suppress parameter-derived postcondition invariants";
  }

  /** Boolean. If true, DerivedParameterFilter is initially turned on. */
  public static boolean dkconfig_enabled = true;

  public DerivedParameterFilter() {
    isOn = dkconfig_enabled;
  }

  /**
   * Returns true if the invariant describes changes made to pass-by-value parameters that shouldn't
   * be part of a routine's visible interface. E.g, suppose that "param" is a parameter to a Java
   * method. If "param" itself is modified, that change won't be visible to a caller, so we
   * shouldn't print it. If "param" points to an object, and that object is changed, that is
   * visible, but only if "param" hasn't changed; otherwise, we're reporting a change in some object
   * other than the one that was passed in.
   *
   * <p>More specifically, return true if the invariant is a post-state invariant, and a variable in
   * it is either a parameter, or a variable derived from a parameter, when we think that the
   * parameter itself may have changed by virtue of not having a "param == orig(param)" invariant.
   */
  @Override
  boolean shouldDiscardInvariant(Invariant inv) {
    if (inv.ppt.parent.ppt_name.isExitPoint()) {
      PrintInvariants.debugFiltering.fine("\tconsidering DPF for vars " + inv.ppt.varNames());
      for (int i = 0; i < inv.ppt.var_infos.length; i++) {
        VarInfo vi = inv.ppt.var_infos[i];
        // ppt has to be a PptSlice, not a PptTopLevel
        PrintInvariants.debugFiltering.fine("\tconsidering DPF for " + vi.name());
        if (vi.isDerivedParamAndUninteresting()) {
          // System.out.printf ("derived and uninteresting: %s\n", vi.name());
          return true;
        }
      }
    }
    return false;
  }
}

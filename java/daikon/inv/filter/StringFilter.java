package daikon.inv.filter;

import daikon.PrintInvariants;
import daikon.ProglangType;
import daikon.VarInfo;
import daikon.inv.Invariant;
import daikon.inv.binary.BinaryInvariant;
import daikon.inv.binary.twoString.StringEqual;
import daikon.inv.unary.scalar.NonZero;
import daikon.inv.unary.scalar.OneOfScalar;
import daikon.inv.unary.string.OneOfString;
import java.util.logging.Level;

/**
 * Suppress string invariants that are redundant for .NET. The following invariants will be
 * discarded:
 *
 * <ul>
 *   <li><code>x != null</code>, if <code>!string.IsNullOrEmpty(...)</code> or <code>
 *       !string.IsNullOrWhitespace(...)</code> is inferred.
 *   <li>Frame conditions for string properties, if the frame condition exists for the string
 *       (reference or value)
 *   <li>String properties, if {@link OneOfString} is inferred.
 *   <li><code>!string.IsNullOrEmpty(...)</code> if <code>!string.IsNullOrWhitespace(...)</code> is
 *       inferred.
 *   <li><code>string.IsNullOrWhitespace(...)</code> if <code>string.IsNullOrEmpty(...)</code> is
 *       inferred.
 * </ul>
 */
public class StringFilter extends InvariantFilter {

  /** Boolean. If true, StringFilter is initially turned on. */
  public static boolean dkconfig_enabled = false;

  public StringFilter() {
    isOn = dkconfig_enabled;
  }

  @Override
  public String getDescription() {
    return "Suppress string invariants that are redundant for .NET.";
  }

  boolean isNullOrEmptyVar(VarInfo var) {
    return var.name().startsWith("string.IsNullOrEmpty")
        || var.name().startsWith("String.IsNullOrEmpty");
  }

  boolean isNullOrWhitespaceVar(VarInfo var) {
    return var.name().startsWith("string.IsNullOrWhiteSpace")
        || var.name().startsWith("String.IsNullOrWhiteSpace");
  }

  boolean isFrame(VarInfo lhs, VarInfo rhs) {
    return lhs.is_prestate_version(rhs) || rhs.is_prestate_version(lhs);
  }

  boolean isFrame(Invariant invariant) {
    return invariant instanceof BinaryInvariant
        && isFrame(invariant.ppt.var_infos[0], invariant.ppt.var_infos[1]);
  }

  /**
   * Since strings are immutable, discard all frame conditions for the properties of the string if a
   * frame condition for the string (reference of value) is inferred.
   */
  boolean shouldDiscardDerivedStringFrameCondition(Invariant invariant) {
    if (isFrame(invariant)
        && invariant.ppt.var_infos[0].enclosing_var != null
        && invariant.ppt.var_infos[0].enclosing_var.type == ProglangType.STRING) {
      VarInfo str = invariant.ppt.var_infos[0].enclosing_var;

      for (Invariant other : str.ppt.getInvariants()) {
        if (!other.is_false() && isFrame(other)) {

          boolean refEq = other.ppt.var_infos[0] == str;
          boolean valEq =
              other instanceof StringEqual && other.ppt.var_infos[0].enclosing_var == str;

          if (refEq || (valEq && !(invariant instanceof StringEqual))) {
            return true;
          }
        }
      }
    }

    return false;
  }

  /**
   * Since properties are pure, discard all properties of the string if the possible values are
   * known (a {@link OneOfString} invariant is present).
   */
  boolean shouldDiscardDerivedStringInvariant(Invariant invariant) {
    if (invariant.ppt.var_infos.length == 1 && !(invariant instanceof OneOfString)) {
      VarInfo var = invariant.ppt.var_infos[0];

      if (var.enclosing_var != null && var.enclosing_var.type == ProglangType.STRING) {
        // variable is derived from a string

        for (Invariant other : var.ppt.getInvariants()) {
          if (!other.is_false()
              && other instanceof OneOfString
              && ((OneOfString) other).var().enclosing_var == var.enclosing_var) {
            // search for a variable.ToString() invariant

            return true;
          }
        }
      }
    }

    return false;
  }

  /**
   * <code>true</code> iff <code>invariant</code> encodes <code>x != null</code> and <code>
   * !string.IsNullOrEmpty(x)</code> is an inferred invariant</code>
   */
  boolean shouldDiscardNonNullInvariant(Invariant invariant) {
    if (invariant instanceof NonZero) {

      NonZero i = (NonZero) invariant;
      if (i.var().type == ProglangType.STRING) {

        for (Invariant other : i.var().ppt.getInvariants()) {
          if (!other.is_false() && other instanceof OneOfScalar) {
            OneOfScalar o = (OneOfScalar) other;

            if (o.var().enclosing_var == i.var()
                && (isNullOrEmptyVar(o.var()) || isNullOrWhitespaceVar(o.var()))
                && o.getElts()[0] == 0) {

              return true;
            }
          }
        }
      }
    }
    return false;
  }

  /**
   * <code>true</code> iff <code>invariant</code> encodes <code>!string.IsNullOrEmpty()</code> and
   * <code>!string.IsNullOrWhitespace()</code> is an inferred invariant</code>.
   */
  boolean shouldDiscardNullOrEmptyInvariant(Invariant invariant) {
    if (invariant instanceof OneOfScalar) {
      OneOfScalar i = (OneOfScalar) invariant;

      if (isNullOrEmptyVar(i.var()) && i.getElts()[0] == 0) {
        //invariant encodes !string.IsNullOrEmpty()

        for (Invariant other : i.var().ppt.getInvariants()) {
          if (!other.is_false() && other instanceof OneOfScalar) {
            OneOfScalar o = (OneOfScalar) other;

            if (o.var().enclosing_var == i.var().enclosing_var
                && isNullOrWhitespaceVar(o.var())
                && o.getElts()[0] == 0) {

              return true;
            }
          }
        }
      }
    }

    return false;
  }

  /**
   * <code>true</code> iff <code>invariant</code> encodes <code>string.IsNullOrWhitespace()</code>
   * and <code>string.IsNullOrEmpty()</code> is an inferred invariant</code>.
   */
  boolean shouldDiscardNullOrWhitespaceInvariant(Invariant invariant) {
    if (invariant instanceof OneOfScalar) {
      OneOfScalar i = (OneOfScalar) invariant;

      if (isNullOrWhitespaceVar(i.var()) && i.getElts()[0] == 1) {
        //invariant encodes !string.IsNullOrEmpty()

        for (Invariant other : i.var().ppt.getInvariants()) {
          if (!other.is_false() && other instanceof OneOfScalar) {
            OneOfScalar o = (OneOfScalar) other;

            if (o.var().enclosing_var == i.var().enclosing_var
                && isNullOrEmptyVar(o.var())
                && o.getElts()[0] == 1) {

              return true;
            }
          }
        }
      }
    }

    return false;
  }

  @Override
  boolean shouldDiscardInvariant(Invariant invariant) {

    if (PrintInvariants.debugFiltering.isLoggable(Level.FINE)) {
      PrintInvariants.debugFiltering.fine("\tEntering StringFilter.shouldDiscard");
    }

    return shouldDiscardDerivedStringFrameCondition(invariant)
        || shouldDiscardNonNullInvariant(invariant)
        || shouldDiscardDerivedStringInvariant(invariant)
        || shouldDiscardNullOrEmptyInvariant(invariant)
        || shouldDiscardNullOrWhitespaceInvariant(invariant);
  }
}

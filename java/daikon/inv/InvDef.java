package daikon.inv;

import daikon.*;
import daikon.inv.binary.*;
import daikon.suppress.*;
import utilMDE.*;

import java.lang.reflect.*;
import java.util.logging.Logger;

/**
 * Class that defines an invariant so that it can be searched for as
 * part of suppression.
 */
public class InvDef {

  /** Debug Tracer **/
  public static final Logger debug
                          = Logger.getLogger ("daikon.inv.InvDef");


  /**
   * Variables used by the invariant.  If v2 is null, this is a unary
   * invariant, if v2 is not null, then this is a binary invariant
   */
  VarInfo v1;
  VarInfo v2;

  /** invariant class **/
  Class inv_class;

  /** State to check for invariants with state **/
  Object state;

  /** true if the order of the variables was swapped **/
  boolean swap = false;

  /** true if invariant permutes by changing its class **/
  boolean swap_class = false;

  public static final long[] elts_zero = {0};
  public static final double[] elts_zero_float = {0.0};
  public static final long[] elts_minus_one = {-1};
  public static final double[] elts_minus_one_float = {-1.0};
  public static final long[] elts_plus_minus_one = {-1, 1};
  public static final double[] elts_plus_minus_one_float = {-1.0, 1.0};

  public InvDef (VarInfo v1, Class cls) {
    this.v1 = v1;
    inv_class = cls;
  }

  public InvDef (VarInfo v1, Class cls, Object state) {
    this (v1, cls);
    this.state = state;
  }

  public InvDef (VarInfo v1, VarInfo v2, Class cls) {

    debug.fine ("creating " + cls.getName() + " " + v1.name.name() + ", " +
                v2.name.name());
    // put the variables in their standard order
    if (v1.varinfo_index > v2.varinfo_index) {
      this.v1 = v2;
      this.v2 = v1;
      swap = true;
    } else {
      this.v1 = v1;
      this.v2 = v2;
      swap = false;
    }

    // If the specified class handles swapping with a different class,
    // get the class
    swap_class = true;
    try {
      Method swap_method = cls.getMethod ("swap_class", null);
      if (swap)
        cls = (Class) swap_method.invoke (null, null);
    } catch (Exception e) {
      swap_class = false;
    }

    this.inv_class = cls;

    debug.fine ("Created " + this);
  }

  public String toString() {
    String out = "v1=" + v1.name.name();
    if (v2 != null)
      out += ", v2=" + v2.name.name();
    return (out + ", class=" + inv_class.getName() + ", swap=" + swap
            + ", swap_class=" + swap_class);
  }

  public void set (SuppressionTemplate sup_template, int index) {
    if (v2 == null)
      sup_template.set (index, inv_class, v1);
    else
      sup_template.set (index, inv_class, v1, v2);
   }

  public boolean check (Invariant inv) {
    Assert.assertTrue (inv.getClass() == inv_class);

    debug.fine ("checking " + this);

    // If its a binary invariant that is swapped, make sure it matches
    if ((v2 != null) && !swap_class) {
      BinaryInvariant binv = (BinaryInvariant) inv;
      if (!binv.is_symmetric() && swap != binv.get_swap()) {
        debug.fine ("inv " + inv.format() + " doesn't match swap value, "
                    + "symmetric=" + binv.is_symmetric());
        return (false);
      }
    }

    // If a state was specified make sure it matches
    if (state != null) {
      if (!inv.state_match (state)) {
        debug.fine ("inv doesn't match state");
        return (false);
      }
    }

    debug.fine ("inv " + inv.format() + " matches");
    return (true);
  }
}

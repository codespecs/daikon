package daikon.suppress;

import daikon.inv.Invariant;

import java.util.*;
import java.io.Serializable;

import java.util.logging.Logger;

import utilMDE.Assert;

/**
 * Connects one Invariant to a set of suppressor invariants;
 * immutable.  When any of the suppressors are destroyed or weakened,
 * this becomes destroyed and the suppressed invariant is rechecked.
 * If the suppressee is actually still suppressed by the suppressor,
 * then a new SuppressionLink must be made.  For full documentation,
 * see this package's doc.  The multiplicity of SuppressionLinks: each
 * Invariant has one or zero suppressors (which are SuppressionLinks)
 * active, and any number of suppresses (also SuppressionLinks).  Each
 * SuppressionLink is connected to exactly ONE invariant on each side.
 * That is, SuppressionLinks are not shared between invariants.
 **/

public class SuppressionLink implements Serializable {

  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20030911L;

  /**
   * General debug tracer.
   **/
  public static final Logger debug =
    Logger.getLogger ("daikon.suppress.SuppresionLink");

  /**
   * List of Invariants that are suppressing this.suppressee. Never null.
   **/
  private final Invariant[] suppressors;

  /**
   * Suppressed invariant.  Never null.
   **/
  private final Invariant suppressee;


  // This is for debugging only.  For efficiency, we could remove the field.
  // (As of 10/24/2003, "getFactory()" is never called.)
  // /**
  //  * Original factory that generated this.  Never null.
  //  **/
  // private final SuppressionFactory factory;

  /**
   * What stage of use this link is in.  Gets modified.  Can be one of
   * CREATED, LINKED or UNLINKED.
   * <p>
   * CREATED means this was just created.  Suppressor and
   * suppressees do not know about this.
   * <p>
   * LINKED means this has been created and linked.  Suppressor and
   * suppressees know about this.
   * <p>
   * UNLINKED means this has been destroyed (unlinked).  Suppressor and
   * suppresses no longer know about this.  This should not be used again.
   **/
  private int state = CREATED;

  // See documentation for state.
  private static final int CREATED = 0;
  private static final int LINKED = 1;
  private static final int UNLINKED = 2;



  //////////////
  // Accessors

  // These are rather simple accessors and for performance, we may
  // consider making fields public.  But I don't know yet if there
  // will be more sophisticated indirection or caching, so we'll keep
  // these as methods for now.
  /**
   * Get the Invariant that this suppresses.
   * @return never null.
   **/
  public Invariant getSuppressee() {
    return suppressee;
  }

  // /**
  //  * Get the original factory that generated this.  Tells what type of
  //  * suppression this does.
  //  * @return never null.
  //  **/
  // public SuppressionFactory getFactory() {
  //   return factory;
  // }


  // Not used as of 10/24/2003.
  /**
   * Get the (unmodifiable) list of invariants that suppress this.
   * @return never null.
   **/
  public List getSuppressors() {
    return Collections.unmodifiableList (Arrays.asList(suppressors));
  }


  // Disabled because hashCode can be called before rep invariants are
  // satisfied when reading from a serialized file.

  //   public int hashCode() {
  //     if (this.factory == null) {
  //       throw new Error("Must have non-null factory");
  //     }
  //     if (this.suppressee == null) {
  //       throw new Error("Must have non-null suppressees");
  //     }
  //     if (this.suppressors == null) {
  //       System.err.println ("Suppressee: " + suppressee.repr());
  //       System.err.println ("Factory: " + factory);
  //       System.err.println ("Linked: " + linked);
  //       System.err.println ("Unlinked: " + unlinked);
  //       throw new Error("Must have non-null suppressors");
  //     }
  //     if (this.suppressors.size() < 1) {
  //       throw new Error("Must have more than zero suppressors");
  //     }
  //     return
  //       suppressee.hashCode() ^
  //       suppressors.hashCode() ^
  //       factory.hashCode();
  //   }


  /**
   * Debug output only.
   **/
  public String toString() {
    StringBuffer suppressorsString = new StringBuffer();
    for (int i=0; i<suppressors.length; i++) {
      Invariant inv = suppressors[i];
      suppressorsString.append (inv.format() + " @" + inv.ppt.name());
      suppressorsString.append (", ");
    }

    return ("SuppressionLink:" +
            // " fact:" + factory +
            " inv: " + suppressee.repr() +
            " suppressors: (" + suppressorsString.toString() + ")");
  }

  ////////////////
  // Constructors

  /**
   * Create a new SuppressionLink.  Requires that there be at least
   * one suppressor, and that the invariant doesn't suppress itself.
   * Postcondition: state == CREATED;
   **/
  public SuppressionLink (// SuppressionFactory argFactory,
                          Invariant argSuppressee,
                          Invariant[] argSuppressors) {
    // this.factory = argFactory;
    this.suppressee = argSuppressee;
    int num_suppressors = argSuppressors.length;
    this.suppressors = new Invariant[num_suppressors];
    System.arraycopy(argSuppressors, 0, this.suppressors, 0, num_suppressors);
    this.state = CREATED;
    Assert.assertTrue (num_suppressors > 0);
    Assert.assertTrue (utilMDE.ArraysMDE.indexOf(suppressors, suppressee) == -1); // Expensive
  }

  /**
   * Return a new suppressionlink that depends on all the parts of the
   * given Suppressionlinks.
   **/
  public static SuppressionLink combine(SuppressionLink l1, SuppressionLink l2) {
    Assert.assertTrue(l1.state == CREATED);
    Assert.assertTrue(l2.state == CREATED);
    Assert.assertTrue(l1.suppressee == l2.suppressee);
    int len1 = l1.suppressors.length;
    int len2 = l2.suppressors.length;
    Invariant[] suppressors = new Invariant[len1 + len2];
    System.arraycopy(l1.suppressors, 0, suppressors, 0, len1);
    System.arraycopy(l2.suppressors, 0, suppressors, len1, len2);
    return new SuppressionLink(l1.suppressee, suppressors);
  }


  ///////////////
  // Activators

  /**
   * Link suppressors and suppresses to this.
   * <br>Requires: this is not yet linked, and invariants to be linked not yet
   * linked.
   * <br>Effects: state == LINKED;
   **/
  public void link() {
    Assert.assertTrue (suppressee.getSuppressor() == null,
                       "Suppressee must have null suppressor");
    Assert.assertTrue (state == CREATED);
    suppressee.setSuppressor (this);
    for (int i=0; i<suppressors.length; i++) {
      Invariant suppressor = suppressors[i];
      suppressor.addSuppressee (this);
    }
    state = LINKED;
  }

  /**
   * Unlink suppressors and suppresses to this.
   * Precondition: state == LINKED and Invariants this links to point to this.
   * Postcondition: state == UNLINKED
   **/
  public void unlink() {
    Assert.assertTrue (suppressee.getSuppressor() == this,
                       "Suppressee must be linked to this");
    Assert.assertTrue (state == LINKED);
    suppressee.setSuppressor (null);
    for (int i=0; i<suppressors.length; i++) {
      Invariant suppressor = suppressors[i];
      suppressor.removeSuppressee (this);
    }
    state = UNLINKED;
  }


  // Shouldn't be necessary, but uncomment if serialization is
  // misbehaving.

  //   private void writeObject(java.io.ObjectOutputStream out)
  //     throws IOException {
  //     Assert.assertTrue (linked && !unlinked);
  //     Assert.assertTrue (suppressors != null);
  //     Assert.assertTrue (suppressee.getSuppressor() == this);
  //     out.defaultWriteObject ();
  //     //     if (true) throw new Error();
  //   }

  //   private void readObject(java.io.ObjectInputStream in)
  //     throws IOException, ClassNotFoundException {
  //     in.defaultReadObject();
  //     Assert.assertTrue (linked && !unlinked);
  //     Assert.assertTrue (suppressors != null);
  //     // Assert.assertTrue (suppressee.getSuppressor() == this);
  //   }


  /**
   * Check the rep invariants of this.
   **/
  public void repCheck() {
    // if (this.factory == null) {
    //   throw new Error("Must have non-null factory");
    // }
    if (this.suppressors == null) {
      throw new Error("Must have non-null suppressors");
    }
    if (this.suppressors.length == 0) {
      throw new Error("Must have at least one suppressor");
    }
    if (this.suppressee == null) {
      throw new Error("Must have non-null suppressee");
    }
    // This must be linked when part of PptMap's hierarchy
    Assert.assertTrue (state == LINKED);
  }
}

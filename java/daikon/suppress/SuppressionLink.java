package daikon.suppress;

import daikon.*;
import daikon.inv.Invariant;

import java.util.*;
import java.io.Serializable;
import java.io.ObjectStreamException;
import java.io.InvalidObjectException;
import java.io.IOException;

import java.util.logging.Logger;
import java.util.logging.Level;

import utilMDE.Assert;
import utilMDE.MathMDE;

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
  private final List suppressors;

  /**
   * Suppressed invariant.  Never null.
   **/
  private final Invariant suppressee;


  /**
   * Original factory that generated this.  Never null.
   **/
  private final SuppressionFactory factory;


  /**
   * State constant: this was just created and suppressor and
   * suppressees do not know about this.
   **/
  private static final int CREATED = 0;

  /**
   * State constant: created and suppressor and suppressees know about
   * this.
   **/
  private static final int LINKED = 1;

  /**
   * State constant: this has been destroyed and suppressor and
   * suppresses no longer know about this.  This should not be used
   * again.
   **/
  private static final int UNLINKED = 2;

  /**
   * What stage of use this link is in.  Gets modified.  Can be one of
   * CREATED, LINKED or UNLINKED.
   **/
  private int state = CREATED;



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

  /**
   * Get the original factory that generated this.  Tells what type of
   * suppression this does.
   * @return never null.
   **/
  public SuppressionFactory getFactory() {
    return factory;
  }


  /**
   * Get the (unmodifiable) list of invariants that suppress this
   * @return never null.
   **/
  public List getSuppressors() {
    return Collections.unmodifiableList (suppressors);
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
    for (Iterator i = suppressors.iterator(); i.hasNext(); ) {
      Invariant inv = (Invariant) i.next();
      suppressorsString.append (inv.repr() + " @" + inv.ppt.ppt_name);
      suppressorsString.append (", ");
    }

    return ("SuppressionLink: fact:" + factory +
            " inv: " + suppressee.repr() +
            " suppressors: (" + suppressorsString.toString() + ")");
  }

  ////////////////
  // Constructors

  /**
   * Create a new SuppressionLink.  Requires that there be at least
   * one suppressor, and that the invariant doesn't suppress itself.
   * @post state == CREATED;
   **/
  public SuppressionLink (SuppressionFactory argFactory,
                          Invariant argSuppressee,
                          List argSuppressors) {
    this.factory = argFactory;
    this.suppressee = argSuppressee;
    this.suppressors = argSuppressors;
    this.state = CREATED;
    Assert.assertTrue (suppressors.size() > 0);
    Assert.assertTrue (!suppressors.contains(suppressee)); // Expensive
  }

  ///////////////
  // Activators

  /**
   * Link suppressors and suppresses to this.
   * @pre this is not yet linked and Invariants to be linked not yet
   * linked.
   * @post state == LINKED;
   **/
  public void link() {
    Assert.assertTrue (suppressee.getSuppressor() == null,
                       "Suppressee must have null suppressor");
    Assert.assertTrue (state == CREATED);
    suppressee.setSuppressor (this);
    for (Iterator i = suppressors.iterator(); i.hasNext(); ) {
      Invariant suppressor = (Invariant) i.next();
      suppressor.addSuppressee (this);
    }
    state = LINKED;
  }

  /**
   * Unlink suppressors and suppresses to this.
   * @pre state == LINKED and Invariants this links to point to this.
   * @post state == UNLINKED
   **/
  public void unlink() {
    Assert.assertTrue (suppressee.getSuppressor() == this,
                       "Suppressee must be linked to this");
    Assert.assertTrue (state == LINKED);
    suppressee.setSuppressor (null);
    for (Iterator i = suppressors.iterator(); i.hasNext(); ) {
      Invariant suppressor = (Invariant) i.next();
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
    if (this.factory == null) {
      throw new Error("Must have non-null factory");
    }
    if (this.suppressors == null) {
      throw new Error("Must have non-null suppressors");
    }
    if (this.suppressors.size() < 1) {
      throw new Error("Must have at least one suppressor");
    }
    if (this.suppressee == null) {
      throw new Error("Must have non-null suppressees");
    }
    // This must be linked when part of PptMap's hierarchy
    Assert.assertTrue (state == LINKED);
  }
}

package daikon.suppress;

import daikon.*;
import daikon.inv.Invariant;

import java.util.*;
import java.io.Serializable;
import java.io.ObjectStreamException;
import java.io.InvalidObjectException;
import java.io.IOException;

import org.apache.log4j.Category;

import utilMDE.Assert;
import utilMDE.MathMDE;

/**
 * Connects Invariants for suppresion; immutable.  This connects to
 * <i>one</i> suppressed invariant and one or many invariants that are
 * suppressing it.  When any of the suppressors are falsified, this
 * becomes destroyed and the suppressed invariant is rechecked.  There
 * is a caveat: a suppressor must be destroyed <i>with respect to</i>
 * the suppressee - that is, if a suppressor is falsified and falls to
 * a program point below the suppressee.  For full documentation, see
 * this package's doc.
 *
 *
 **/

public class SuppressionLink implements Serializable {

  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020722L;

  /**
   * General debug tracer.
   **/
  public static final Category debug = Category.getInstance ("daikon.suppress.SuppresionLink");

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
   * Whether this has been linked.  Gets modified.
   **/
  private boolean linked = false;

  /**
   * Whether this has been unlinked.  Gets modified.  When true, this
   * can never be used for linking again.
   **/
  private boolean unlinked = false;


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
  // satisfied.

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
      suppressorsString.append (inv.repr());
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
   **/
  public SuppressionLink (SuppressionFactory argFactory,
			  Invariant argSuppressee,
			  List argSuppressors) {
    this.factory = argFactory;
    this.suppressee = argSuppressee;
    this.suppressors = argSuppressors;
    Assert.assertTrue (suppressors.size() > 0);
    Assert.assertTrue (!suppressors.contains(suppressee)); // Expensive
  }

  private SuppressionLink() {
    if (true) throw new Error();
    factory = null;
    suppressee = null;
    suppressors = new LinkedList();
  }

  ///////////////
  // Activators

  /**
   * Link this to suppressors and suppresses.
   * @pre this is not yet linked and Invariants to be linked not yet
   * linked.
   **/
  public void link() {
    Assert.assertTrue (suppressee.getSuppressor() == null,
                       "Suppressee must have null suppressor");
    Assert.assertTrue (!this.linked);
    Assert.assertTrue (!this.unlinked);
    suppressee.setSuppressor (this);
    for (Iterator i = suppressors.iterator(); i.hasNext(); ) {
      Invariant suppressor = (Invariant) i.next();
      suppressor.addSuppressee (this);
    }
    this.linked = true;
  }

  /**
   * Unlink this to suppressors and suppresses.
   * @pre this is linked and Invariants this links to point to this.
   **/
  public void unlink() {
    Assert.assertTrue (suppressee.getSuppressor() == this,
                       "Suppressee must be linked to this");
    Assert.assertTrue (this.linked);
    suppressee.setSuppressor (null);
    for (Iterator i = suppressors.iterator(); i.hasNext(); ) {
      Invariant suppressor = (Invariant) i.next();
      suppressor.removeSuppressee (this);
    }
    this.linked = false;
    this.unlinked = true;
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
    Assert.assertTrue (linked && !unlinked);
  }
}

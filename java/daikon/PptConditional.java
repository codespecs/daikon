package daikon;

import daikon.inv.DummyInvariant;
import daikon.split.Splitter;
import org.checkerframework.checker.initialization.qual.Initialized;
import org.checkerframework.checker.nullness.qual.Nullable;

// Information about a disjunctive program point that represents just part
// of the data.
// This doesn't do any direct computation, instead deferring that to its
// views that are slices.

// This perhaps shouldn't extend PptTopLevel; fix that in the future.
// For now, it's convenient to take advantage of its functionality.
// And they're so similar that maybe this is the right thing after all.
public final class PptConditional extends PptTopLevel {
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20041216L;

  public PptTopLevel parent;
  public transient Splitter splitter;
  // indicates whether we're on the true side or the false side of the Splitter
  public boolean splitter_inverse;

  // This does not install the variable values.  The reason is that it's
  // more efficient to do that for two PptConditional objects at once.

  public PptConditional(PptTopLevel parent, Splitter splitter, boolean splitter_inverse) {

    super(ctor_name_helper(parent, splitter, splitter_inverse), ctor_vis_helper(parent));
    // assert splitter.instantiated() == false;
    this.parent = parent;
    @SuppressWarnings({"nullness"}) // won't be used until it's fully initialized
    @Initialized PptConditional thisNonRaw = this;
    this.splitter = splitter.instantiateSplitter(thisNonRaw);
    this.splitter_inverse = splitter_inverse;
    // assert splitter.instantiated() == false;
    // assert this.splitter.instantiated() == true;
    // jhp this.invflow_ppts = new PptTopLevel[0];
    // jhp this.invflow_transforms = new int[0][];
  }

  private static String ctor_name_helper(
      PptTopLevel parent, Splitter splitter, boolean splitter_inverse) {
    if (splitter_inverse) {
      return parent.name + ";condition=\"not(" + splitter.condition() + ")\"";
    } else {
      return parent.name + ";condition=\"" + splitter.condition() + "\"";
    }
  }

  private static VarInfo[] ctor_vis_helper(PptTopLevel parent) {
    return (VarInfo.arrayclone_simple(parent.var_infos));
  }

  // This is tested after constructing a PptConditional but before
  // installing it on any lists.  It should perhaps be checked earlier, but
  // it's convenient (for the Splitter writer) to do so after instantiating.
  public boolean splitter_valid() {
    return splitter.valid();
  }

  public @Nullable DummyInvariant dummyInvariant() {
    return splitter.getDummyInvariant();
  }
}

package daikon;

import daikon.inv.*;

import utilMDE.*;

import java.util.*;


// This is a fake PptSlice for use with Implication invariants.

// - The implication invariants at a program point are grouped into a
// single PptSlice0 with no variables

// - In order to output pre-state invariants as if they were
// post-state, or OBJECT invariants as if they applied to a particular
// parameter, we construct a PptSlice0 whose VarInfos have had their
// names tweaked, and temporarily use that as the invariant's ppt.

public class PptSlice0
  extends PptSlice
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  PptSlice0(PptTopLevel parent) {
     super(parent, new VarInfo[0]);
  }

  public final int arity() {
    return 0;
  }

  // Make a fake slice whose variables are the same as the ones in
  // sliceTemplate, but marked as prestate (i.e., orig(x) rather than x).
  public static PptSlice makeFakePrestate(PptSlice sliceTemplate) {
    PptSlice0 fake = new PptSlice0(sliceTemplate.parent);
    fake.var_infos = new VarInfo[sliceTemplate.var_infos.length];
    for (int i=0; i < fake.var_infos.length; i++) {
      fake.var_infos[i] = VarInfo.origVarInfo(sliceTemplate.var_infos[i]);
    }
    return fake;
  }

  // Make a fake slice whose variables are the same as the ones in
  // sliceTemplate, but with "this" replaced by newName.
  public static PptSlice makeFakeReplaceThis(PptSlice template,
                                             VarInfoName newName) {
    PptSlice0 fake = new PptSlice0(template.parent);
    fake.var_infos = new VarInfo[template.var_infos.length];
    for (int i=0; i < fake.var_infos.length; i++) {
      VarInfo svi = template.var_infos[i];
      fake.var_infos[i] =
        new VarInfo(svi.name.replace(VarInfoName.THIS, newName),
                    svi.type, svi.file_rep_type,
                    svi.comparability.makeAlias(svi.name), svi.aux);
    }
    return fake;
  }

  // We trade space for time by keeping a hash table of all the
  // implications (they're also stored as a vector in invs) so we can
  // efficiently avoid adding implications more than once.
  // - I had to make this transient because when it wasn't, the hash
  // set tried to get the hash codes of all the invariants when it
  // read them in, but their format methods croaked when they couldn't
  // get their varInfos -smcc

  // Really a HashSet<ImplicationByFormatWrapper>.
  // This should not be transient:  more implications can be created during
  // printing, for instance due to guarding.
  private /* [INCR] transient */ HashSet invariantsSeen = new HashSet();

  void init_po() {
    throw new Error("Shouldn't get called");
  }

  public void addInvariant(Invariant inv) {
    Assert.assertTrue(inv != null);
    // The assertion on the next line used to be commented out; why? -smcc
    Assert.assertTrue(inv instanceof Implication);
    invs.add(inv);
    invariantsSeen.add(new ImplicationByFormatWrapper((Implication)inv));
  }

  public boolean hasImplication(Implication imp) {
    return invariantsSeen.contains(new ImplicationByFormatWrapper(imp));
  }

  // We'd like to use a more sophisticated equality check and hashCode
  // for implications when they appear in the invariantsSeen HashSet,
  // but not anywhere else, so we make wrapper objects with the
  // desired methods to go directly in the set.
  private static final class ImplicationByFormatWrapper {
    static final long serialVersionUID = 20021113L;

    private Implication theImp;

    public ImplicationByFormatWrapper(Implication theImp) {
      this.theImp = theImp;
    }

    public boolean equals(Object o) {
      if (o == null || !(o instanceof ImplicationByFormatWrapper))
        return false;
      ImplicationByFormatWrapper other = (ImplicationByFormatWrapper)o;
      // It seems like a bit of a hack to use format() this way, but the
      // check this is replacing (used to be in makeImplication())
      // compared two invariants by their format() values, so I'm
      // assuming there's some good reason -SMcC
      return theImp.format().equals(other.theImp.format());
    }

    public int hashCode() {
      return theImp.format().hashCode();
    }
  }

  // I need to figure out how to set these.
  public int num_samples() { return 2222; }
  public int num_mod_samples() { return 2222; }
  public int num_values() { return 2222; }

  void instantiate_invariants() {
    throw new Error("Shouldn't get called");
  }

  public List add(ValueTuple vt, int count) {
    throw new Error("Shouldn't get called");
  }

}

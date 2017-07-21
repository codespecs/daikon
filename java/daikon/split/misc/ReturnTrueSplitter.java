package daikon.split.misc;

import daikon.*;
import daikon.inv.DummyInvariant;
import daikon.split.*;

/*>>>
import org.checkerframework.checker.nullness.qual.*;
*/

// This splitter tests the condition "return == true".
public final class ReturnTrueSplitter extends Splitter {
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  private /*@Nullable*/ VarInfo return_varinfo;

  public ReturnTrueSplitter() {}

  public ReturnTrueSplitter(Ppt ppt) {
    return_varinfo = ppt.find_var_by_name("return");
    instantiated = true;
  }

  @Override
  public Splitter instantiate(Ppt ppt) {
    return new ReturnTrueSplitter(ppt);
  }

  /*@EnsuresNonNullIf(result=true, expression="return_varinfo")*/
  @Override
  public boolean valid() {
    return ((return_varinfo != null) && (return_varinfo.type == ProglangType.BOOLEAN));
  }

  @SuppressWarnings(
      "nullness:contracts.precondition.override.invalid") // application invariant about private variable
  /*@RequiresNonNull("return_varinfo")*/
  @Override
  public boolean test(ValueTuple vt) {
    return (return_varinfo.getIntValue(vt) != 0);
  }

  @Override
  public String condition() {
    return "return == true";
  }

  @Override
  public /*@Nullable*/ DummyInvariant getDummyInvariant() {
    return null;
  }
}

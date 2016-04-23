package daikon.inv.unary.sequence;

import daikon.*;
import daikon.derive.binary.SequenceSubsequence;
import daikon.inv.*;
import daikon.inv.binary.twoSequence.*;
import daikon.inv.unary.UnaryInvariant;
import java.util.logging.Level;
import java.util.logging.Logger;

/*>>>
import org.checkerframework.checker.initialization.qual.*;
import org.checkerframework.checker.nullness.qual.*;
import typequals.*;
*/

/**
 * Invariants on a single sequence.
 **/
public abstract class SingleSequence extends UnaryInvariant {
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20031024L;

  /**
   * Boolean.  Set to true to disable all SeqIndex invariants
   * (SeqIndexIntEqual, SeqIndexFloatLessThan, etc).  This overrides the
   * settings of the individual SeqIndex enable configuration options.
   * To disable only some options, the options must be disabled
   * individually.
   */
  public static boolean dkconfig_SeqIndexDisableAll = false;

  protected SingleSequence(PptSlice ppt) {
    super(ppt);
  }

  protected /*@Prototype*/ SingleSequence() {
    super();
  }

  public VarInfo var(
      /*>>>@UnknownInitialization(SingleSequence.class) @Raw(SingleSequence.class) SingleSequence this*/ ) {
    return ppt.var_infos[0];
  }
}

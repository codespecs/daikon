package daikon;

import java.io.Serializable;
import org.checkerframework.checker.nullness.qual.Nullable;

/**
 * Represents a parent of a variable. Includes the name of the parent program point, as well as the
 * relationship id. If the name of the variable at the parent program point is different, the parent
 * variable name is also specified.
 */
public class VarParent implements Serializable {
  private static final long serialVersionUID = 20130425L;

  /** Parent ppt for this variable. */
  public String parent_ppt;

  /** Parent variable (within parent_ppt) (if any) */
  public @Nullable String parent_variable;

  /** Parent ppt relation id. */
  public int parent_relation_id;

  public VarParent(String parent_ppt, int parent_relation_id, @Nullable String parent_variable) {
    this.parent_ppt = parent_ppt;
    this.parent_relation_id = parent_relation_id;
    this.parent_variable = parent_variable;
  }
}

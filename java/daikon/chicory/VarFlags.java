package daikon.chicory;

/**
 * An enumeration of various flags that give information about variables. This enum is also present
 * in Daikon (in the {@code VarInfo} class), but is <i>intentionally</i> not shared in an effort to
 * keep Daikon and Chicory completely independent. If you make a change, then you need to change it
 * in all places.
 *
 * <p>These names are written into decl files; they should be the same across every Daikon front-end
 * (Java or otherwise), so please do not change a name without a very good reason. Adding new names
 * is fine.
 *
 * <p>These are documented in the Daikon Developer Manual, in section "A.3.3 Variable declarations"
 * (but really ought to be documented here too...).
 */
public enum VarFlags {
  IS_PARAM,
  NO_DUPS,
  NOT_ORDERED,
  NO_SIZE,
  NOMOD,
  SYNTHETIC,
  CLASSNAME,
  TO_STRING,
  NON_NULL
}

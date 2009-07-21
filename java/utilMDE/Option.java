// The two files
//   Option.java
//   Options.java
// together comprise the implementation of command-line processing.

package utilMDE;

import java.lang.annotation.*;

/**
 * Indicates that the annotated field is set via command-line option.
 * Takes a single string argument that describes the option.  The string
 * is in the format '[-c] [&lt;type&gt;] description':
 * <ul>
 *   <li>'-c' is an optional single-character short name for the option.
 *   <li>'&lt;type&gt;' is an optional description of the option type more
 *       specific than its Java type (eg, '&lt;filename&gt;' if the variable's
 *       type is String).  The less-than and greater-than symbols are required.
 *   <li>'description' is a short (often one-line) description of the option
 *       suitable for a usage message.
 * </ul>
 * <p>
 *
 * The command-line options are processed by the {@link utilMDE.Options} class.
 * For example usage, see the documentation for {@link utilMDE.Options}.
 * @see utilMDE.Options
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface Option {
  String value();
}

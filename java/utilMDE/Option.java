package utilMDE;

import java.lang.annotation.*;

/**
 * Indicates that the annotated field is set via command line option.
 * Takes a single string argument that describes the option.  The string
 * is in the format '[-c] [<type>] description':
 * <ul>
 *   <li>'-c' is an optional single character short name for the option.
 *   <li>'<type>' is an optional description of the option type more
 *       specific than its Java type (eg, '<filename>' if the variable's
 *       type is String).  The less-than and greater-than symbols are required.
 *   <li>'description' is a description of the option suitable for a
 *       usage message.
 * </ul>
 * <p>
 *
 * The command-line options are processed by the {@link Options} class.
 * @see Options
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface Option {
  String value();
}

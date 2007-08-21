package checkers.quals;

import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import static java.lang.annotation.ElementType.*;

/**
 * Indicates that the annotated type behaves as the most restrictive
 * of ReadOnly and Mutable: only Mutable can be assigned to it, and it
 * can only be assigned to ReadOnly.
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({TYPE})
public @interface QReadOnly {

}

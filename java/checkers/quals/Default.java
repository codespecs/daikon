package checkers.quals;

import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import static java.lang.annotation.ElementType.*;
import java.lang.annotation.Target;

/**
 * Specifies that an annotation should be included on a type without having
 * to provide it explicitly. Default annotations may be restricted to
 * particular locations.
 * 
 * @see DefaultLocation
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({CONSTRUCTOR, METHOD, FIELD, LOCAL_VARIABLE, PARAMETER, TYPE})
public @interface Default {
    
    /** @return the fully qualified name of the annotation to apply */
    String value();
    
    /** @return the locations to which the annotation should be applied */
    DefaultLocation[] types() default {DefaultLocation.ALL};
}

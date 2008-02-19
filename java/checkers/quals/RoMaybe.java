package checkers.quals;

import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import static java.lang.annotation.ElementType.*;

/**
 * Specifies the allowed mutabilities of a method's return value or the arguments,
 * based on the mutability type of the arguments and the receiver at a method
 * invocation. {@code @RoMaybe} has the same behavior as creating two copies
 * of the method signature, one where all of its ocurrences are substituted with
 * {@code @ReadOnly}, and one where all of its occurences are substituted with
 * {@code @Mutable}; that is, if it were possible to have annotation overloading,
 * <pre>
 * &#064;RoMaybe getA() &#064;RoMaybe {return a;}
 * </pre>
 * would be equivalent to
 * <pre>
 * &#064;ReadOnly getA() &#064;ReadOnly {return a;}
 * getA() {return a;}
 * </pre>
 *
 * As a first example, if {@code @RoMaybe} appears in the return type of a
 * method, at the method invocation it will be interpreted as {@code @ReadOnly}
 * if any the arguments passed to parameter annotated with
 * {@code @ReadOnly} is a readonly instance, or if the receiver type
 * is readonly and the method is invocated from a readonly context. That is,
 * <pre>
 *  &#064;RoMaybe aTestMethod(String a,
 *                            &#064;RoMaybe Object b,
 *                            List<&#064;RoMaybe Date> c) &#064;RoMaybe
 * </pre>
 * has a readonly return type if the argument passed as b is readonly,
 * or if the argument passed as c is a list of readonly Dates, or if
 * the aTestMethod is invoked from a readonly receiver. Otherwise, it
 * has a mutable return type.
 *
 * As a second example, if the receiver type of a constructor is
 * annotated with {@code @RoMaybe}, the created instance will be
 * readonly if any of the arguments passed to parameters annotated
 * with {@code @RoMaybe} is readonly, and it will be mutable
 * otherwise. That is,
 * <pre>
 *  public Something(String a,
 *                   &#064;RoMaybe Object b,
 *                   List<&#064;RoMaybe Date> c) &#064;RoMaybe
 * </pre>
 * instanciates a readonly Something if a readonly argument is passed
 * as b, or if the argument passed as c is a list of readonly
 * Dates. Otherwise, it instanciates a mutable Something.
 *
 * As a third example, if the return type of a method is not annotated
 * anywhere with {@code @RoMaybe}, but its receiver type and some of
 * its parameters are, then, at a mutable instance, only mutable
 * arguments are accepted; at a readonly instance, both types of
 * arguments are accepted. That is,
 * <pre>
 *  aTestMethod(String a,
 *              &#064;RoMaybe Object b,
 *              List<&#064;RoMaybe Date> c) &#064;RoMaybe
 * </pre>
 * when invoked from a mutable reference will only accept mutable
 * arguments as b, and lists of mutable Dates as c. When aTestMethod
 * is invoked from a readonly reference, it will accept readonly
 * arguments as b, and lists of readonly arguments as c.
 *
 * Since the code must be legal at both "overloaded" cases, parameters
 * annotated with {@code @RoMaybe} suffer the same restrictions inside
 * the method body as parameters annotated with {@code @ReadOnly}, and
 * methods with receiver type annotated as {@code @RoMaybe} suffer the
 * same restrictions as methods with receiver type annotated as {@code @ReadOnly}.
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({METHOD, PARAMETER, LOCAL_VARIABLE, TYPE})
public @interface RoMaybe {
    String[] value() default {};
}

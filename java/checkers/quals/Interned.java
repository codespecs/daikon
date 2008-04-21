package checkers.quals;

import static java.lang.annotation.ElementType.*;

import java.lang.annotation.*;

import checkers.interned.InternedChecker;
import checkers.metaquals.*;
import checkers.types.AnnotatedTypeMirror.AnnotatedPrimitiveType;

import com.sun.source.tree.LiteralTree;

/**
 * Indicates that a variable has been interned, i.e., that the variable refers
 * to the canonical representation of an object.
 * 
 * <p>
 * 
 * This annotation is associated with the {@link InternedChecker}.
 * 
 * @see InternedChecker
 * @manual #interned Interned Checker
 */
@Documented
@TypeQualifier
@Retention(RetentionPolicy.RUNTIME)
@Target({FIELD, LOCAL_VARIABLE, METHOD, PARAMETER, TYPE})
@ImplicitFor(
    treeClasses={LiteralTree.class},
    typeClasses={AnnotatedPrimitiveType.class})
public @interface Interned {
    
}

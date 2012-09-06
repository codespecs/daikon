package typequals;

import java.lang.annotation.*;
import checkers.quals.*;

/** An index into a ValueTuple or list of values. */
@Target({ElementType.TYPE_USE, ElementType.TYPE_PARAMETER})
@TypeQualifier
@SubtypeOf(VIndexTop.class)
public @interface ValueIndex {}

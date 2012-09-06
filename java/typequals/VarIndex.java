package typequals;

import java.lang.annotation.*;
import checkers.quals.*;

/** An index into a list of VarInfo objects. */
@Target({ElementType.TYPE_USE, ElementType.TYPE_PARAMETER})
@TypeQualifier
@SubtypeOf(VIndexTop.class)
public @interface VarIndex {}

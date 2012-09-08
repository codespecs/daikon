package typequals;

import java.lang.annotation.*;
import checkers.quals.*;

import com.sun.source.tree.Tree;

@Target({ElementType.TYPE_USE, ElementType.TYPE_PARAMETER})
@TypeQualifier
@SubtypeOf({ValueIndex.class, VarIndex.class, VIndexUnqualified.class})
@ImplicitFor(trees={Tree.Kind.NULL_LITERAL, Tree.Kind.INT_LITERAL})
public @interface VIndexBottom {}

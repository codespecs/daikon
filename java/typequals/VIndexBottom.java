package typequals;

import java.lang.annotation.ElementType;
import java.lang.annotation.Target;

import org.checkerframework.framework.qual.ImplicitFor;
import org.checkerframework.framework.qual.SubtypeOf;
import org.checkerframework.framework.qual.TypeQualifier;

import com.sun.source.tree.Tree;

@Target({ElementType.TYPE_USE, ElementType.TYPE_PARAMETER})
@TypeQualifier
@SubtypeOf({ValueIndex.class, VarIndex.class, VIndexUnqualified.class})
@ImplicitFor(trees={Tree.Kind.NULL_LITERAL, Tree.Kind.INT_LITERAL})
public @interface VIndexBottom {}

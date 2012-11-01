package checkers.regex;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import javax.lang.model.element.AnnotationMirror;
import javax.lang.model.element.ExecutableElement;

import javacutils.InternalUtils;
import javacutils.TypesUtils;

import checkers.basetype.BaseTypeChecker;
import checkers.types.AnnotatedTypeMirror;
import checkers.types.BasicAnnotatedTypeFactory;
import checkers.types.TreeAnnotator;
import checkers.util.*;

import com.sun.source.tree.BinaryTree;
import com.sun.source.tree.CompilationUnitTree;
import com.sun.source.tree.CompoundAssignmentTree;
import com.sun.source.tree.ExpressionTree;
import com.sun.source.tree.LiteralTree;
import com.sun.source.tree.MethodInvocationTree;
import com.sun.source.tree.Tree;
import com.sun.source.tree.Tree.Kind;

/**
 * Adds a type qualifier from the VIndex type system to the type of tree,
 * in the following cases:
 * [TODO]
 */
public class VIndexAnnotatedTypeFactory extends BasicAnnotatedTypeFactory<VIndexChecker> {

    public VIndexAnnotatedTypeFactory(VIndexChecker checker,
            CompilationUnitTree root) {
        super(checker, root);

        this.postInit();
    }

    @Override
    public TreeAnnotator createTreeAnnotator(VIndexChecker checker) {
        return new VIndexTreeAnnotator(checker);
    }

    private class VIndexTreeAnnotator extends TreeAnnotator {

        public VIndexTreeAnnotator(BaseTypeChecker checker) {
            super(checker, VIndexAnnotatedTypeFactory.this);
        }

        /**
         * Case 2: concatenation of VIndex or PolyVIndex String/char literals.
         * Also handles concatenation of partial regular expressions.
         */
        @Override
        public Void visitBinary(BinaryTree tree, AnnotatedTypeMirror type) {
            if (!type.isAnnotated()
                && tree.getKind() == Tree.Kind.PLUS
                && TypesUtils.isDeclaredOfName(InternalUtils.typeOf(tree), "int")) {
                AnnotatedTypeMirror lExpr = getAnnotatedType(tree.getLeftOperand());
                AnnotatedTypeMirror rExpr = getAnnotatedType(tree.getRightOperand());
                Set<AnnotationMirror> lubs = qualHierarchy.leastUpperBounds(lExpr.getAnnotations(), rExpr.getAnnotations());
                type.replaceAnnotations(lubs);
            }
            return null; // super.visitBinary(tree, type);
        }

    }
}

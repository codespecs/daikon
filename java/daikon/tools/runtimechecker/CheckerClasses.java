package daikon.tools.runtimechecker;

import daikon.tools.jtb.Ast;
import java.util.ArrayList;
import java.util.List;
import jtb.syntaxtree.*;
import org.checkerframework.checker.nullness.qual.NonNull;

/** Represents a set of classes created by the instrumenter to check invariants. */
public class CheckerClasses {

  protected List<CheckerClass> classes = new ArrayList<>();

  public void addCheckerClass(ClassOrInterfaceBody clazz) {
    classes.add(new CheckerClass(clazz));
  }

  public List<CheckerClass> getCheckerClasses() {
    return classes;
  }

  @SuppressWarnings("interning")
  public void addDeclaration(ClassOrInterfaceBody clazz, StringBuilder decl) {

    for (CheckerClass c : classes) {
      if (c.fclassbody == clazz) {
        c.addDeclaration(decl);
      }
    }
  }

  public void addDeclaration(ConstructorDeclaration clazz, StringBuilder decl) {

    @SuppressWarnings(
        "nullness") // application invariant: a constructor is always in a class or interface
    @NonNull ClassOrInterfaceBody body =
        (ClassOrInterfaceBody) Ast.getParent(ClassOrInterfaceBody.class, clazz);
    addDeclaration(body, decl);
  }

  public void addDeclaration(MethodDeclaration clazz, StringBuilder decl) {

    @SuppressWarnings(
        "nullness") // application invariant: a method is always in a class or interface
    @NonNull ClassOrInterfaceBody body =
        (ClassOrInterfaceBody) Ast.getParent(ClassOrInterfaceBody.class, clazz);
    addDeclaration(body, decl);
  }
}

package daikon.tools.runtimechecker;

import daikon.tools.jtb.*;
import java.util.*;
import java.util.ArrayList;
import java.util.List;
import jtb.syntaxtree.*;

/*>>>
import org.checkerframework.checker.nullness.qual.*;
*/

/** Represents a set of classes created by the instrumenter to check invariants. */
public class CheckerClasses {

  protected List<CheckerClass> classes = new ArrayList<CheckerClass>();

  public void addCheckerClass(ClassOrInterfaceBody clazz) {
    classes.add(new CheckerClass(clazz));
  }

  public List<CheckerClass> getCheckerClasses() {
    return classes;
  }

  @SuppressWarnings("interned")
  public void addDeclaration(ClassOrInterfaceBody clazz, StringBuffer decl) {

    for (CheckerClass c : classes) {
      if (c.fclassbody == clazz) {
        c.addDeclaration(decl);
      }
    }
  }

  public void addDeclaration(ConstructorDeclaration clazz, StringBuffer decl) {

    @SuppressWarnings(
        "nullness") // application invariant: a constructor is always in a class or interface
    /*@NonNull*/ ClassOrInterfaceBody body =
        (ClassOrInterfaceBody) Ast.getParent(ClassOrInterfaceBody.class, clazz);
    addDeclaration(body, decl);
  }

  public void addDeclaration(MethodDeclaration clazz, StringBuffer decl) {

    @SuppressWarnings(
        "nullness") // application invariant: a method is always in a class or interface
    /*@NonNull*/ ClassOrInterfaceBody body =
        (ClassOrInterfaceBody) Ast.getParent(ClassOrInterfaceBody.class, clazz);
    addDeclaration(body, decl);
  }
}

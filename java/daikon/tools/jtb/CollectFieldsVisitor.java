package daikon.tools.jtb;

import java.util.*;
import syntaxtree.*;
import visitor.*;
import daikon.*;

// Method "fieldDeclarations" returns a list of all FieldDeclarations
// declared in this class or in nested/inner classes.

class CollectFieldsVisitor extends DepthFirstVisitor {

  private Vector fieldDecls = new Vector();
  private int cachedSize = -1;
  private FieldDeclaration[] fieldDeclsArray;
  private String[] ownedNamesArray;
  private String[] finalNamesArray;

  private void updateCache() {
    if (cachedSize != fieldDecls.size()) {
      fieldDeclsArray = (FieldDeclaration[]) fieldDecls.toArray(new FieldDeclaration[0]);
      Vector ownedNames = new Vector();
      Vector finalNames = new Vector();
      for (int i=0; i<fieldDeclsArray.length; i++) {
        FieldDeclaration fd = fieldDeclsArray[i];
        boolean isFinal = hasModifier(fd, "final");
        Type fdtype = fd.f1;
        boolean isOwned = ! isPrimitive(fdtype);
        {
          String name = name(fd.f2);
          if (isFinal)
            finalNames.add(name);
          if (isOwned)
            ownedNames.add(name);
        }
        NodeListOptional fds = fd.f3;
        if (fds.present()) {
          for (int j=0; j<fds.size(); j++) {
            // System.out.println("" + j + ": " + fds.elementAt(j));
            NodeSequence ns = (NodeSequence) fds.elementAt(j);
            if (ns.size() != 2) {
              System.out.println("Bad length " + ns.size() + " for NodeSequence");
            }
            String name = name((VariableDeclarator) ns.elementAt(1));
            if (isFinal)
              finalNames.add(name);
            if (isOwned)
              ownedNames.add(name);
          }
        }
      }
      ownedNamesArray = (String[]) ownedNames.toArray(new String[0]);
      finalNamesArray = (String[]) finalNames.toArray(new String[0]);
      cachedSize = fieldDecls.size();
    }
  }

  private String name(VariableDeclarator n) {
    return n.f0.f0.tokenImage;
  }

  private boolean hasModifier(FieldDeclaration n, String mod) {
    return Ast.contains(n.f0, mod);
  }

  private boolean isPrimitive(Type n) {
    NodeChoice c = n.f0;
    if (! ((c.choice instanceof PrimitiveType) || (c.choice instanceof Name))) {
      throw new Error("Bad type choice");
    }
    return ((c.choice instanceof PrimitiveType) && ! n.f1.present());
  }


  // Returns a list of all FieldDeclarations declared in this class or in
  // nested/inner classes.
  public FieldDeclaration[] fieldDeclarations() {
    updateCache();
    return fieldDeclsArray;
  }

  // Returns a list of names of all fields with owner annotations.
  public String[] ownedFieldNames() {
    updateCache();
    return ownedNamesArray;
  }

  // Returns a list of all final fields.
  public String[] finalFieldNames() {
    updateCache();
    return finalNamesArray;
  }


  /**
   * f0 -> ( "public" | "protected" | "private" | "static" | "final" | "transient" | "volatile" )*
   * f1 -> Type()
   * f2 -> VariableDeclarator()
   * f3 -> ( "," VariableDeclarator() )*
   * f4 -> ";"
   */
  public void visit(FieldDeclaration n) {
    fieldDecls.add(n);

    n.f0.accept(this);
    n.f1.accept(this);
    n.f2.accept(this);
    n.f3.accept(this);
    n.f4.accept(this);
  }

}

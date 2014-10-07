package jtb.cparser.customvisitor;

import jtb.cparser.syntaxtree.*;
import jtb.cparser.visitor.*;
import java.util.*;

public class Converter extends DepthFirstVisitor {

  public ArrayList<String> actualStrings;
  public ArrayList<String> stringArrays;
  private boolean matrixAccess = false;
  private boolean reorder = false;
  private NestedArrayChecker nestChecker= new NestedArrayChecker();
  private boolean shouldConvert = true;
  private Vector<Node> toBringToFront = new Vector<Node>();
  private int nestedIndents = 0;

  public void visit(PostfixExpression n) {
    if (isInParentheses(n)) {
      handleParenthesesExpression(n);
      return;
    }
    if (isMatrixAccess(n)) {
      handleMatrixAccess(n);
      return;
    }
    if (isArrayAccess(n)) {
      handleArrayAccess(n);
      return;
    }
    if (isFunctionCall(n)) {
      handleFunctionCall(n);
      return;
    }
  }



  public boolean isInParentheses(PostfixExpression n) {
    return (n.f0.f0.choice instanceof NodeSequence);
  }

  public void handleParenthesesExpression(PostfixExpression n) {
    visit((Expression)((NodeSequence)n.f0.f0.choice).nodes.get(1));
  }


  public boolean isMatrixAccess(PostfixExpression n) {
    if (n.f1.nodes.size() == 2) {
      NodeChoice first = (NodeChoice) n.f1.nodes.get(0);
      NodeChoice second = (NodeChoice) n.f1.nodes.get(1);
      if (first.choice instanceof NodeSequence
          && second.choice instanceof NodeSequence) {
        return (isArrayAccess((NodeSequence) first.choice) &&
                isArrayAccess((NodeSequence) second.choice));
      }
    }
    return false;
  }

    public void handleMatrixAccess(PostfixExpression n) {
    nestedIndents++;
    NodeChoice first = (NodeChoice) n.f1.nodes.get(0);
    NodeChoice second = (NodeChoice) n.f1.nodes.get(1);
    NodeSequence seq1 = (NodeSequence) first.choice;
    NodeSequence seq2 = (NodeSequence) second.choice;
    if (containsArrayAccess((Expression)seq1.nodes.get(1)) ||
        containsArrayAccess((Expression)seq2.nodes.get(1))) {
      shouldConvert = false;
      super.visit(n);
      if (nestedIndents == 1) {
        shouldConvert = true;
      }
      convertMatrixExpression(n, seq1.nodes, seq2.nodes);
      if (nestedIndents == 1) {
        seq1.nodes.addAll(0, toBringToFront);
        toBringToFront.clear();
      }
    }
    else if (reorder) {
      convertMatrixExpression(n, seq1.nodes, seq2.nodes);
      seq1.nodes.addAll(0, toBringToFront);
      toBringToFront.clear();
    }
    else {
      convertMatrixExpression(n, seq1.nodes, seq2.nodes);
    }
    nestedIndents--;
  }

  public void  convertMatrixExpression(PostfixExpression n, Vector<Node> nodes1, Vector<Node> nodes2) {
    NodeToken nameToken = (NodeToken) n.f0.f0.choice;
    String nameString = nameToken.tokenImage;
    String lengthString = "length";
    if (isStringArray(nameString)) {
      lengthString = "length()";
      convertToStringAccess(nodes2);
    }
    if (shouldConvert) {
      nameToken.tokenImage = "";
      nodes1.remove(0);
      nodes1.remove(1);
      nodes1.add(1, new NodeToken(">=0 && "));
      nodes1.add(2, nodes1.get(0));
      nodes1.add(3, new NodeToken("<" + nameString + ".length && "));
      nodes1.add(4, nodes2.get(1));
      nodes1.add(5, new NodeToken(">=0 && "));
      nodes1.add(6, nodes2.get(1));
      nodes1.add(7, new NodeToken("<" + nameString + "["));
      nodes1.add(8, nodes1.get(0));
      nodes1.add(9, new NodeToken("]." + lengthString +  " && "));
      nodes1.add(10, new NodeToken(nameString));
      nodes1.add(11, new NodeToken("["));
      nodes1.add(12, nodes1.get(0));
      nodes1.add(13, new NodeToken("]"));
      nodes1.addAll(nodes2);
      nodes2.clear();
    }
    else {
      toBringToFront.add(nodes1.get(1));
      toBringToFront.add(new NodeToken(">=0 && "));
      toBringToFront.add(nodes1.get(1));
      toBringToFront.add(new NodeToken("<" + nameString + ".length && "));
      toBringToFront.add(nodes2.get(1));
      toBringToFront.add(new NodeToken("<" + nameString + "["));
      toBringToFront.add(nodes1.get(1));
      toBringToFront.add(new NodeToken("]." + lengthString +  " && "));
      toBringToFront.add(nodes2.get(1));
      toBringToFront.add(new NodeToken(">= 0 && "));
    }
  }


  private void convertToStringAccess(Vector<Node> v) {
    // remove the [ and ]
    v.remove(0);
    v.remove(1);
    v.add(0, new NodeToken(".charAt("));
    v.add(new NodeToken(")"));
  }


  public boolean isArrayAccess(PostfixExpression n) {
    if (n.f1.nodes.size() > 0) {
      NodeChoice choice = (NodeChoice) n.f1.nodes.get(0);
      if (choice.choice instanceof NodeSequence) {
        NodeSequence seq = (NodeSequence) choice.choice;
        return (isArrayAccess(seq));
      }
    }
    return false;
  }

  private boolean isArrayAccess(NodeSequence seq) {
    return (seq.nodes.size() == 3 && seq.nodes.get(0).toString().equals("[") &&
            seq.nodes.get(2).toString().equals("]"));
  }

  public void handleArrayAccess(PostfixExpression n) {
    nestedIndents++;
    NodeChoice choice = (NodeChoice) n.f1.nodes.get(0);
    if (choice.choice instanceof NodeSequence) {
      NodeSequence seq = (NodeSequence) choice.choice;
      Expression p = (Expression)(seq.nodes.get(1));
      if (containsArrayAccess(p)) {
        shouldConvert = false;
        super.visit(p);
        if (nestedIndents == 1) {
          shouldConvert = true;
        }
        convertArrayExpression(n, seq.nodes, 0);
        if (nestedIndents == 1) {
          seq.nodes.addAll(0, toBringToFront);
          toBringToFront.clear();
        }
      }
      else if (reorder) {
        convertArrayExpression(n, seq.nodes, 0);
        seq.nodes.addAll(0, toBringToFront);
        toBringToFront.clear();
      }
      else {
        convertArrayExpression(n, seq.nodes, 0);
      }
    }
    nestedIndents--;
  }

    public void convertArrayExpression(PostfixExpression n, Vector<Node> nodes, int i) {
    if ((n.f0.f0.choice instanceof NodeToken)) {
      String name = n.f0.f0.choice.toString();
      if (shouldConvert) {
        ((NodeToken)n.f0.f0.choice).tokenImage = "";
        nodes.add(0, nodes.get(1));
        nodes.add(1, new NodeToken("<" + name+ ".length && "));
        nodes.add(2, nodes.get(3));
        nodes.add(3, new NodeToken(">=0 && "));
        nodes.add(4, new NodeToken(name));
        if (isString(name)) {
          NodeToken lengthAppend = (NodeToken) nodes.get(1);
          lengthAppend.tokenImage = ("<" + name+ ".length() && ");
          NodeToken token = (NodeToken) n.f0.f0.choice;
          token.tokenImage =  "";
          nodes.add(5, new NodeToken(".charAt((int)"));
          nodes.remove(6);
          NodeToken close = (NodeToken) nodes.get(7);
          close.tokenImage = ")";
        }
      } else {
        int index = 1;
        if (isString(name)) {
          nodes.add(0, new NodeToken(name));
          NodeToken token = (NodeToken) n.f0.f0.choice;
          token.tokenImage =  "";
          nodes.add(1, new NodeToken(".charAt((int)"));
          nodes.remove(2);
          NodeToken close = (NodeToken) nodes.get(3);
          close.tokenImage = ")";
          index = 2;
        }
        String lengthString = (isString(name))?".length()":".length";

        toBringToFront.add(nodes.get(index));
        toBringToFront.add(new NodeToken("<" + name+ lengthString + " && "));
        toBringToFront.add(nodes.get(index));
        toBringToFront.add(new NodeToken(">=0 && "));
      }
    }
  }



  public boolean isFunctionCall(PostfixExpression n) {
    if (n.f1.nodes.size() > 0) {
      NodeChoice choice = (NodeChoice) n.f1.nodes.get(0);
      if (choice.choice instanceof NodeSequence) {
        NodeSequence seq = (NodeSequence) choice.choice;
        return (isFunctionCall(seq));
      }
    }
    return false;
  }

  public boolean isFunctionCall(NodeSequence seq) {
    return (seq.nodes.size() == 3 && seq.nodes.get(0).toString().equals("(") &&
            seq.nodes.get(2).toString().equals(")"));
  }

  public void handleFunctionCall(PostfixExpression n) {
    NodeToken token = (NodeToken) n.f0.f0.choice;
    NodeChoice choice = (NodeChoice) n.f1.nodes.get(0);
    NodeSequence seq = (NodeSequence) choice.choice;
    if (token.tokenImage.equals("strlen")) {
      NodeOptional opt = (NodeOptional) seq.nodes.get(1);
      ArrayList<Node> l = extractArgumentNames((ArgumentExpressionList)opt.node);
      token.tokenImage =  "";
      seq.nodes.remove(1);
      seq.nodes.add(0, new NodeToken(".length"));
      seq.nodes.add(0, l.get(0));
    }
  }

  public boolean isStringArray(String n) {
    return stringArrays.contains(n.trim());
  }

  public boolean isString(String name) {
    return actualStrings.contains(name.trim());
  }


  private ArrayList<Node> extractArgumentNames(ArgumentExpressionList ael) {
    ArrayList<Node> assigns = new ArrayList<Node>();
    if (ael!=null) {
      assigns.add(ael.f0);
      if (ael.f1.nodes.size() > 0) {
        for (int i = 0; i < ael.f1.nodes.size(); i++) {
          NodeSequence curr = (NodeSequence)ael.f1.nodes.get(i);
          for (int j = 0; j < curr.nodes.size(); j++) {
            if (j%2 == 1) {
              assigns.add(curr.nodes.get(j));
            }
          }
        }
      }
    }
    extractNamesAssignmentExpressions(assigns);
    return assigns;
  }


  private ArrayList<Node> extractNamesAssignmentExpressions(ArrayList<Node> assigns) {
    for (int i = 0; i < assigns.size(); i++) {
      AssignmentExpression curr = (AssignmentExpression)assigns.get(i);
      Node n = ((UnaryExpression)((ConditionalExpression)curr.f0.choice).f0.f0.f0.f0.f0.f0.f0.f0.f0.f0.f0.f0.choice).f0.choice;
      Node temp  = ((PostfixExpression)n).f0.f0.choice;
      assigns.remove(i);
      if ((temp instanceof NodeToken)) {
        assigns.add(temp);
      }

    }
    return assigns;
  }






  public void visit(RelationalExpression n) {
    boolean first = containsArrayAccess(n.f0);
    boolean second = containsArrayAccess(n.f1);
    if (!first && !second) {
      n.f0.accept(this);
      n.f1.accept(this);
    }
    else if (first && !second) {
      n.f0.accept(this);
      n.f1.accept(this);
    }
    else if (!first && second) {
      NodeSequence seq = (NodeSequence) n.f1.node;
      super.visit(n.f1);
      seq.nodes.add(2, n.f0);
      seq.nodes.add(1, seq.nodes.remove(0));
      n.f0 = null;
    }
    else  {
      shouldConvert = false;
      n.f1.accept(this);
      shouldConvert = true;
      reorder = true;
      n.f0.accept(this);
      reorder = false;
    }
  }


  public void visit(EqualityExpression n) {
    boolean first = containsArrayAccess(n.f0);
    boolean second = containsArrayAccess(n.f1);
    if (!first && !second) {
      n.f0.accept(this);
      n.f1.accept(this);
    }
    else if (first && !second) {
      n.f0.accept(this);
      n.f1.accept(this);
    }
    else if (!first && second) {
      NodeSequence seq = (NodeSequence) n.f1.node;
      super.visit(n.f1);
      seq.nodes.add(2, n.f0);
      seq.nodes.add(1, seq.nodes.remove(0));
      n.f0 = null;
    }
    else  {
      shouldConvert = false;
      n.f1.accept(this);
      shouldConvert = true;
      reorder = true;
      n.f0.accept(this);
      reorder = false;

    }
  }

  public void visit(LogicalANDExpression n) {
    // put parentheses around each subexpression
    if (n!=null &&n.f1 != null && n.f1.node instanceof NodeSequence) {
      Vector<Node> nodes = ((NodeSequence) n.f1.node).nodes;
      if (nodes.get(0).toString().equals("&&")) {
        nodes.add(0, new NodeToken("("));
        nodes.add(1, n.f0);
        nodes.add(2, new NodeToken(")"));
        nodes.add(4, new NodeToken("("));
        nodes.add(new NodeToken(")"));
        n.f0 = null;
        n.f1.accept(this);
        return;
      }
    }
    n.f0.accept(this);
    n.f1.accept(this);
  }

  public void visit(LogicalORExpression n) {
    // put parentheses around each subexpression
    if (n != null && n.f1.node instanceof NodeSequence) {
      Vector<Node> nodes = ((NodeSequence) n.f1.node).nodes;
      if (nodes.get(0).toString().equals("||")) {
        nodes.add(0, new NodeToken("("));
        nodes.add(1, n.f0);
        nodes.add(2, new NodeToken(")"));
        nodes.add(4, new NodeToken("("));
        nodes.add(new NodeToken(")"));
        n.f0 = null;
        n.f1.accept(this);
        return;
      }
    }
    n.f0.accept(this);
    n.f1.accept(this);
  }

  public boolean containsArrayAccess(Expression n) {
    return nestChecker.containsArrayAccess(n);
  }

  public boolean containsArrayAccess(ShiftExpression n) {
    return nestChecker.containsArrayAccess(n);
  }

  public boolean containsArrayAccess(RelationalExpression n) {
    return nestChecker.containsArrayAccess(n);
  }

  public boolean containsArrayAccess(NodeOptional n) {
    return nestChecker.containsArrayAccess(n);
  }

  class NestedArrayChecker extends DepthFirstVisitor {

    private boolean isNested = false;
    public boolean containsArrayAccess(Expression n) {
      isNested = false;
      super.visit(n);
      return isNested;
    }

    public boolean containsArrayAccess(RelationalExpression n) {
      isNested = false;
      super.visit(n);
      return isNested;
    }

    public boolean containsArrayAccess(NodeOptional n) {
      isNested = false;
      super.visit(n);
      return isNested;
    }

    public boolean containsArrayAccess(ShiftExpression n) {
      isNested = false;
      super.visit(n);
      return isNested;
    }

    public void visit(PostfixExpression n) {
      if (isArrayAccess(n) || isMatrixAccess(n)) {
        isNested = true;
      }
    }
  }


}

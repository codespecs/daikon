package jtb.cparser.customvisitor;

import jtb.cparser.syntaxtree.*;
import jtb.cparser.visitor.*;
import java.util.*;

/**
 * Identifies the strings in an C AST
 */

public class StringFinder extends DepthFirstVisitor {

  //Rules used to identify strings:
  //1.  A 'char[]' is always a string
  //2.  A 'string' is always a string
  //3.  A '*char' is a string if:
  //     a.  There is an array access on it
  //     b.  It is an argument to one of the functions in stringFunctions
  //4.  A **char, *char[], or char[][] are always converted to string arrays



  //holds the names of the char pointers, char arrays, and strings
  private ArrayList<String> possibleStrings;
  //holds the possibleStrings that have been verified as strings
  private ArrayList<String> actualStrings;
  //holds the string arrays
  private ArrayList<String> actualStringMatrices;
  //map the function name with the string arrays
  public HashMap<String,ArrayList<String>> stringMatrices = new HashMap<String,ArrayList<String>>();
  //map the function name with the strings
  public HashMap<String,ArrayList<String>> functionStringMapping = new HashMap<String,ArrayList<String>>();
  //holds the names of C functions that indicate a char pointer is a string
  private static HashMap<String,String> stringFunctions = new HashMap<String,String>();

  static {
    stringFunctions.put("strcat", "strcat");
    stringFunctions.put("strchr", "strchr");
    stringFunctions.put("strcmp", "strcmp");
    stringFunctions.put("strcoll", "strcoll");
    stringFunctions.put("strcpy", "strcpy");
    stringFunctions.put("strlen", "strlen");
    stringFunctions.put("strncat", "strncat");
    stringFunctions.put("strncmp", "strncmp");
    stringFunctions.put("strncpy", "strncpy");
    stringFunctions.put("strstr", "strstr");
  }

  public void visit(FunctionDefinition n) {
    possibleStrings = new ArrayList<String>();
    actualStrings = new ArrayList<String>();
    actualStringMatrices = new ArrayList<String>();
    NodeChoice temp = (NodeChoice) n.f1.f1.f1.nodes.get(0);
    NodeSequence params = (NodeSequence) temp.choice;
    //find the strings in the function parameters
    if (params.nodes.get(1) instanceof ParameterTypeList) {
      ParameterTypeList list = (ParameterTypeList) params.nodes.get(1);
      identifyPossibleStrings(list.f0);
    }
    super.visit(n);
    functionStringMapping.put(n.f1.f1.f0.choice.toString(), actualStrings);
    stringMatrices.put(n.f1.f1.f0.choice.toString(), actualStringMatrices);
  }

  private void identifyPossibleStrings(ParameterList l) {
    identifyPossibleString(l.f0);
    for (int i = 0; i < l.f1.nodes.size(); i++) {
      NodeSequence curr = (NodeSequence) l.f1.nodes.get(i);
      identifyPossibleString((ParameterDeclaration)curr.nodes.get(1));
    }
  }

  private void identifyPossibleString(ParameterDeclaration p) {
    NodeSequence seq = (NodeSequence)p.f0.f0.choice;
    if (seq.nodes.get(0) instanceof TypeSpecifier) {
      TypeSpecifier ts = (TypeSpecifier)(seq).nodes.get(0);
      if (p.f1.choice instanceof Declarator) {
        Declarator d = (Declarator)p.f1.choice;
        Node maybePointer = (d.f0).node;
        if (ts.f0.choice.toString().equals("char")) {
          NodeListOptional nodeList = d.f1.f1;
          if (nodeList.nodes.size() == 1 && !( maybePointer instanceof Pointer)) {
            NodeChoice tempChoice = (NodeChoice)nodeList.nodes.get(0);
            NodeSequence tempNodeSeq = (NodeSequence)tempChoice.choice;
            if (tempNodeSeq.size() > 0 &&
                tempNodeSeq.nodes.get(0).toString().equals("[") &&
                tempNodeSeq.nodes.get(2).toString().equals("]")) {
              actualStrings.add(d.f1.f0.choice.toString().trim());
            }
          }
          else if (nodeList.nodes.size() == 0 && maybePointer instanceof Pointer) {
            Pointer temp = (Pointer) maybePointer;
            if (temp.f1.node == null && temp.f2.node == null) {
              possibleStrings.add( d.f1.f0.choice.toString().trim());
            }
            if (temp.f1.node == null && temp.f2.node != null) {
              actualStringMatrices.add(d.f1.f0.choice.toString().trim());
            }
          }
          else if (nodeList.nodes.size() == 2 && !(maybePointer instanceof Pointer)) {
            actualStringMatrices.add(d.f1.f0.choice.toString().trim());
          }
          else if (nodeList.nodes.size() == 1 && maybePointer instanceof Pointer) {
            Pointer temp = (Pointer) maybePointer;
            if (temp.f1.node == null && temp.f2.node == null) {
              actualStringMatrices.add(d.f1.f0.choice.toString().trim());
            }
          }
        }
      }
    }
  }

  public void visit(PostfixExpression n) {
    if (n.f1.nodes.size() > 0) {
      NodeChoice choice = (NodeChoice) n.f1.nodes.get(0);
      if (choice.choice instanceof NodeSequence) {
        NodeSequence seq = (NodeSequence) choice.choice;
        if (seq.nodes.size() == 3 && seq.nodes.get(0).toString().equals("[") &&
            seq.nodes.get(2).toString().equals("]")) {
          if (n.f0.f0.choice instanceof NodeToken) {
            String name = n.f0.f0.choice.toString();
            addString(name.trim());
          }
        }
        else if (seq.nodes.get(0).toString().equals("(")) {
          if (stringFunctions.containsKey(n.f0.f0.choice.toString())) {
            NodeOptional opt = (NodeOptional)seq.nodes.get(1);
            ArgumentExpressionList ael = (ArgumentExpressionList) opt.node;
            Vector<AssignmentExpression> assigns = new Vector<AssignmentExpression>();
            if (ael!=null) {
              assigns.add(ael.f0);
              if (ael.f1.nodes.size() > 0) {
                for (int i = 0; i < ael.f1.nodes.size(); i++) {
                  NodeSequence curr = (NodeSequence)ael.f1.nodes.get(i);
                  for (int j = 0; j < curr.nodes.size(); j++) {
                    if (j%2 == 1) {
                      assigns.add((AssignmentExpression)curr.nodes.get(j));
                    }
                  }
                }
              }
            }
            identifyActualStrings(assigns);
          }
        }
      }
    }
    super.visit(n);
  }



  private void identifyActualStrings(Vector<AssignmentExpression> assigns) {
    for (int i = 0; i < assigns.size(); i++) {
      AssignmentExpression curr = assigns.get(i);
      Node n = ((UnaryExpression)((ConditionalExpression)curr.f0.choice).f0.f0.f0.f0.f0.f0.f0.f0.f0.f0.f0.f0.choice).f0.choice;
      PostfixExpression tempPFExp = (PostfixExpression) n;
      if (tempPFExp.f0.f0.choice instanceof NodeToken) {
        String temp = tempPFExp.f0.f0.choice.toString();
        addString(temp.trim());
      }
    }
  }

  private void addString(String s) {
    if (possibleStrings.contains(s) && !(actualStrings.contains(s))) {
      actualStrings.add(s);
    }
  }
}

/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util.salamis;

import java.util.*;
import ajax.jbc.*;
import java.io.*;
import ajax.jbc.util.*;

public class SalamisParser implements SalamisParserConstants {
    private Hashtable labels;
    private Hashtable vars;
    private String functionName;
    private Hashtable functionTable = new Hashtable();
    private SalamisDefNode curControlNode;
    private ExternalCFGNode rootNode;
    private ExternalCFGDefNode resultNode;
    private ExternalCFGDefNode exceptionNode;
    private Vector nodeList;

    protected void beginFunction(String name, Vector parameters) {
        labels = new Hashtable();
        vars = new Hashtable();
        functionName = name;
        curControlNode = null;
        rootNode = null;
        resultNode = null;
        exceptionNode = null;
        nodeList = new Vector();

        if (parameters != null) {
            for (int i = parameters.size() - 1; i >= 0; i--) {
                addNode(null,
                        new SalamisParameterDefNode(this,
                            (String)parameters.elementAt(i),
                            i));
            }
        }
    }

    protected void endFunction() {
        SalamisFlowgraph fg = new SalamisFlowgraph(rootNode, resultNode, exceptionNode);

        for (Enumeration e = nodeList.elements(); e.hasMoreElements();) {
            ((SalamisNode)e.nextElement()).resolve(labels, fg);
        }

        functionTable.put(functionName, fg);
    }

    protected ExternalCFGVariable getVar(String varName) {
        if (varName == null) {
            return null;
        }

        ExternalCFGVariable result = (ExternalCFGVariable)vars.get(varName);

        if (result == null) {
            result = new ExternalNamedVariable(varName);
            vars.put(varName, result);
        }

        return result;
    }

    protected ExternalCFGVariable defineVar(String def, SalamisDefNode node) {
        if (def == null) {
            def = "_";
        }

        if (def.equals("return")) {
            resultNode = node;
        } else if (def.equals("throw")) {
            exceptionNode = node;
        }

        return getVar(def);
    }

    protected void makeGoto(String label, Vector targets) {
        String[] targetArray = new String[targets.size()];

        targets.copyInto(targetArray);

        addNode(label, new SalamisGotoNode(targetArray));
    }

    protected void makeCatch(String label, String def, String className, Vector labels) {
        String[] labelArray;

        if (labels == null) {
            labelArray = new String[0];
        } else {
            labelArray = new String[labels.size()];
            labels.copyInto(labelArray);
        }

        addNode(label, new SalamisCatchDefNode(this, def, className, labelArray));
    }

    protected void makeChoose(String label, String def, Vector varNames) {
        ExternalCFGVariable[] varArray;

        if (varNames == null) {
            varArray = new ExternalCFGVariable[0];
        } else {
            varArray = new ExternalCFGVariable[varNames.size()];

            for (int i = varNames.size() - 1; i >= 0; i--) {
                varArray[i] = getVar((String)varNames.elementAt(i));
            }
        }

        addNode(label, new SalamisChooseDefNode(this, def, varArray));
    }

    protected void makeFieldAssign(String label, String def, String obj, String field, String value) throws ParseException {
        if (field.indexOf('#') >= 0) {
            addNode(label, new SalamisUserFieldAssignDefNode(this, def,
                    getVar(obj), field, getVar(value)));
        } else if (field.indexOf('.') >= 0) {
            addNode(label, new SalamisFieldAssignDefNode(this, def,
                    getVar(obj), field, getVar(value)));
        } else {
            throw new ParseException("Invalid field name in assignment: " + field
                + " (at line " + token.beginLine + ", column " + token.beginColumn + ")");
        }
    }

    protected void makeField(String label, String def, String obj, String field) throws ParseException {
        if (field.indexOf('#') >= 0) {
            addNode(label, new SalamisUserFieldDefNode(this, def, getVar(obj), field));
        } else if (field.indexOf('.') >= 0) {
            addNode(label, new SalamisFieldDefNode(this, def, getVar(obj), field));
        } else {
            throw new ParseException("Invalid field name: " + field
                + " (at line " + token.beginLine + ", column " + token.beginColumn + ")");
        }
    }

    protected void makeCall(String label, String def, String methodName, Vector varNames,
        String signature) {
        ExternalCFGVariable[] varArray;

        if (varNames == null) {
            varArray = new ExternalCFGVariable[0];
        } else {
            varArray = new ExternalCFGVariable[varNames.size()];

            for (int i = varNames.size() - 1; i >= 0; i--) {
                varArray[i] = getVar((String)varNames.elementAt(i));
            }
        }

        if (signature != null) {
            signature = signature.substring(1, signature.length() - 1);
        }

        if (methodName.indexOf('.') == -1) {
            addNode(label, new SalamisFlowgraphCallDefNode(this, def, methodName, varArray));
        } else {
            addNode(label, new SalamisMethodCallDefNode(this, def, methodName, varArray, signature));
        }
    }

    protected void makeNew(String label, String def, String className) {
        addNode(label, new SalamisNewDefNode(this, def, className));
    }

    protected void addNode(String label, ExternalCFGNode node) {
        if (label != null) {
            labels.put(label, node);
        }

        nodeList.addElement(node);

        if (curControlNode != null) {
            curControlNode.next = node;
        } else if (rootNode == null) {
            rootNode = node;
        }

        if (node instanceof SalamisDefNode) {
            curControlNode = (SalamisDefNode)node;
        } else {
            curControlNode = null;
        }
    }

    protected Hashtable makeFunctionTable() {
        Hashtable result = functionTable;

        functionTable = new Hashtable();

        return result;
    }

    public static String fixExtension(String fileName) {
        int lastDot = fileName.lastIndexOf('.');

        if (lastDot == -1) {
            return fileName + ".csal";
        } else if (fileName.substring(lastDot + 1).equals("sal")) {
            return fileName.substring(0, lastDot + 1) + "csal";
        } else {
            return fileName + ".csal";
        }
    }

    public static void main(String[] args) {
        try {
            if (args.length < 1) {
                System.err.println("Parameters: <infile> [ <outfile> ]");
                return;
            }

            String inFile = args[0];
            String outFile = args.length >= 2 ? args[1] : fixExtension(inFile);

            SalamisParser p = new SalamisParser(new BufferedReader(new FileReader(inFile)));
            Hashtable functions = p.CompilationUnit();
            ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream(outFile));

            out.writeObject(functions);
            out.close();
        } catch (IOException ex) {
            System.err.println("I/O error: " + ex.getMessage());
        } catch (ParseException ex) {
            System.err.println("Parse error: " + ex.getMessage());
        }
    }

  final public Hashtable CompilationUnit() throws ParseException {
    label_1:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case IDENTIFIER:
        ;
        break;
      default:
        jj_la1[0] = jj_gen;
        break label_1;
      }
      Function();
    }
    jj_consume_token(0);
    {if (true) return makeFunctionTable();}
    throw new Error("Missing return statement in function");
  }

  final public void Function() throws ParseException {
  String name = null; Vector identifiers = null;
    name = Name();
    jj_consume_token(22);
    switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
    case IDENTIFIER:
      identifiers = Identifiers();
      break;
    default:
      jj_la1[1] = jj_gen;
      ;
    }
    jj_consume_token(23);
                   beginFunction(name, identifiers);
    jj_consume_token(LBRACE);
    label_2:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CATCH:
      case GOTO:
      case CHOOSE:
      case NEW:
      case IDENTIFIER:
      case SEMICOLON:
        ;
        break;
      default:
        jj_la1[2] = jj_gen;
        break label_2;
      }
      Statement();
    }
    jj_consume_token(RBRACE);
                   endFunction();
  }

  final public void Statement() throws ParseException {
  String label = null; Vector identifiers = null; String def = null; String name = null; String v = null;
    if (jj_2_1(2147483647)) {
      jj_consume_token(IDENTIFIER);
                                               label = token.image;
      jj_consume_token(24);
    } else {
      ;
    }
    switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
    case CATCH:
    case GOTO:
    case CHOOSE:
    case NEW:
    case IDENTIFIER:
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case GOTO:
        jj_consume_token(GOTO);
        identifiers = Identifiers();
                   makeGoto(label, identifiers);
        break;
      default:
        jj_la1[8] = jj_gen;
        if (jj_2_4(2147483647)) {
          def = Definition();
          jj_consume_token(CATCH);
          jj_consume_token(22);
          switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
          case IDENTIFIER:
            name = Name();
            break;
          default:
            jj_la1[3] = jj_gen;
            ;
          }
          jj_consume_token(23);
          switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
          case IDENTIFIER:
            identifiers = Identifiers();
            break;
          default:
            jj_la1[4] = jj_gen;
            ;
          }
                   makeCatch(label, def, name, identifiers);
        } else if (jj_2_5(2147483647)) {
          def = Definition();
          jj_consume_token(CHOOSE);
          switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
          case IDENTIFIER:
            identifiers = Identifiers();
            break;
          default:
            jj_la1[5] = jj_gen;
            ;
          }
                   makeChoose(label, def, identifiers);
        } else if (jj_2_6(2147483647)) {
          def = Definition();
          if (jj_2_2(2147483647)) {
            jj_consume_token(IDENTIFIER);
                                                                       v = token.image;
          } else {
            ;
          }
          name = Name();
          jj_consume_token(25);
          jj_consume_token(IDENTIFIER);
                   makeFieldAssign(label, def, v, name, token.image);
        } else if (jj_2_7(2147483647)) {
          def = Definition();
          if (jj_2_3(2147483647)) {
            jj_consume_token(IDENTIFIER);
                                                                       v = token.image;
          } else {
            ;
          }
          name = Name();
                   makeField(label, def, v, name);
        } else if (jj_2_8(2147483647)) {
          def = Definition();
          name = Name();
          jj_consume_token(22);
          switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
          case IDENTIFIER:
            identifiers = Identifiers();
            break;
          default:
            jj_la1[6] = jj_gen;
            ;
          }
          jj_consume_token(23);
          switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
          case STRING:
            jj_consume_token(STRING);
                              v = token.image;
            break;
          default:
            jj_la1[7] = jj_gen;
            ;
          }
                   makeCall(label, def, name, identifiers, v);
        } else if (jj_2_9(2147483647)) {
          def = Definition();
          jj_consume_token(NEW);
          name = Name();
                   makeNew(label, def, name);
        } else {
          jj_consume_token(-1);
          throw new ParseException();
        }
      }
      break;
    default:
      jj_la1[9] = jj_gen;
      ;
    }
    jj_consume_token(SEMICOLON);
  }

  final public String Definition() throws ParseException {
                 String identifier = null;
    if (jj_2_10(2)) {
      jj_consume_token(IDENTIFIER);
                                identifier = token.image;
      jj_consume_token(26);
    } else {
      ;
    }
                 {if (true) return identifier;}
    throw new Error("Missing return statement in function");
  }

  final public Vector Identifiers() throws ParseException {
                       Vector list = new Vector();
    jj_consume_token(IDENTIFIER);
                       list.addElement(token.image);
    label_3:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case COMMA:
        ;
        break;
      default:
        jj_la1[10] = jj_gen;
        break label_3;
      }
      jj_consume_token(COMMA);
      jj_consume_token(IDENTIFIER);
                       list.addElement(token.image);
    }
                       {if (true) return list;}
    throw new Error("Missing return statement in function");
  }

  final public String Name() throws ParseException {
                                 StringBuffer buf;
    jj_consume_token(IDENTIFIER);
                                 buf = new StringBuffer(token.image);
    label_4:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case DOT:
      case 27:
        ;
        break;
      default:
        jj_la1[11] = jj_gen;
        break label_4;
      }
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case DOT:
        jj_consume_token(DOT);
                                 buf.append(token.image);
        break;
      case 27:
        jj_consume_token(27);
                                 buf.append(token.image);
        break;
      default:
        jj_la1[12] = jj_gen;
        jj_consume_token(-1);
        throw new ParseException();
      }
      jj_consume_token(IDENTIFIER);
                                 buf.append(token.image);
    }
                                 {if (true) return buf.toString();}
    throw new Error("Missing return statement in function");
  }

  final private boolean jj_2_1(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    boolean retval = !jj_3_1();
    jj_save(0, xla);
    return retval;
  }

  final private boolean jj_2_2(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    boolean retval = !jj_3_2();
    jj_save(1, xla);
    return retval;
  }

  final private boolean jj_2_3(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    boolean retval = !jj_3_3();
    jj_save(2, xla);
    return retval;
  }

  final private boolean jj_2_4(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    boolean retval = !jj_3_4();
    jj_save(3, xla);
    return retval;
  }

  final private boolean jj_2_5(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    boolean retval = !jj_3_5();
    jj_save(4, xla);
    return retval;
  }

  final private boolean jj_2_6(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    boolean retval = !jj_3_6();
    jj_save(5, xla);
    return retval;
  }

  final private boolean jj_2_7(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    boolean retval = !jj_3_7();
    jj_save(6, xla);
    return retval;
  }

  final private boolean jj_2_8(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    boolean retval = !jj_3_8();
    jj_save(7, xla);
    return retval;
  }

  final private boolean jj_2_9(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    boolean retval = !jj_3_9();
    jj_save(8, xla);
    return retval;
  }

  final private boolean jj_2_10(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    boolean retval = !jj_3_10();
    jj_save(9, xla);
    return retval;
  }

  final private boolean jj_3R_11() {
    if (jj_scan_token(27)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_10() {
    if (jj_scan_token(DOT)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_9() {
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3R_10()) {
    jj_scanpos = xsp;
    if (jj_3R_11()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    } else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_scan_token(IDENTIFIER)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_8() {
    if (jj_3R_6()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_6() {
    if (jj_scan_token(IDENTIFIER)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    Token xsp;
    while (true) {
      xsp = jj_scanpos;
      if (jj_3R_9()) { jj_scanpos = xsp; break; }
      if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    }
    return false;
  }

  final private boolean jj_3R_7() {
    if (jj_3R_6()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3_3() {
    if (jj_scan_token(IDENTIFIER)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_scan_token(IDENTIFIER)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3_2() {
    if (jj_scan_token(IDENTIFIER)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_scan_token(IDENTIFIER)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3_9() {
    if (jj_3R_5()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_scan_token(NEW)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3_10() {
    if (jj_scan_token(IDENTIFIER)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_scan_token(26)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3R_5() {
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3_10()) jj_scanpos = xsp;
    else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3_8() {
    if (jj_3R_5()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_3R_6()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_scan_token(22)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3_7() {
    if (jj_3R_5()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_3R_6()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3R_8()) jj_scanpos = xsp;
    else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_scan_token(SEMICOLON)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3_6() {
    if (jj_3R_5()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_3R_6()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3R_7()) jj_scanpos = xsp;
    else if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_scan_token(25)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3_5() {
    if (jj_3R_5()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_scan_token(CHOOSE)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3_4() {
    if (jj_3R_5()) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_scan_token(CATCH)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  final private boolean jj_3_1() {
    if (jj_scan_token(IDENTIFIER)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    if (jj_scan_token(24)) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) return false;
    return false;
  }

  public SalamisParserTokenManager token_source;
  ASCII_UCodeESC_CharStream jj_input_stream;
  public Token token, jj_nt;
  private int jj_ntk;
  private Token jj_scanpos, jj_lastpos;
  private int jj_la;
  public boolean lookingAhead = false;
  private boolean jj_semLA;
  private int jj_gen;
  final private int[] jj_la1 = new int[13];
  final private int[] jj_la1_0 = {0x4000,0x4000,0x85e00,0x4000,0x4000,0x4000,0x4000,0x2000,0x400,0x5e00,0x100000,0x8200000,0x8200000,};
  final private JJCalls[] jj_2_rtns = new JJCalls[10];
  private boolean jj_rescan = false;
  private int jj_gc = 0;

  public SalamisParser(java.io.InputStream stream) {
    jj_input_stream = new ASCII_UCodeESC_CharStream(stream, 1, 1);
    token_source = new SalamisParserTokenManager(jj_input_stream);
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 13; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  public void ReInit(java.io.InputStream stream) {
    jj_input_stream.ReInit(stream, 1, 1);
    token_source.ReInit(jj_input_stream);
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 13; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  public SalamisParser(java.io.Reader stream) {
    jj_input_stream = new ASCII_UCodeESC_CharStream(stream, 1, 1);
    token_source = new SalamisParserTokenManager(jj_input_stream);
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 13; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  public void ReInit(java.io.Reader stream) {
    jj_input_stream.ReInit(stream, 1, 1);
    token_source.ReInit(jj_input_stream);
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 13; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  public SalamisParser(SalamisParserTokenManager tm) {
    token_source = tm;
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 13; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  public void ReInit(SalamisParserTokenManager tm) {
    token_source = tm;
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 13; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  final private Token jj_consume_token(int kind) throws ParseException {
    Token oldToken;
    if ((oldToken = token).next != null) token = token.next;
    else token = token.next = token_source.getNextToken();
    jj_ntk = -1;
    if (token.kind == kind) {
      jj_gen++;
      if (++jj_gc > 100) {
        jj_gc = 0;
        for (int i = 0; i < jj_2_rtns.length; i++) {
          JJCalls c = jj_2_rtns[i];
          while (c != null) {
            if (c.gen < jj_gen) c.first = null;
            c = c.next;
          }
        }
      }
      return token;
    }
    token = oldToken;
    jj_kind = kind;
    throw generateParseException();
  }

  final private boolean jj_scan_token(int kind) {
    if (jj_scanpos == jj_lastpos) {
      jj_la--;
      if (jj_scanpos.next == null) {
        jj_lastpos = jj_scanpos = jj_scanpos.next = token_source.getNextToken();
      } else {
        jj_lastpos = jj_scanpos = jj_scanpos.next;
      }
    } else {
      jj_scanpos = jj_scanpos.next;
    }
    if (jj_rescan) {
      int i = 0; Token tok = token;
      while (tok != null && tok != jj_scanpos) { i++; tok = tok.next; }
      if (tok != null) jj_add_error_token(kind, i);
    }
    return (jj_scanpos.kind != kind);
  }

  final public Token getNextToken() {
    if (token.next != null) token = token.next;
    else token = token.next = token_source.getNextToken();
    jj_ntk = -1;
    jj_gen++;
    return token;
  }

  final public Token getToken(int index) {
    Token t = lookingAhead ? jj_scanpos : token;
    for (int i = 0; i < index; i++) {
      if (t.next != null) t = t.next;
      else t = t.next = token_source.getNextToken();
    }
    return t;
  }

  final private int jj_ntk() {
    if ((jj_nt=token.next) == null)
      return (jj_ntk = (token.next=token_source.getNextToken()).kind);
    else
      return (jj_ntk = jj_nt.kind);
  }

  private java.util.Vector jj_expentries = new java.util.Vector();
  private int[] jj_expentry;
  private int jj_kind = -1;
  private int[] jj_lasttokens = new int[100];
  private int jj_endpos;

  private void jj_add_error_token(int kind, int pos) {
    if (pos >= 100) return;
    if (pos == jj_endpos + 1) {
      jj_lasttokens[jj_endpos++] = kind;
    } else if (jj_endpos != 0) {
      jj_expentry = new int[jj_endpos];
      for (int i = 0; i < jj_endpos; i++) {
        jj_expentry[i] = jj_lasttokens[i];
      }
      boolean exists = false;
      for (java.util.Enumeration enum = jj_expentries.elements(); enum.hasMoreElements();) {
        int[] oldentry = (int[])(enum.nextElement());
        if (oldentry.length == jj_expentry.length) {
          exists = true;
          for (int i = 0; i < jj_expentry.length; i++) {
            if (oldentry[i] != jj_expentry[i]) {
              exists = false;
              break;
            }
          }
          if (exists) break;
        }
      }
      if (!exists) jj_expentries.addElement(jj_expentry);
      if (pos != 0) jj_lasttokens[(jj_endpos = pos) - 1] = kind;
    }
  }

  final public ParseException generateParseException() {
    jj_expentries.removeAllElements();
    boolean[] la1tokens = new boolean[28];
    for (int i = 0; i < 28; i++) {
      la1tokens[i] = false;
    }
    if (jj_kind >= 0) {
      la1tokens[jj_kind] = true;
      jj_kind = -1;
    }
    for (int i = 0; i < 13; i++) {
      if (jj_la1[i] == jj_gen) {
        for (int j = 0; j < 32; j++) {
          if ((jj_la1_0[i] & (1<<j)) != 0) {
            la1tokens[j] = true;
          }
        }
      }
    }
    for (int i = 0; i < 28; i++) {
      if (la1tokens[i]) {
        jj_expentry = new int[1];
        jj_expentry[0] = i;
        jj_expentries.addElement(jj_expentry);
      }
    }
    jj_endpos = 0;
    jj_rescan_token();
    jj_add_error_token(0, 0);
    int[][] exptokseq = new int[jj_expentries.size()][];
    for (int i = 0; i < jj_expentries.size(); i++) {
      exptokseq[i] = (int[])jj_expentries.elementAt(i);
    }
    return new ParseException(token, exptokseq, tokenImage);
  }

  final public void enable_tracing() {
  }

  final public void disable_tracing() {
  }

  final private void jj_rescan_token() {
    jj_rescan = true;
    for (int i = 0; i < 10; i++) {
      JJCalls p = jj_2_rtns[i];
      do {
        if (p.gen > jj_gen) {
          jj_la = p.arg; jj_lastpos = jj_scanpos = p.first;
          switch (i) {
            case 0: jj_3_1(); break;
            case 1: jj_3_2(); break;
            case 2: jj_3_3(); break;
            case 3: jj_3_4(); break;
            case 4: jj_3_5(); break;
            case 5: jj_3_6(); break;
            case 6: jj_3_7(); break;
            case 7: jj_3_8(); break;
            case 8: jj_3_9(); break;
            case 9: jj_3_10(); break;
          }
        }
        p = p.next;
      } while (p != null);
    }
    jj_rescan = false;
  }

  final private void jj_save(int index, int xla) {
    JJCalls p = jj_2_rtns[index];
    while (p.gen > jj_gen) {
      if (p.next == null) { p = p.next = new JJCalls(); break; }
      p = p.next;
    }
    p.gen = jj_gen + xla - jj_la; p.first = token; p.arg = xla;
  }

  static final class JJCalls {
    int gen;
    Token first;
    int arg;
    JJCalls next;
  }

}

/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver.test;

import ajax.solver.*;
import java.util.*;
import java.io.*;
import ajax.util.StringSorter;
import ajax.solver.util.SolverDebug;

public class Test3 {
    private static String makeVar(int[] nonce) {
        int n = nonce[0];
        char[] ch = { (char)('a' + n%26) };
        String s = new String(ch);
       
        nonce[0]++;

        if (n < 26) {
            return s;
        } else {
            return s + (n/26 - 1);
        }
    }
    
    private static boolean isTupleType(Hashtable labels) {
        int i = 0;
        
        while (labels.get("#" + i) != null) {
            i++;
        }
        
        return i == labels.size();
    }
    
    private static String localToString(World w, Variable v, Hashtable visited, int[] nonce) {
        v = v.getHead();
        
        Enumeration e = v.getComponents(w);
        
        if (!e.hasMoreElements()) {
            String s = makeVar(nonce);
            
            visited.put(v, s);
            return s;
        } else {
            Hashtable labels = new Hashtable();
            
            do {
                ComponentElement elem = (ComponentElement)e.nextElement();
                
                labels.put(((TypeComponent)elem.getLabel()).getName(), elem.getComponent());
            } while (e.hasMoreElements());
            
            if (labels.size() == 2 && labels.get("arg") != null && labels.get("result") != null) {
                return "(" + toString(w, (Variable)labels.get("arg"), visited, nonce) + " -> "
                    + toString(w, (Variable)labels.get("result"), visited, nonce) + ")";
            } else if (isTupleType(labels)) {
                StringBuffer buf = new StringBuffer();
                
                buf.append("[");
                for (int i = 0; i < labels.size(); i++) {
                    if (i > 0) {
                        buf.append(", ");
                    }
                    buf.append(toString(w, (Variable)labels.get("#" + i), visited, nonce));
                }
                buf.append("]");
                
                return buf.toString();
            } else {
                String[] labelList = new String[labels.size()];
                int i = 0;
                StringBuffer buf = new StringBuffer();
                
                for (Enumeration e2 = labels.keys(); e2.hasMoreElements();) {
                    labelList[i] = (String)e2.nextElement();
                    i++;
                }
                
                StringSorter.sort(labelList);
                
                buf.append("(");
                for (i = 0; i < labelList.length; i++) {
                    buf.append(labelList[i]);
                    buf.append(":");
                    buf.append(toString(w, (Variable)labels.get(labelList[i]), visited, nonce));
                    
                    if (i < labelList.length - 1) {
                      buf.append("; ");
                    }
                }
                buf.append(")");
                
                return buf.toString();
            }
        }
    }
    
    private static String toString(World w, Variable v, Hashtable visited, int[] nonce) {
        String visitedV = (String)visited.get(v);
        
        if (visitedV != null) {
            if (visitedV.equals("")) {
                visitedV = makeVar(nonce);
                visited.put(v, visitedV);
            }
            
            return visitedV;
        } else {
            visited.put(v, "");
            
            String result = localToString(w, v, visited, nonce);
            
            visitedV = (String)visited.get(v);
            if (!visitedV.equals("")
              && v.getHead().getComponents(w).hasMoreElements()) {
                result = "rec " + visitedV + ". " + result;
            }
            visited.remove(v);
            
            return result;
        }
    }
    
    static String toString(World w, Variable v) {
        return toString(w, v, new Hashtable(), new int[1]);
    }
    
    static void dumpGraph(String name, String data) {
        try {
            FileWriter w = new FileWriter(name + ".dot");
            
            w.write(data);
            w.close();
        } catch (IOException ex) {
            throw new Error("IO error: " + ex.getMessage());
        }
    }
    
    static Variable getType(World w, Term t, int n, boolean doGraphs) {
        Environment env = new Environment();
        Variable v = t.makeType(w, env);
        int i = 0;
        String path = "m:\\tmp\\graphs\\graph-" + n + "-";
        String graph = null;
        
        if (doGraphs) {
            graph = SolverDebug.dumpVarInfo(env.getBindingList(), w);
            dumpGraph(path + i, graph);
        }
        
        while (w.work()) {
            if (doGraphs) {
                String newgraph = SolverDebug.dumpVarInfo(env.getBindingList(), w);
                
                if (!newgraph.equals(graph)) {
                    i++;
                    graph = newgraph;
                    dumpGraph(path + i, graph);
                }
            }
        }
        
        return v;
    }
    
    static Term pair(Term a, Term b) {
        Term[] terms = { a, b };
        
        return new TupleTerm(terms);
    }
    
    static Term triple(Term a, Term b, Term c) {
        Term[] terms = { a, b, c };
        
        return new TupleTerm(terms);
    }
    
    static Term head(Term t) {
        return new ProjectionTerm(t, 0);
    }
    
    static Term tail(Term t) {
        return new ProjectionTerm(t, 1);
    }
    
    static Term rest(Term t) {
        return new ProjectionTerm(t, 2);
    }
    
    static Term lam(String a, Term b) {
        return new LambdaTerm(a, b);
    }
    
    static Term app(Term a, Term b) {
        return new AppTerm(a, b);
    }
    
    static Term let(String a, Term t, Term b) {
        String[] vars = { a };
        Term[] terms = { t };
        
        return new LetrecTerm(vars, terms, b);
    }
    
    static Term letrec(String a, Term t, Term b) {
        String[] vars = { a };
        Term[] terms = { t };
        
        return new LetrecTerm(vars, terms, b);
    }
    
    static Term v(String name) {
        return new VarTerm(name);
    }
    
    static Term p(String name, String label) {
        return new PolyVarTerm(name, label);
    }

    private static Term terms[] = {
        let("f0", lam("x", v("x")), v("f0")),
        let("f0", lam("x", v("x")), p("f0", "~f0")),
        let("f", lam("x", v("x")), app(p("f", "~f_a"), p("f", "~f_b"))),
        let("f0", lam("x", v("x")),
          let("f1",  pair(p("f0", "~f0_a"), p("f0", "~f0_b")),
            let("f2",  pair(p("f1", "~f1_a"), p("f1", "~f1_b")),
              let("f3",  pair(p("f2", "~f2_a"), p("f2", "~f2_b")),
                let("_", head(head(head(v("f3")))),
                  v("f3")))))),
        letrec("f", lam("x", app(p("f", "~f_r"), head(v("x")))),
          letrec("t", pair(p("t", "~t_r"), lam("x", v("x"))),
            let("a", p("f", "~f"),
              let("_", app(v("a"), v("t")),
                v("a")))))
    };
    
    static Term makeBody(int n, int f1, int f2, int f3) {
        return
            let("l" + n, app(p("f" + f3, "~" + n + "-" + f3), v("x" + n)),
                let("r" + n, app(p("f" + f2, "~" + n + "-" + f2),
                    triple(v("l" + n), tail(v("x" + n)), rest(v("x" + n)))),
                    triple(app(p("f" + f1, "~" + n + "-" + f1),
                        triple(v("l" + n), v("r" + n), head(v("x" + n)))),
                        v("r" + n), v("l" + n))));
    }
    
    static Term makeBigTerm(int n, int max) {
        return let("f" + n, lam("x" + n,
                makeBody(n, Math.max(n - 1, 0), Math.max(n - 2, 0), Math.max(n - 3, 0))),
            n < max ? makeBigTerm(n + 1, max) : v("f" + n));
    }
    
    static Term makeBigTerm() {
        return makeBigTerm(0, 10);
    }
    
    static boolean runTest() {
        World w = new World();
        Term term = terms[3];
        Variable v = getType(w, term, 0, true);
        
//        System.err.println(term.toString());
        System.err.println(toString(w, v));
        
        return true;
    }
    
    public static void main(String[] args) {
        runTest();
    }
}

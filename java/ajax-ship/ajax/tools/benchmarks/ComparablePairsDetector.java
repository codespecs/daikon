/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.benchmarks;

import java.util.*;
import ajax.jbc.*;
import ajax.jbc.util.*;
import java.io.*;
import ajax.analyzer.*;
import ajax.analyzer.semantics.*;
import ajax.analyzer.util.*;
import ajax.util.*;

public class ComparablePairsDetector extends Benchmark implements ResultListener, DatumSpecifier {
    private String locationName;
    private JBCLocation location = null;
    private int numLocals;
    private Hashtable results = new Hashtable();
    private String errMsg = null;
    private CompactSet expressions = null;
    private Hashtable exprNames = null;
    private String detectorName = null;
    
    protected ComparablePairsDetector() {
    }

    protected ComparablePairsDetector(JBCLocation location, CompactSet expressions, Hashtable exprNames, String detectorName) {
        this.location = location;
        this.expressions = expressions;
        this.exprNames = exprNames;
        this.detectorName = detectorName;
    }
    
    public JBCLocation getLocation() {
        return location;
    }
    
    public Hashtable getExprNames() {
        return exprNames;
    }
    
    public String getDetectorName() {
        return detectorName;
    }

    public void registerTarget(Object targetCookie) {
    }
    
    public void notifyAnalysisComplete() {
    }
    
    public void updateResult(Object targetCookie, Object intermediate) {
        if (intermediate != null) {
            results.put(targetCookie, intermediate);
	} else {
            results.remove(targetCookie);
	}
        errMsg = null;
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, JBCMethod method, ExternalFlowgraph fg, Vector externalNodes) {
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, String name, ExternalFlowgraph fg, Vector externalNodes) {
    }
    
    Hashtable getResults() {
        return results;
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, JBCMethod method, byte[] code, boolean[] instructions) {
        if (method.equals(location.getMethod())) {
            int offset = location.getOffset();
            
            if (!instructions[offset]) {
                errMsg = "The offset " + offset + " in method " + location.getMethod() + " is not the beginning of an instruction!\n";
            } else {
                errMsg = "The offset " + offset + " in method " + location.getMethod() + " is not reachable!\n";
                
                if (expressions == null) {
                    for (int i = 0; i < numLocals; i++) {
                        Integer iObj = new Integer(i);
                        JBCExpression iExpr = JBCExpression.makeLocalVarExpression(i);
                        
                        family.addSourceDatum(offset, iExpr, iObj);
                        family.addTargetDatum(offset, iExpr, iObj);
                    }
                } else {
                    for (Enumeration e = expressions.elements(); e.hasMoreElements();) {
                        JBCExpression expr = (JBCExpression)e.nextElement();
                        
                        family.addSourceDatum(offset, expr, expr);
                        family.addTargetDatum(offset, expr, expr);
                    }
                }
            }
        }
    }
    
    public void configure(Analyzer analyzer) {
        DatumSpecifier[] specifiers = { this };
        ResultListener[] listeners = { this };
        
        analyzer.setSemantics(new CombiningSemantics());
        
        try {
            location = QueryExpressionParser.parseLocation(analyzer.getWorld().getSystemClassLoader(),
                locationName);
            if (location.getMethod() == null) {
	        System.err.println("No method?");
 	    }
	    if (location.getMethod().getData() == null) {
		System.err.println("Cannot find method: " + location.getMethod());
	    }
        } catch (ParseException ex) {
            System.err.println("Error in location " + locationName + ": "
                + ex.getMessage());
            System.exit(2);
            return;
        }
 
        errMsg = "Method " + location.getMethod() + " is dead!\n";
        numLocals = location.getMethod().getData().getMaxLocalWords();
            
        (new UnboundedSetTracker(analyzer, specifiers, listeners)).start();
    }

    public void printReport(Writer w) throws IOException {
        String[] varNames =
            JBCCodeUtilities.getLocalVariableNames(location.getMethod(), location.getOffset());
        
        if (errMsg != null) {
            w.write(errMsg);
            return;
        }
        
        w.write("Results:\n");
        
        for (int i = 0; i < varNames.length; i++) {
            w.write(varNames[i] + ": ");
            
            for (Enumeration e = UnboundedSetTracker.enumerateIntermediate(results.get(new Integer(i)));
                e.hasMoreElements();) {
                w.write(varNames[((Integer)e.nextElement()).intValue()] + " ");
            }
            
            w.write("\n");
        }
    }
    
    public void parseArgs(Args args) {
        locationName = args.extractNextArg("<location>");
    }
    
    public static void main(String[] args) {
        GeneralBenchmark.run(args, new ComparablePairsDetector(), "<location>", "RTA-SEMI");
    }
}

/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.benchmarks;

import java.io.*;
import ajax.analyzer.*;
import ajax.analyzer.util.*;
import ajax.util.*;
import java.util.*;
import ajax.tools.explain.*;

public class ExplainerTool extends Benchmark implements ExplanationListener {
    private String expr1;
    private String expr2;
    private Hashtable indices = new Hashtable();
    private CompactSet results = new CompactSet();
    private int counter = 0;
    
    protected ExplainerTool() {
    }
    
    public void configure(Analyzer analyzer) {
        analyzer.setDelayLiveProcessing();
        
        ExplanationGatherer explainer = new ExplanationGatherer(analyzer, this);
        
        explainer.setShortCircuitTermination();
        
        try {
            CompactSet p1 = QueryExpressionParser.parseExpression(
                analyzer.getWorld().getSystemClassLoader(), expr1);

            for (Enumeration e = p1.elements(); e.hasMoreElements();) {
                explainer.addPoint1((JBCValuePoint)e.nextElement());
            }
        } catch (ParseException ex) {
            System.err.println("Error in expression " + expr1 + ": "
                + ex.getMessage());
            System.exit(2);
        }
        
        try {
            CompactSet p2 = QueryExpressionParser.parseExpression(
                analyzer.getWorld().getSystemClassLoader(), expr2);
        
            for (Enumeration e = p2.elements(); e.hasMoreElements();) {
                explainer.addPoint2((JBCValuePoint)e.nextElement());
            }
        } catch (ParseException ex) {
            System.err.println("Error in expression " + expr2 + ": "
                + ex.getMessage());
            System.exit(2);
        }
    }
    
    public void notifyReachedFromPoint1(SearchItem item) {
        if (indices.get(item) == null) {
            indices.put(item, new Integer(counter));
            counter++;
        }
    }
    
    public void notifyReachedFromPoint2(SearchItem item) {
    }
    
    public void notifyReached(SearchItem item) {
        results.add(item);
    }
    
    private String makeString(SearchItem item) {
        return item.getType() + "\t" + indices.get(item) + "\t" + item.toString();
    }
    
    public void printReport(Writer w) throws IOException {
        for (Enumeration e = results.elements(); e.hasMoreElements();) {
            SearchItem item = (SearchItem)e.nextElement();
            
            w.write(makeString(item) + "\n");
        }
    }
    
    public void parseArgs(Args args) {
        expr1 = args.extractNextArg("<value-point-1>");
        expr2 = args.extractNextArg("<value-point-2>");
    }
    
    public static void main(String[] args) {
        GeneralBenchmark.run(args, new ExplainerTool(), "<value-point-1> <value-point-2>");
    }
    
    public void terminate() {
        super.terminate();
    }
}

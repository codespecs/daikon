/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.benchmarks;

import ajax.Globals;
import java.io.*;
import ajax.analyzer.*;
import ajax.analyzer.util.*;
import ajax.util.*;
import java.util.*;
import ajax.tools.explain.*;
import ajax.jbc.*;
import ajax.jbc.util.*;

public class PairCheck extends Benchmark implements ResultListener {
    private String expr1 = null;
    private String expr2 = null;
    private boolean foundMatch = false;
    
    protected PairCheck() {
    }
    
    public void notifyAnalysisComplete() {
    }
    
    public void configure(Analyzer analyzer) {
        DatumSpecifier sources;
        DatumSpecifier targets;
        
        try {
            sources = ExplicitValuePointSource.makeFromQuery(
                analyzer.getWorld().getSystemClassLoader(), expr1);
        } catch (ParseException ex) {
            System.err.println("Error in expression " + expr1 + ": "
                + ex.getMessage());
            System.exit(2);
            return;
        }
        
        try {
            targets = ExplicitValuePointTarget.makeFromQuery(
                analyzer.getWorld().getSystemClassLoader(), expr2);
        } catch (ParseException ex) {
            System.err.println("Error in expression " + expr2 + ": "
                + ex.getMessage());
            System.exit(2);
            return;
        }
        
        DatumSpecifier[] specifiers = { sources, targets };
        ResultListener[] listeners = { this };
        
        (new SingletonTracker(analyzer, specifiers, listeners)).start();
    }
    
    public void printReport(Writer w) throws IOException {
        w.write(foundMatch ? "RELATED\n" : "NOT RELATED\n");
    }
    
    public void registerTarget(Object targetCookie) {
    }
    
    public void updateResult(Object targetCookie, Object intermediate) {
        foundMatch = intermediate != null;
    }
    
    public void parseArgs(Args args) {
        expr1 = args.extractNextArg("<value-point-1>");
        expr2 = args.extractNextArg("<value-point-2>");
    }
    
    public static void main(String[] args) {
        GeneralBenchmark.run(args, new PairCheck(),
            "<value-point-1> <value-point-2>");
    }
}

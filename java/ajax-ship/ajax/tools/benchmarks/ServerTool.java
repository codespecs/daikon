/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.benchmarks;

import ajax.Globals;
import java.util.*;
import ajax.jbc.*;
import ajax.jbc.util.*;
import java.io.*;
import ajax.analyzer.*;
import ajax.analyzer.util.*;
import ajax.util.*;
import ajax.tools.server.*;

public class ServerTool extends Benchmark {
    private String serverName;
    private JBCClassLoader appClassLoader;
    private Server server = null;

    public ServerTool() {
    }

    public void parseOptions(Args args) {
        serverName = args.extractNextArg("<server-name>");
    }
    
    public void configure(Analyzer analyzer) {
    }
    
    public void printReport(Writer w) throws IOException {
    }
    
    protected void notifyAppClassLoader(JBCClassLoader appClassLoader) {
        this.appClassLoader = appClassLoader;
    }
    
    public boolean work(Analyzer analyzer) {
        server = Server.create(serverName, analyzer, appClassLoader);
        
        if (server != null) {
            server.run();
        } else {
            Globals.userError("Cannot create server!");
        }
        
        return false;
    }

    protected boolean usesDynamicQueryFamilies() {
        return true;
    }

    public void terminate() {
	    if (server != null) {
	        server.terminate();
	    }
    }
    
    public static void main(String[] args) {
        GeneralBenchmark.run(args, new ServerTool(), "<server-name>");
    }
}

/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.benchmarks;

import java.io.*;
import ajax.analyzer.*;

class GeneralBenchmarkTerminationThread implements Runnable {
    private GeneralBenchmark benchmark;
    
    public GeneralBenchmarkTerminationThread(GeneralBenchmark benchmark) {
        this.benchmark = benchmark;
    }
    
    public void start() {
        Thread t = new Thread(this, "Benchmark Termination Thread");
        
        t.setDaemon(true);
        t.start();
    }
    
    public void run() {
        try {
            int ch = System.in.read();
                    
            if (ch != -1) {
		benchmark.terminate("KILLED due to user input (char " + ch + ")");
            }
        } catch (IOException ex) {
        }
    }
}

/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.benchmarks;

import java.io.*;
import ajax.analyzer.*;

class GeneralBenchmarkTimerTerminationThread implements Runnable {
    private GeneralBenchmark benchmark;
    private int duration;
    private long maxUsedMemory = 0;
    
/**
@param duration the amount of time to wait before shutting down the analyzer, in seconds
*/
    public GeneralBenchmarkTimerTerminationThread(GeneralBenchmark benchmark,
        int duration) {
        this.benchmark = benchmark;
        this.duration = duration;
    }
    
    public void start() {
        Thread t = new Thread(this, "Benchmark Timer Termination Thread");
        
        t.setDaemon(true);
        t.start();
    }
    
    public long getMaxUsedMemory() {
        return maxUsedMemory;
    }
    
    public void run() {
        try {
            for (int i = 0; i < duration; i++) {
                Thread.sleep(1000);
                
                Runtime r = Runtime.getRuntime();
                long memUsed = r.totalMemory() - r.freeMemory();
                
                if (memUsed > maxUsedMemory) {
                    maxUsedMemory = memUsed;
                }
            }
            benchmark.terminate("TIMEOUT");
        } catch (InterruptedException ex) {
        }
    }
}

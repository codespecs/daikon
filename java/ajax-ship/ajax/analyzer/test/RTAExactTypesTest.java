/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.test;

public class RTAExactTypesTest extends Thread {
    public void run() {
    }
    
    private static void f2(RTAExactTypesTest t) {
        t.run();
    }
    
    public static void main(String[] args) {
        (new Thread()).run();
        f2(new RTAExactTypesTest());
    }
}

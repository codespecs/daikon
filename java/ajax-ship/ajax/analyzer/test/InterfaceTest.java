/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.test;

import ajax.jbc.*;
import ajax.jbc.util.*;
import java.io.*;
import java.util.*;
import java.text.*;

public class InterfaceTest implements Runnable {
    public void run() {
        System.out.println("Hello");
    }
    
    static void doTest(Runnable r) {
        r.run();
    }
    
    public static void main(String[] args) {
        Thread t = new Thread(); // Another implementation of Runnable to confuse RTA
        
        doTest(new InterfaceTest());
    }
}

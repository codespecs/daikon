/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.test;

import ajax.jbc.*;
import ajax.jbc.util.*;
import java.io.*;
import java.util.*;
import java.text.*;

public class DowncastTest {
    public void run() {
        System.out.println("Hello");
    }
    
    static Object makeDowncastTest2() {
        return new DowncastTest();
    }
    
    static DowncastTest makeDowncastTest() {
        makeDowncastTest2();
        
        return (DowncastTest)makeDowncastTest2();
    }
    
    public static void main(String[] args) {
        makeDowncastTest();
        
        DowncastTest obj = makeDowncastTest();
        
        obj.run();
    }
}

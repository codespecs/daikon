/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.test;

import ajax.jbc.*;
import ajax.jbc.util.*;
import java.io.*;
import java.util.*;
import java.text.*;

public class ExceptionTest extends Exception {
    ExceptionTest() {
    }
    
    void run() {
    }
    
    static void test() throws Exception {
        throw new ExceptionTest();
    }
    
    public static void main(String[] args) {
        try {
            test();
        } catch (ExceptionTest ex) {
            ex.run();
        } catch (Exception ex) {
        }
    }
}

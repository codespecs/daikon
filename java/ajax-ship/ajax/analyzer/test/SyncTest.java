/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.test;

public class SyncTest {
    private Object o1 = new Object();
    private Object o2 = new Object();
    
    public static void myF2(Object o1, Object o2) {
        synchronized (o1) {
            synchronized (o2) {
            }
        }
    }
    
    public static void myFun(Object o1, Object o2) {
        synchronized (o2) {
        }
    }
    
    public static void main(String[] args) {
        Object o1 = new Object();
        Object o2 = new Object();
        
        synchronized (o1) {
            myFun(o1, o2);
        }
    }
}

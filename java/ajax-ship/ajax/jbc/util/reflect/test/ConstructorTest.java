/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util.reflect.test;

import java.util.*;
import java.lang.reflect.*;

public class ConstructorTest {
    public static void main(String[] args) {
        Class c = Hashtable.class;
        Class[] classParams = { Integer.TYPE, Integer.TYPE };
        Object[] a = { new Integer(5), new Float(0.9) };
        
        try {
            Constructor con = c.getConstructor(classParams);
            Hashtable h = (Hashtable)con.newInstance(a);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
}

/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.test;

import java.util.*;
import java.lang.reflect.*;

public class ReflectionTest {
    private static String arg0;
    
    public ReflectionTest() {
        System.out.println("I'm Live --- A!");
    }
    
    public void hello() {
        System.out.println("I'm Live --- B!");
        
        try {
            Class c = getClass();
            Class[] params = new Class[0];
            Method m = c.getMethod(arg0, params);
        
            m.invoke(this, new Object[0]);
        } catch (Exception ex) {
            ex.printStackTrace();
        } catch (Error ex) {
            ex.printStackTrace();
        }
    }

    public static void hello2() {
        System.out.println("I'm Live --- C!");
    }
    
    public static void main(String[] args) {
        arg0 = args[0];
        
        try {
            Class c = Class.forName("ajax.analyzer.test.ReflectionTest");
            Class[] params = new Class[0];
            Method m = c.getMethod(arg0, params);
        
            m.invoke(c.newInstance(), new Object[0]);
            
            OneOffTest.class.newInstance();
        } catch (Exception ex) {
            ex.printStackTrace();
        } catch (Error ex) {
            ex.printStackTrace();
        }
    }
}

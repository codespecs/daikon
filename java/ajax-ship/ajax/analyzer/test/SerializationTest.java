/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.test;

import ajax.jbc.*;
import ajax.jbc.util.*;
import java.io.*;
import java.util.*;
import java.text.*;

public class SerializationTest {
    public static void main(String[] args) {
        try {
            ObjectInputStream in = new ObjectInputStream(new FileInputStream("native-spec.csal"));
            Hashtable code = (Hashtable)in.readObject();
            
            for (Enumeration e = code.keys(); e.hasMoreElements();) {
                Object key = e.nextElement();
                
                System.out.println(key);
                System.out.println(code.get(key));
            }
        } catch (IOException ex) {
            System.err.println(ex);
        } catch (ClassNotFoundException ex) {
            System.err.println(ex);
        }
    }
}

/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util.reflect.test;

import ajax.jbc.*;
import ajax.jbc.util.*;
import ajax.util.*;
import ajax.jbc.util.reflect.*;
import java.io.*;
import java.util.*;

public class Test1 {
    public static void main(String[] args) {
        JBCWorld world = new JBCWorld();
        StandardClassLoader loader = new StandardClassLoader(world, "examples\\javafig\\javafig-classes.zip;d:\\programs\\jdk117\\lib\\classes.zip");
        
        System.out.println(loader.getClass("javafig.commands.UndoStack").getField("versionString")
            .isAccessibleTo(loader.getClass("javafig.commands.UndoStack").getInitializerMethod().getContainingClass()));
    }
}

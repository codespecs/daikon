/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver.util;

import ajax.Globals;
import ajax.util.graph.*;
import java.util.*;
import java.io.*;

public class RemoveGlobals {
    public static void main(String[] args) {
        try {
            VardumpTools tools = new VardumpTools(new BufferedReader(new InputStreamReader(System.in)));
            Writer out = new BufferedWriter(new OutputStreamWriter(System.out));
       
            DotWriter.write(out, tools.removeGlobals(), "clusters", new DotGraphFormatter());
            out.flush();
        } catch (IOException ex) {
            System.err.println("Error writing cluster graph: " + ex);
        }
    }
}

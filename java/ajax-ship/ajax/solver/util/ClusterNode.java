/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver.util;

import ajax.Globals;
import ajax.util.graph.*;
import java.util.*;
import java.io.*;

public class ClusterNode implements GraphNode {
    private String name;
    
    public ClusterNode(String name) {
        this.name = name;
    }
    
    public String getName() {
        return name;
    }
}

/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.protocol;

import java.io.*;

public class JGrepRequest extends AnalysisRequest {
    private String expression;
    
    public JGrepRequest(String expr) {
        super("ajax.tools.server.JGrepServer");
        expression = expr;
    }
    
    public String getExpression() {
        return expression;
    }
    
    public String toString() {
        return super.toString() + " (expr=" + expression + ")";
    }
}

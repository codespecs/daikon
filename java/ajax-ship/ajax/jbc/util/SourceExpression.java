/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.jbc.*;

public class SourceExpression {
    private JBCClassLoader loader;
    private String sourceFileName = null;
    private int sourceLineNum = -1;
    private String className = null;
    private String methodName = null;
    private String methodType = null;
    private int methodIndex = -1;
    private String expression = null;
    private int PC = -1;
    
    public SourceExpression(JBCClassLoader loader) {
        this.loader = loader;
    }
    
    public void setSourceFileName(String sourceFileName) {
        this.sourceFileName = sourceFileName;
    }
    
    public void setSourceLineNum(int sourceLineNum) {
        this.sourceLineNum = sourceLineNum;
    }
/**
    public String getSourceFileName() {
        if (sourceFileName != null) {
            return sourceFileName;
        } else if (className != null) {
            loader.getClass(className).getData().
        }
    } */
}

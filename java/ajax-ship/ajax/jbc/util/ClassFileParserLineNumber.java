/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.jbc.LineNumberData;

class ClassFileParserLineNumber implements LineNumberData {
    private int startingPC;
    private int lineNumber;
    
    ClassFileParserLineNumber(int startingPC,
        int lineNumber) {
        this.startingPC = startingPC;
        this.lineNumber = lineNumber;
    }
    
    public int getStartingPC() {
        return startingPC;
    }
    
    public int getLineNumber() {
        return lineNumber;
    }
}

/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.jbc.LocalVariableData;

class ClassFileParserLocalVariable implements LocalVariableData {
    private ClassFileParser parser;
    private int scopeStartingPC;
    private int scopeLength;
    private int varNameCPItem;
    private int varTypeCPItem;
    private int index;
    
    ClassFileParserLocalVariable(ClassFileParser parser,
        int scopeStartingPC, int scopeLength, int varNameCPItem,
        int varTypeCPItem, int index) {
        this.parser = parser;
        this.scopeStartingPC = scopeStartingPC;
        this.scopeLength = scopeLength;
        this.varNameCPItem = varNameCPItem;
        this.varTypeCPItem = varTypeCPItem;
        this.index = index;
    }
    
    public int getScopeStartPC() {
        return scopeStartingPC;
    }
    
    public int getScopeLength() {
        return scopeLength;
    }
    
    public String getVarName() {
        return parser.getStringAt(varNameCPItem);
    }
    
    public String getVarType() {
        return parser.getTypeAt(varNameCPItem);
    }
    
    public int getVarIndex() {
        return index;
    }
}

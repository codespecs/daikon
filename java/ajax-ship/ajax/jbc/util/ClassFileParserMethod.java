/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.jbc.*;

class ClassFileParserMethod implements MethodData {
    private ClassFileParser parser;
    private int accessFlags;
    private int methodNameCPItem;
    private int methodTypeCPItem;
    private int maxStackWords;
    private int maxLocalWords;
    private int codeOffset;
    private int codeAttributesOffset;
    private int catchBlocksOffset;
    private int exceptionsThrownOffset;
    
    ClassFileParserMethod(ClassFileParser parser,
        int accessFlags, int methodNameCPItem, int methodTypeCPItem,
        int maxStackWords, int maxLocalWords, int codeOffset,
        int codeAttributesOffset, int catchBlocksOffset,
        int exceptionsThrownOffset) {
        this.parser = parser;
        this.accessFlags = accessFlags;
        this.methodNameCPItem = methodNameCPItem;
        this.methodTypeCPItem = methodTypeCPItem;
        this.maxStackWords = maxStackWords;
        this.maxLocalWords = maxLocalWords;
        this.codeOffset = codeOffset;
        this.codeAttributesOffset = codeAttributesOffset;
        this.catchBlocksOffset = catchBlocksOffset;
        this.exceptionsThrownOffset = exceptionsThrownOffset;
    }
    
    public int getAccessFlags() {
        return accessFlags;
    }
    
    public String getMethodName() {
        return parser.getStringAt(methodNameCPItem).intern();
    }
    
    public String getMethodType() {
        return parser.getTypeAt(methodTypeCPItem);
    }
    
    public int getMaxStackWords() {
        return maxStackWords;
    }
    
    public int getMaxLocalWords() {
        return maxLocalWords;
    }
    
    public byte[] getCode() {
        return parser.getCode(codeOffset);
    }
    
    public Object getCodeConstant(int index) {
        return parser.getValueAt(index);
    }
    
    public CodeRefData getCodeRef(int index) {
        return parser.getRefAt(index);
    }
    
    public String getCodeClass(int index) {
        return parser.getClassAt(index);
    }
    
    public CatchBlockData[] getCatchBlocks() {
        return parser.getCatchBlocks(catchBlocksOffset);
    }
    
    public String[] getCheckedExceptionsThrown() {
        return parser.getCheckedExceptionsThrown(exceptionsThrownOffset);
    }
    
    public LineNumberData[] getLineNumbers() {
        return parser.getLineNumbers(codeAttributesOffset);
    }
    
    public LocalVariableData[] getLocalVariables() {
        return parser.getLocalVariables(codeAttributesOffset);
    }
}

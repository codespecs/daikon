/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

public interface MethodData {
    public int getAccessFlags();
    public String getMethodName();
    public String getMethodType();
    
    public int getMaxStackWords();
    public int getMaxLocalWords();
    public byte[] getCode();
    public Object getCodeConstant(int index);
    public CodeRefData getCodeRef(int index);
    public String getCodeClass(int index);
    public CatchBlockData[] getCatchBlocks();
    
    public String[] getCheckedExceptionsThrown();
    
    public LineNumberData[] getLineNumbers();
    
    public LocalVariableData[] getLocalVariables();
}

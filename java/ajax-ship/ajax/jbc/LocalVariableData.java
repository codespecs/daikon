/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

public interface LocalVariableData {
    public int getScopeStartPC();
    public int getScopeLength();
    public String getVarName();
    public String getVarType();
    public int getVarIndex();
}

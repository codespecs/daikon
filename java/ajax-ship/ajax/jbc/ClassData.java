/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

/*
This interface, along with the related interfaces FieldData, MethodData,
CatchBlockData, CodeRefData, LineNumberData, and LocalVariableData,
specify how class file information is provided to the JBC analyzer.
The standard implementation is ajax.jbc.util.ClassFileParser, which
obtains all this information by parsing the bytes corresponding to
a .class file, but other implementations are possible if the code 
to be analyzed is really in some other format.

@see ajax.jbc.FieldData
@see ajax.jbc.MethoData
@see ajax.jbc.CatchBlockData
@see ajax.jbc.CodeRefData
@see ajax.jbc.LineNumberData
@see ajax.jbc.LocalVariableData
@see ajax.jbc.util.ClassFileParser
*/
public interface ClassData {
    public int getAccessFlags();
    public String getClassName();
    public String getSuperClassName();
    public String[] getSuperInterfaceNames();
    public FieldData[] getFields();
    public MethodData[] getMethods();
    
    public String getSourceFileName();
}

/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.jbc.*;
import java.io.*;

/*
This class is a utility class that implements the ajax.jbc.ClassData
interface. It takes an array of bytes containing the .class file data
for a JBC class, and parses it into a form acceptable to the Ajax
JBC-analysis subsystem. The parsing is actually done lazily in response
to requests for information.

This class tries to check that the .class data is valid, whenever such
checks can be made efficiently. If a check fails, it throws an
InvalidClassDataError.

@see ajax.jbc.ClassData
*/
public class ClassFileParser implements ClassData, DataConstants {
    private SeekableByteArrayInputStream rawClassFile;
    private DataInputStream classFile;
    private int[] constantPoolOffsets = null;
    private int accessFlags = 0;
    private int classNameCPItem = -1;
    private int superClassNameCPItem = -1;
    private int interfaceListOffset = -1;
    private int fieldsOffset = -1;
    private int methodsOffset = -1;
    private int sourceFileCPItem = 0;

    private MethodData[] methodData = null;
    private FieldData[] fieldData = null;

    public ClassFileParser(byte[] classFile) {
        rawClassFile = new SeekableByteArrayInputStream(classFile);
        this.classFile = new DataInputStream(rawClassFile);

        try {
            prepareClassFile();
        } catch (IOException ex) {
            throw fail("File truncated");
        }
    }

    protected InvalidClassDataError fail(String s) {
        int pos = rawClassFile.getPos();

        if (classNameCPItem > 0) {
            try {
                return new InvalidClassDataError(s
                    + " in class " + getClassName() + " near offset "
                    + pos);
            } catch (InvalidClassDataError ex) {
                return new InvalidClassDataError(s
                    + " in class near offset " + pos);
            }
        } else {
            return new InvalidClassDataError(s
                + " in class near offset " + pos);
        }
    }

    protected InvalidClassDataError failErr(String s) {
        return fail(s);
    }

    protected void skip(int bytes) {
        if (bytes > rawClassFile.available()) {
            bytes = rawClassFile.available();
        }

        rawClassFile.setPos(rawClassFile.getPos() + bytes);
    }

    protected void skipCPIndex(int itemType) throws IOException {
        int index = classFile.readUnsignedShort();

        if (index >= constantPoolOffsets.length) {
            throw fail("Invalid constant pool offset");
        } else {
            int constantItemOffset = constantPoolOffsets[index];

            if (constantItemOffset != 0) {
                int curPos = rawClassFile.getPos();

                rawClassFile.setPos(constantItemOffset);

                int theType = classFile.readUnsignedByte();

                rawClassFile.setPos(curPos);

                if (theType != itemType) {
                    throw fail("Invalid constant pool item type (expected "
                        + itemType + ", got " + theType + ")");
                }
            }
        }
    }

    protected int skipConstantPoolItem() throws IOException {
        int type = classFile.readUnsignedByte();

        switch (type) {
            case CONSTANT_Class:
            case CONSTANT_String:
                skipCPIndex(CONSTANT_Utf8); return 1;
            case CONSTANT_Fieldref:
            case CONSTANT_Methodref:
            case CONSTANT_InterfaceMethodref:
                skipCPIndex(CONSTANT_Class); skipCPIndex(CONSTANT_NameAndType); return 1;
            case CONSTANT_NameAndType:
                skipCPIndex(CONSTANT_Utf8); skipCPIndex(CONSTANT_Utf8); return 1;
            case CONSTANT_Integer:
            case CONSTANT_Float:
                skip(4); return 1;
            case CONSTANT_Long:
            case CONSTANT_Double:
                skip(8); return 2;
            case CONSTANT_Utf8:
                skip(classFile.readUnsignedShort()); return 1;
            default:
                return 1;
        }
    }

    protected void skipAttributeList() throws IOException {
        for (int i = classFile.readUnsignedShort(); i > 0; i--) {
            skip(2);

            skip(classFile.readInt());
        }
    }

    protected void skipFieldItem() throws IOException {
        skip(6);
        skipAttributeList();
    }

    protected void skipMethodItem() throws IOException {
        skip(6);
        skipAttributeList();
    }

    protected void positionAtCPItem(int itemNum, int expectedTag) {
        try {
            rawClassFile.setPos(constantPoolOffsets[itemNum]);

            int tag = classFile.readUnsignedByte();

            if (tag != expectedTag) {
                throw failErr("Constant item has wrong tag (expected " +
                    expectedTag + ", got " + tag + ")");
            }
        } catch (ArrayIndexOutOfBoundsException ex) {
            throw failErr("Invalid constant pool item number: " + itemNum);
        } catch (IOException ex) {
            throw failErr("File truncated");
        }
    }
    
    String getNonnullStringAt(int itemNum) {
        String s = getStringAt(itemNum);
        
        if (s == null) {
            throw failErr("Null string in unexpected place");
        } else {
            return s;
        }
    }

    String getStringAt(int itemNum) {
        if (itemNum == 0) {
            return null;
        } else {
            int curPos = rawClassFile.getPos();

            positionAtCPItem(itemNum, CONSTANT_Utf8);

            try {
                return classFile.readUTF().intern();
            } catch (IOException ex) {
                throw failErr("File truncated");
            } finally {
                rawClassFile.setPos(curPos);
            }
        }
    }

    String getClassAt(int itemNum) {
        if (itemNum == 0) {
            return null;
        } else {
            int curPos = rawClassFile.getPos();

            positionAtCPItem(itemNum, CONSTANT_Class);

            try {
                return getStringAt(classFile.readUnsignedShort()).replace('/', '.').intern();
            } catch (IOException ex) {
                throw failErr("File truncated");
            } finally {
                rawClassFile.setPos(curPos);
            }
        }
    }

    String getTypeAt(int itemNum) {
        if (itemNum == 0) {
            return null;
        } else {
            return getNonnullStringAt(itemNum).replace('/', '.').intern();
        }
    }

    Object getValueAt(int itemNum) {
        if (itemNum == 0) {
            return null;
        } else {
            int curPos = rawClassFile.getPos();

            try {
                rawClassFile.setPos(constantPoolOffsets[itemNum]);

                int tag = classFile.readUnsignedByte();

                switch (tag) {
                    case CONSTANT_String:
                        return getStringAt(classFile.readUnsignedShort());
                    case CONSTANT_Integer:
                        return new Integer(classFile.readInt());
                    case CONSTANT_Float:
                        return new Float(classFile.readFloat());
                    case CONSTANT_Long:
                        return new Long(classFile.readLong());
                    case CONSTANT_Double:
                        return new Double(classFile.readDouble());
                    default:
                        throw failErr("Invalid constant value tag");
                }
            } catch (ArrayIndexOutOfBoundsException ex) {
                throw failErr("Invalid constant pool item number");
            } catch (IOException ex) {
                throw failErr("File truncated");
            } finally {
                rawClassFile.setPos(curPos);
            }
        }
    }

    CodeRefData getRefAt(int itemNum) {
        if (itemNum == 0) {
            return null;
        } else {
            int curPos = rawClassFile.getPos();

            try {
                rawClassFile.setPos(constantPoolOffsets[itemNum]);

                int tag = classFile.readUnsignedByte();

                switch (tag) {
                    case CONSTANT_Fieldref:
                    case CONSTANT_Methodref:
                    case CONSTANT_InterfaceMethodref:
                        int classNameIndex = classFile.readUnsignedShort();

                        positionAtCPItem(classFile.readUnsignedShort(),
                            CONSTANT_NameAndType);

                        int slotNameIndex = classFile.readUnsignedShort();
                        int slotTypeIndex = classFile.readUnsignedShort();

                        return new ClassFileParserRef(this,
                            classNameIndex, slotNameIndex, slotTypeIndex);
                    default:
                        throw failErr("Invalid constant reference tag");
                }
            } catch (ArrayIndexOutOfBoundsException ex) {
                throw failErr("Invalid constant pool item number");
            } catch (IOException ex) {
                throw failErr("File truncated");
            } finally {
                rawClassFile.setPos(curPos);
            }
        }
    }

    byte[] getCode(int offset) {
        if (offset == -1) {
            return null;
        } else {
            try {
                rawClassFile.setPos(offset);

                byte[] code = new byte[classFile.readInt()];

                classFile.readFully(code);

                return code;
            } catch (IOException ex) {
                throw failErr("File truncated");
            }
        }
    }

    CatchBlockData[] getCatchBlocks(int offset) {
        try {
            rawClassFile.setPos(offset);

            CatchBlockData[] catches = new CatchBlockData[classFile.readUnsignedShort()];

            for (int i = 0; i < catches.length; i++) {
                int startPC = classFile.readUnsignedShort();
                int endPC = classFile.readUnsignedShort();
                int handlerPC = classFile.readUnsignedShort();
                int catchType = classFile.readUnsignedShort();

                catches[i] = new ClassFileParserCatchBlock(this,
                    startPC, endPC, handlerPC, catchType);
            }

            return catches;
        } catch (IOException ex) {
            throw failErr("File truncated");
        }
    }

    String[] getCheckedExceptionsThrown(int offset) {
        if (offset == -1) {
            return new String[0];
        } else {
            try {
                rawClassFile.setPos(offset);

                String[] exceptions = new String[classFile.readUnsignedShort()];

                for (int i = 0; i < exceptions.length; i++) {
                    exceptions[i] = getClassAt(classFile.readUnsignedShort());
                }

                return exceptions;
            } catch (IOException ex) {
                throw failErr("File truncated");
            }
        }
    }

    LineNumberData[] getLineNumbers(int offset) {
        try {
            rawClassFile.setPos(offset);

            int numLineNumbers = 0;

            for (int i = classFile.readUnsignedShort(); i > 0; i--) {
                String attributeName = getNonnullStringAt(classFile.readUnsignedShort());

                if (attributeName.equals("LineNumberTable")) {
                    if (classFile.readInt() < 4) {
                        throw failErr("Invalid LineNumberTable attribute length");
                    }

                    int thisAttrLineNumbers = classFile.readUnsignedShort();

                    numLineNumbers += thisAttrLineNumbers;
                    skip(thisAttrLineNumbers*4);
                } else {
                    skip(classFile.readInt());
                }
            }

            LineNumberData[] lineNumbers = new LineNumberData[numLineNumbers];

            numLineNumbers = 0;
            rawClassFile.setPos(offset);
            for (int i = classFile.readUnsignedShort(); i > 0; i--) {
                String attributeName = getNonnullStringAt(classFile.readUnsignedShort());

                if (attributeName.equals("LineNumberTable")) {
                    classFile.readInt();
                    
                    for (int j = classFile.readUnsignedShort(); j > 0; j--) {
                        int startPC = classFile.readUnsignedShort();
                        int lineNum = classFile.readUnsignedShort();

                        lineNumbers[numLineNumbers] =
                            new ClassFileParserLineNumber(startPC, lineNum);
                        numLineNumbers++;
                    }
                } else {
                    skip(classFile.readInt());
                }
            }

            return lineNumbers;
        } catch (IOException ex) {
            throw failErr("File truncated");
        }
    }

    LocalVariableData[] getLocalVariables(int offset) {
        try {
            rawClassFile.setPos(offset);

            int numLocalVariables = 0;

            for (int i = classFile.readUnsignedShort(); i > 0; i--) {
                String attributeName = getNonnullStringAt(classFile.readUnsignedShort());

                if (attributeName.equals("LocalVariableTable")) {
                    if (classFile.readInt() < 4) {
                        throw failErr("Invalid LocalVariableTable attribute length");
                    }

                    int thisAttrLocalVariables = classFile.readUnsignedShort();

                    numLocalVariables += thisAttrLocalVariables;
                    skip(thisAttrLocalVariables*10);
                } else {
                    skip(classFile.readInt());
                }
            }

            LocalVariableData[] localVariables = new LocalVariableData[numLocalVariables];

            numLocalVariables = 0;
            rawClassFile.setPos(offset);
            for (int i = classFile.readUnsignedShort(); i > 0; i--) {
                String attributeName = getNonnullStringAt(classFile.readUnsignedShort());

                if (attributeName.equals("LocalVariableTable")) {
                    classFile.readInt();
                        
                    for (int j = classFile.readUnsignedShort(); j > 0; j--) {
                        int startPC = classFile.readUnsignedShort();
                        int length = classFile.readUnsignedShort();
                        int nameIndex = classFile.readUnsignedShort();
                        int descriptorIndex = classFile.readUnsignedShort();
                        int index = classFile.readUnsignedShort();

                        localVariables[numLocalVariables] =
                            new ClassFileParserLocalVariable(this,
                                startPC, length, nameIndex, descriptorIndex, index);
                        numLocalVariables++;
                    }
                } else {
                    skip(classFile.readInt());
                }
            }

            return localVariables;
        } catch (IOException ex) {
            throw failErr("File truncated");
        }
    }

    protected void prepareClassFile() throws IOException {
        if (classFile.readInt() != 0xCAFEBABE) {
            throw fail("Invalid magic number");
        }

        if (classFile.readUnsignedShort() > 3) {
            throw fail("Unknown class file minor version");
        }

        if (classFile.readUnsignedShort() != 45) {
            throw fail("Unknown class file major version");
        }

        constantPoolOffsets = new int[classFile.readUnsignedShort()];
        for (int i = 1; i < constantPoolOffsets.length;) {
            constantPoolOffsets[i] = rawClassFile.getPos();
/* Since a constant pool item can actually occupy more than one constant pool
   "slot", the number of slots occupied must be obtained from the return value.
   See the JVM Reference, section 4.4.5. Those Sun people have a lot to answer
   for. */
            i += skipConstantPoolItem();
        }

        accessFlags = classFile.readUnsignedShort();
/* The JVM Spec (section 4.1) says, fairly enough, that an interface must
   be abstract. However, lots of Sun-generated .class files violate this
   rule. So, we patch them up here. */
        if ((accessFlags & ACC_INTERFACE) != 0) {
            accessFlags |= ACC_ABSTRACT;
        }
     
        classNameCPItem = classFile.readUnsignedShort();
        superClassNameCPItem = classFile.readUnsignedShort();

        interfaceListOffset = rawClassFile.getPos();
        skip(classFile.readUnsignedShort()*2);

        fieldsOffset = rawClassFile.getPos();
        for (int i = classFile.readUnsignedShort(); i > 0; i--) {
            skipFieldItem();
        }

        methodsOffset = rawClassFile.getPos();
        for (int i = classFile.readUnsignedShort(); i > 0; i--) {
            skipMethodItem();
        }

        for (int i = classFile.readUnsignedShort(); i > 0; i--) {
            String attributeName = getNonnullStringAt(classFile.readUnsignedShort());

            if (attributeName.equals("SourceFile")) {
                if (classFile.readInt() != 2) {
                    throw fail("Invalid SourceFile attribute length");
                }
                sourceFileCPItem = classFile.readUnsignedShort();
            } else {
                skip(classFile.readInt());
            }
        }
    }

    public int getAccessFlags() {
        return accessFlags;
    }

    public String getClassName() {
        return getClassAt(classNameCPItem);
    }

    public String getSuperClassName() {
        return getClassAt(superClassNameCPItem);
    }

    public String[] getSuperInterfaceNames() {
        rawClassFile.setPos(interfaceListOffset);

        try {
            String[] interfaces = new String[classFile.readUnsignedShort()];

            for (int i = 0; i < interfaces.length; i++) {
                interfaces[i] = getClassAt(classFile.readUnsignedShort());
            }

            return interfaces;
        } catch (IOException ex) {
            throw failErr("File truncated");
        }
    }

    public FieldData[] getFields() {
        if (fieldData != null) {
            return fieldData;
        } else {
            try {
                rawClassFile.setPos(fieldsOffset);

                FieldData[] fields = new FieldData[classFile.readUnsignedShort()];

                for (int i = 0; i < fields.length; i++) {
                    int accessFlags = classFile.readUnsignedShort();
                    int nameIndex = classFile.readUnsignedShort();
                    int descriptorIndex = classFile.readUnsignedShort();
                    int constantValueIndex = 0;

                    for (int j = classFile.readUnsignedShort(); j > 0; j--) {
                        if (getNonnullStringAt(classFile.readUnsignedShort()).
                            equals("ConstantValue")) {
                            if (classFile.readInt() != 2) {
                                throw failErr("Invalid ConstantValue attribute length");
                            }
                            constantValueIndex = classFile.readUnsignedShort();
                        } else {
                            skip(classFile.readInt());
                        }
                    }

                    fields[i] = new ClassFileParserField(this,
                        accessFlags, nameIndex, descriptorIndex, constantValueIndex);
                }

                fieldData = fields;

                return fields;
            } catch (IOException ex) {
                throw failErr("File truncated");
            }
        }
    }

    public MethodData[] getMethods() {
        if (methodData != null) {
            return methodData;
        } else {
            try {
                rawClassFile.setPos(methodsOffset);

                MethodData[] methods = new MethodData[classFile.readUnsignedShort()];

                for (int i = 0; i < methods.length; i++) {
                    int accessFlags = classFile.readUnsignedShort();
                    int nameIndex = classFile.readUnsignedShort();
                    int descriptorIndex = classFile.readUnsignedShort();
                    int maxStackWords = -1;
                    int maxLocalWords = -1;
                    int codeOffset = -1;
                    int codeAttributesOffset = -1;
                    int catchBlocksOffset = -1;
                    int exceptionsThrownOffset = -1;

                    for (int j = classFile.readUnsignedShort(); j > 0; j--) {
                        String attributeName = getNonnullStringAt(classFile.readUnsignedShort());

                        if (attributeName.equals("Code")) {
                            int codeAttributeLength = classFile.readInt();
                            int codeAttributeEnd = rawClassFile.getPos() + codeAttributeLength;

                            if (codeAttributeLength < 13) {
                                throw failErr("Invalid Code attribute length");
                            }

                            if (codeOffset != -1) {
                                throw failErr("Duplicate Code attributes found");
                            }

                            maxStackWords = classFile.readUnsignedShort();
                            maxLocalWords = classFile.readUnsignedShort();
                            codeOffset = rawClassFile.getPos();
                            skip(classFile.readInt());
                            catchBlocksOffset = rawClassFile.getPos();
                            skip(classFile.readUnsignedShort()*8);
                            codeAttributesOffset = rawClassFile.getPos();

                            rawClassFile.setPos(codeAttributeEnd);
                        } else if (attributeName.equals("Exceptions")) {
                            int exceptionsAttributeLength = classFile.readInt();
                            int exceptionsAttributeEnd = rawClassFile.getPos() + exceptionsAttributeLength;

                            if (exceptionsAttributeLength < 2) {
                                throw failErr("Invalid Code attribute length");
                            }

                            if (exceptionsThrownOffset != -1) {
                                throw failErr("Duplicate Code attributes found");
                            }

                            exceptionsThrownOffset = rawClassFile.getPos();

                            rawClassFile.setPos(exceptionsAttributeEnd);
                        } else {
                            skip(classFile.readInt());
                        }
                    }

                    if ((accessFlags & (ACC_NATIVE | ACC_ABSTRACT)) == 0 && codeOffset == -1) {
                        throw failErr("No Code attribute found");
                    }
                    
                    methods[i] = new ClassFileParserMethod(this,
                        accessFlags, nameIndex, descriptorIndex, maxStackWords,
                        maxLocalWords, codeOffset, codeAttributesOffset,
                        catchBlocksOffset, exceptionsThrownOffset);
                }

                methodData = methods;

                return methods;
            } catch (IOException ex) {
                throw failErr("File truncated");
            }
        }
    }

    public String getSourceFileName() {
        return getStringAt(sourceFileCPItem);
    }
}

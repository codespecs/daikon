/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.jbc.FieldData;

class ClassFileParserField implements FieldData {
    private ClassFileParser parser;
    private int accessFlags;
    private int fieldNameCPItem;
    private int fieldTypeCPItem;
    private int constantValueCPItem;

    ClassFileParserField(ClassFileParser parser, int accessFlags,
        int fieldNameCPItem, int fieldTypeCPItem, int constantValueCPItem) {
        this.parser = parser;
        this.accessFlags = accessFlags;
        this.fieldNameCPItem = fieldNameCPItem;
        this.fieldTypeCPItem = fieldTypeCPItem;
        this.constantValueCPItem = constantValueCPItem;
    }
    
    public int getAccessFlags() {
        return accessFlags;
    }
    
    public String getFieldName() {
        return parser.getStringAt(fieldNameCPItem).intern();
    }
    
    public String getFieldType() {
        return parser.getTypeAt(fieldTypeCPItem);
    }
    
    public Object getInitialValue() {
        return parser.getValueAt(constantValueCPItem);
    }
}

/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.jbc.CodeRefData;

class ClassFileParserRef implements CodeRefData {
    private ClassFileParser parser;
    private int classNameCPItem;
    private int slotNameCPItem;
    private int slotTypeCPItem;
    
    ClassFileParserRef(ClassFileParser parser,
        int classNameCPItem, int slotNameCPItem, int slotTypeCPItem) {
        this.parser = parser;
        this.classNameCPItem = classNameCPItem;
        this.slotNameCPItem = slotNameCPItem;
        this.slotTypeCPItem = slotTypeCPItem;
    }
        
    public String getClassName() {
        return parser.getClassAt(classNameCPItem);
    }
    
    public String getSlotName() {
        return parser.getStringAt(slotNameCPItem);
    }
    
    public String getSlotType() {
        return parser.getTypeAt(slotTypeCPItem);
    }
}

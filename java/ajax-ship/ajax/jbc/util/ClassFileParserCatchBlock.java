/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.jbc.CatchBlockData;

class ClassFileParserCatchBlock implements CatchBlockData {
    private ClassFileParser parser;
    private int startPC;
    private int endPC;
    private int handlerPC;
    private int catchTypeCPItem;
    
    ClassFileParserCatchBlock(ClassFileParser parser,
        int startPC, int endPC, int handlerPC,
        int catchTypeCPItem) {
        this.parser = parser;
        this.startPC = startPC;
        this.endPC = endPC;
        this.handlerPC = handlerPC;
        this.catchTypeCPItem = catchTypeCPItem;
    }
    
    public int getStartPC() {
        return startPC;
    }
    
    public int getEndPC() {
        return endPC;
    }
    
    public int getHandlerPC() {
        return handlerPC;
    }
    
    public String getCatchType() {
        return parser.getClassAt(catchTypeCPItem);
    }
}

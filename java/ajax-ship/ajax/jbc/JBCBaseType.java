/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

public class JBCBaseType extends JBCType {
    private String s;
    private int wordSize;
    private boolean weaklyEqualsInt;
    
    public JBCBaseType(String name) {
        this(name, 1, false);
    }
    
    public JBCBaseType(String name, boolean weaklyEqualsInt) {
        this(name, 1, weaklyEqualsInt);
    }
    
    public JBCBaseType(String name, int wordSize, boolean weaklyEqualsInt) {
        this.s = name;
        this.wordSize = wordSize;
        this.weaklyEqualsInt = weaklyEqualsInt;
    }
    
    public JBCBaseType(String name, int wordSize) {
        this(name, wordSize, false);
    }
    
    public String getName() {
        return s;
    }
    
    public int getWordSize() {
        return wordSize;
    }
    
    public boolean isEqualType(JBCType t) {
        if (t == this) {
            return true;
        } else if (t instanceof JBCBaseType && weaklyEqualsInt) {
            JBCBaseType base = (JBCBaseType)t;
            
            return base.weaklyEqualsInt;
        } else {
            return false;
        }
    }
    
    public String toString() {
        return s;
    }
}

/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.protocol;

import ajax.tools.protocol.*;
import javax.swing.tree.*;
import java.awt.Color;

public class JGrepUpdate implements java.io.Serializable {
    private int type;
    private String member;
    private LocationDescriptor loc;
    
    public static final int NEW = 0;   // member is the class
    public static final int CALL = 1;  // member is the method name
    public static final int READ = 2;  // member is the field name
    public static final int WRITE = 3; // member is the field name
    
    public JGrepUpdate(int type, String member, LocationDescriptor loc) {
        this.type = type;
        this.member = member;
        this.loc = loc;
    }
    
    public int getType() {
        return type;
    }
    
    public LocationDescriptor getLocation() {
        return loc;
    }

    public String getMember() {
        return member;
    }

    public String getTypeString() {
      switch (getType()) {
      case NEW: return "NEW";
      case CALL: return "CALL";
      case READ: return "READ";
      case WRITE: return "WRITE";
      default: return "<unknown>";
      }
    }

    public String toString() {
        return "JGrepUpdate: type=" + getTypeString() + ", member=" + member + ", loc="
            + loc;
    }
}

/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import java.io.ByteArrayInputStream;
import ajax.Globals;

public class SeekableByteArrayInputStream extends ByteArrayInputStream {
    public SeekableByteArrayInputStream(byte[] data) {
        super(data);
    }
    
    public void setPos(int offset) {
        if (offset < 0 || offset > buf.length) {
            throw Globals.nonlocalError("Invalid offset: " + offset);
        }
        
        pos = offset;
    }
    
    public int getPos() {
        return pos;
    }
}

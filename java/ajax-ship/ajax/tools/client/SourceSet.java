/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.client;

import ajax.tools.protocol.*;
import java.util.*;
import java.awt.Color;

public class SourceSet {
    private Hashtable classesToSources = new Hashtable();
    private MessagePort port;
    
    public SourceSet(MessagePort port) {
        this.port = port;
    }
    
    public void showSource(ClassDescriptor c, int lineNum, String textToSelect,
        Color selectionColor) {
        SourceState source = (SourceState)classesToSources.get(c);
        
        if (source == null) {
            source = new SourceState(c);
            classesToSources.put(c, source);
            port.sendMessage(c);
        }
        source.setSelection(lineNum, textToSelect, selectionColor);
    }
    
    public void setText(ClassDescriptor c, String text) {
        SourceState source = (SourceState)classesToSources.get(c);
        
        if (source != null) {
            if (text == null) {
                source.loadText("<Source unavailable>", false);
            } else {
                source.loadText(text, true);
            }
        }
    }
}

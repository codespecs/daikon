/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.client;

import ajax.tools.protocol.*;
import java.awt.*;
import java.awt.event.*;
import java.applet.*;
import java.rmi.*;
import java.rmi.registry.*;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.border.*;
import javax.swing.text.*;
import ajax.util.*;

class SourceState {
    private ClassDescriptor classDescriptor;
    private JTextArea widget = null;
    private JFrame frame = null;
    private int lineToSelect = -1;
    private String textToSelect = null;
    private Color selectionColor = null;
    private boolean textIsSource = false;
    private String text = "Loading...";
    
/**
Runs in UI thread.
*/
    SourceState(ClassDescriptor c) {
        classDescriptor = c;
        updateWidget();
    }

    private void updateWidget() {
        if (frame == null) {
            frame = new JFrame("Source for " + classDescriptor.getClassName());

            frame.addWindowListener(new WindowAdapter() {
    	            public void windowClosed(WindowEvent e) {
                        frame = null;
		    }
                });
            frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);

            widget = new JTextArea();
            widget.setEditable(false);
            frame.setContentPane(new JScrollPane(widget));
            frame.pack();
            frame.setSize(600, 400);
            frame.show();
	}

        widget.setText(text);
        widget.setFont(
            new Font("Courier", textIsSource ? Font.PLAIN : Font.ITALIC, 12));

        if (textIsSource && lineToSelect >= 0) {
            try {
                int start = widget.getLineStartOffset(lineToSelect - 1);
                int end = widget.getLineEndOffset(lineToSelect - 1);

                if (textToSelect != null) {
                    String s = widget.getText(start, end - start);

                    int firstMatch = s.indexOf(textToSelect);

                    if (firstMatch >= 0
                        && firstMatch == s.lastIndexOf(textToSelect)) {
                        start = firstMatch + start;
                        end = start + textToSelect.length();
                    }
                }

                if (selectionColor != null) {
                    widget.setCaretPosition(start);
                    widget.moveCaretPosition(end);
                    widget.setSelectionColor(selectionColor);
		}
                
                Rectangle r = widget.modelToView(start);
                
                if (r != null) {
                    widget.scrollRectToVisible(r);
                }
            } catch (BadLocationException ex) {
            }

            frame.show();
            frame.toFront();
        }
    }
    
/**
Runs in UI thread.
*/
    void setSelection(int lineToSelect, String textToSelect, Color selectionColor) {
        this.lineToSelect = lineToSelect;
        this.textToSelect = textToSelect;
        this.selectionColor = selectionColor;
        
        updateWidget();
    }
    
/**
Runs in UI thread.
*/
    void loadText(String text, boolean textIsSource) {
        this.textIsSource = textIsSource;
        this.text = text;
        
        updateWidget();
    }
}

/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.client;

import ajax.tools.protocol.*;
import javax.swing.*;
import javax.swing.tree.*;
import java.awt.*;
import java.util.*;
import ajax.tools.client.*;
import java.awt.event.*;
import java.net.*;

public class ClassBrowserClient
    implements FrontEnd, MouseListener, ScrollPaneConstants, MessageHandler {
    private Client client;
    private MessagePort port;
    private JFrame window;
    private JList list;
    private DefaultListModel listModel;
    private SourceSet sourceSet;
    private Hashtable classNameMap = new Hashtable();
    
    public ClassBrowserClient() {
    }
    
    private void showDeath(String s) {
        window.getContentPane().removeAll();
        window.getContentPane().setLayout(new BorderLayout());

        JTextArea text = new JTextArea(s);

        text.setEditable(false);

        JPanel holder = new JPanel();

        holder.add(text);

        window.getContentPane().add("South", holder);
        window.pack();
        window.show();
    }
        
    public void init(Client client, String serverName, MessagePort port) {
        this.client = client;
        this.port = port;
        
        sourceSet = new SourceSet(port);
        window = new JFrame("Classes in " + serverName);
        window.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
            
        port.sendMessage(new AnalysisRequest("ajax.tools.server.ClassBrowserServer"));
            
        listModel = new DefaultListModel();
        list = new JList(listModel);
        list.addMouseListener(this);
        list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        showList();
    }
    
    private void showList() {
        window.getContentPane().removeAll();
        window.getContentPane().setLayout(new BorderLayout());
        JScrollPane scroller = new JScrollPane(list);
        scroller.setPreferredSize(new Dimension(400, 300));
        window.getContentPane().add("Center", scroller);
        window.pack();
        window.show();
    }
    
    private static int compareClasses(String p1, String p2) {
        int p1Dot = p1.indexOf('.');
        int p2Dot = p2.indexOf('.');
        
        if (p1Dot < 0) {
            if (p2Dot < 0) {
                return p1.compareTo(p2);
            } else {
                return -1;
            }
        } else if (p2Dot < 0) {
            return 1;
        } else {
            int preCmp = p1.substring(0, p1Dot).compareTo(p2.substring(0, p2Dot));
            
            if (preCmp != 0) {
                return preCmp;
            } else {
                return compareClasses(p1.substring(p1Dot + 1), p2.substring(p2Dot + 1));
            }
        }
    }
    
    public void handleMessage(final MessagePort port, final Object o) {
        SwingUtilities.invokeLater(new Runnable() {
                public void run() { handleMessageEvent(port, o); }
            });
    }
    
    private void handleMessageEvent(MessagePort port, Object o) {
        if (o instanceof ClassDescriptor) {
            handleUpdate((ClassDescriptor)o);
        } else if (o instanceof SourceResponse) {
            SourceResponse r = (SourceResponse)o;
            
            sourceSet.setText(r.getForClass(), r.getText());
        } else if (o instanceof PortStatusMsg) {
        } else if (o instanceof PortErrorMsg) {
            showDeath("Communications error: " + o);
        } else {
            showDeath("Unknown message: o");
        }
    }
    
    private void handleUpdate(ClassDescriptor c) {
        String cName = c.getClassName();
        String sourceName = c.getSourceFileName();
        String n = sourceName != null ? cName + " (" + sourceName + ")" : cName;
        
        classNameMap.put(n, c);

        int insertAt = listModel.getSize();
        for (int i = 0; i < listModel.getSize(); i++) {
            String s = (String)listModel.elementAt(i);
            int sNameSpace = s.indexOf(' ');
            String sName = sNameSpace >= 0 ? s.substring(0, sNameSpace) : s;
            //if (sName.compareTo(cName) > 0) {
	               if (compareClasses(sName, cName) > 0) {
                insertAt = i;
                System.err.println("Inserting " + cName + " at index " + i);
                break;
	    }
	}
        listModel.insertElementAt(n, insertAt);
    }
    
    public void mouseClicked(MouseEvent e) {
        if (e.getClickCount() == 2) {
            Object selection = list.getSelectedValue();

            if (selection != null) {
                ClassDescriptor c = (ClassDescriptor)classNameMap.get(selection);

                if (c != null) {
                    sourceSet.showSource(c, 1, null, null);
                }
            }
        }
    }

    public void mouseEntered(MouseEvent e) {
    }

    public void mouseExited(MouseEvent e) {
    }

    public void mousePressed(MouseEvent e) {
    }

    public void mouseReleased(MouseEvent e) {
    }
}

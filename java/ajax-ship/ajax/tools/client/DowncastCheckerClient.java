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

public class DowncastCheckerClient
    implements FrontEnd, MouseListener, ScrollPaneConstants, TreeCellRenderer,
        MessageHandler, ComponentListener {
    private Client client;
    private MessagePort port;
    private JFrame window;
    private JTree tree;
    private DefaultTreeModel treeModel;
    private Hashtable locationStates = new Hashtable();
    private DefaultMutableTreeNode treeRoot;
    private DefaultTreeCellRenderer renderer = new DefaultTreeCellRenderer();
    private Icon[] icons = new Icon[iconNames.length];
    private SourceSet sourceSet;
    
    private static final String[] iconNames = { "unknown.gif", "safe.gif", "unsafe.gif" };
    
    private static final String DEFAULT_PACKAGE = "Default package";
    
    public DowncastCheckerClient() {
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
        window = new JFrame("Downcasts for " + serverName);
        window.addComponentListener(this);
            
        port.sendMessage(new AnalysisRequest("ajax.tools.server.DowncastCheckerServer"));
            
        treeRoot = new DefaultMutableTreeNode();
        treeModel = new DefaultTreeModel(treeRoot);
        tree = new JTree(treeModel);
        tree.setRootVisible(false);
        tree.setShowsRootHandles(true);
        tree.setCellRenderer(this);
        tree.addMouseListener(this);
        showTree();
    }
    
    private void showTree() {
        for (int i = 0; i < iconNames.length; i++) {
            try {
                icons[i] = new ImageIcon(new URL(client.getDocumentBase(), iconNames[i]));
            } catch (MalformedURLException ex) {
            }
        }
        
        window.getContentPane().removeAll();
        window.getContentPane().setLayout(new BorderLayout());
        JScrollPane scroller = new JScrollPane(tree);
        scroller.setPreferredSize(new Dimension(400, 300));
        window.getContentPane().add("Center", scroller);
        window.pack();
        window.show();
    }
    
    public Component getTreeCellRendererComponent(JTree tree, Object value,
        boolean sel, boolean expanded, boolean leaf, int row, boolean hasFocus) {
        JLabel result = (JLabel)renderer.getTreeCellRendererComponent(tree, value,
            sel, expanded, leaf, row, hasFocus);
        
        if (value instanceof DefaultMutableTreeNode) {
            Object userObject = ((DefaultMutableTreeNode)value).getUserObject();
            
            result.setIcon(null);
            if (userObject instanceof LocationDescriptor) {
                LocationDescriptor location = (LocationDescriptor)userObject;
                DowncastState state = (DowncastState)locationStates.get(location);
                String text;
                String sourceFileName = location.getMethodDescriptor().getClassDescriptor()
                    .getSourceFileName();
                int lineNumber = location.getLineNumber();
                    
                if (sourceFileName != null && lineNumber >= 0) {
                    text = sourceFileName + ":" + lineNumber;
                } else {
                    text = "Bytecode offset " + location.getOffset();
                }
                    
                result.setText("To " + state.getBound().getClassName() + " (" + text + ")");
                result.setIcon(icons[state.getState()]);
            } else if (userObject instanceof MethodDescriptor) {
                result.setText(((MethodDescriptor)userObject).getMethodName());
            }
        }
        
        return result;
    }
    
    private DefaultMutableTreeNode insertChildAndNotify(DefaultMutableTreeNode parent,
        DefaultMutableTreeNode child, int pos) {
        treeModel.insertNodeInto(child, parent, pos);
        treeModel.reload(parent);
        return child;
    }
    
    private DefaultMutableTreeNode getLocationNode(DefaultMutableTreeNode parent,
        LocationDescriptor location) {
        int insertAt = parent.getChildCount();
        
        for (int i = parent.getChildCount() - 1; i >= 0; i--) {
            DefaultMutableTreeNode child = (DefaultMutableTreeNode)parent.getChildAt(i);
            LocationDescriptor childLoc = (LocationDescriptor)child.getUserObject();
            
            if (childLoc.equals(location)) {
                return child;
            } else if (childLoc.getOffset() > location.getOffset()) {
                insertAt = i;
            }
        }
        
        return insertChildAndNotify(parent, new DefaultMutableTreeNode(location), insertAt);
    }
    
    private DefaultMutableTreeNode getMethodNode(DefaultMutableTreeNode parent,
        MethodDescriptor method) {
        int insertAt = parent.getChildCount();
        
        for (int i = parent.getChildCount() - 1; i >= 0; i--) {
            DefaultMutableTreeNode child = (DefaultMutableTreeNode)parent.getChildAt(i);
            MethodDescriptor childMethod = (MethodDescriptor)child.getUserObject();
            
            if (childMethod.equals(method)) {
                return child;
            } else if (childMethod.getMethodName().compareTo(method.getMethodName()) > 0) {
                insertAt = i;
            }
        }
        
        return insertChildAndNotify(parent, new DefaultMutableTreeNode(method), insertAt);
    }
    
    private DefaultMutableTreeNode getClassNode(DefaultMutableTreeNode parent,
        String className) {
        int insertAt = parent.getChildCount();
        
        for (int i = parent.getChildCount() - 1; i >= 0; i--) {
            DefaultMutableTreeNode child = (DefaultMutableTreeNode)parent.getChildAt(i);
            String childName = (String)child.getUserObject();
            
            if (childName.equals(className)) {
                return child;
            } else if (childName.compareTo(className) > 0) {
                insertAt = i;
            }
        }
        
        return insertChildAndNotify(parent, new DefaultMutableTreeNode(className), insertAt);
    }
    
/**
Returns a positive value if p1 comes after p2.
*/
    private static int comparePackages(String p1, String p2) {
        if (p1.equals(DEFAULT_PACKAGE)) {
            return -1;
        } else if (p2.equals(DEFAULT_PACKAGE)) {
            return 1;
        }
        
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
                return comparePackages(p1.substring(p1Dot + 1), p2.substring(p2Dot + 1));
            }
        }
    }
    
    private DefaultMutableTreeNode getPackageNode(DefaultMutableTreeNode parent,
        String packageName) {
        int insertAt = parent.getChildCount();
        
        for (int i = parent.getChildCount() - 1; i >= 0; i--) {
            DefaultMutableTreeNode child = (DefaultMutableTreeNode)parent.getChildAt(i);
            String childPackage = (String)child.getUserObject();
            
            if (childPackage.equals(packageName)) {
                return child;
            } else if (comparePackages(childPackage, packageName) > 0) {
                insertAt = i;
            }
        }
        
        return insertChildAndNotify(parent, new DefaultMutableTreeNode(packageName), insertAt);
    }
    
    private void updateTreeNode(LocationDescriptor location) {
        String className = location.getMethodDescriptor().getClassDescriptor().getClassName();
        int lastDot = className.lastIndexOf('.');
        String packageName;
        
        if (lastDot >= 0) {
            packageName = className.substring(0, lastDot);
            className = className.substring(lastDot + 1);
        } else {
            packageName = DEFAULT_PACKAGE;
        }
        
        DefaultMutableTreeNode methodNode = getMethodNode(getClassNode(getPackageNode(treeRoot, packageName), className),
                location.getMethodDescriptor());
        DefaultMutableTreeNode locationNode = getLocationNode(methodNode, location);
        
        tree.expandPath(new TreePath(methodNode.getPath()));
        
        treeModel.nodeChanged(locationNode);
    }
    
    private DowncastState makeState(LocationDescriptor location, ClassDescriptor declaredBound) {
        DowncastState state = (DowncastState)locationStates.get(location);
        
        if (state == null) {
            state = new DowncastState(declaredBound);
            locationStates.put(location, state);
        }
        
        return state;
    }
    
    public void handleMessage(final MessagePort port, final Object o) {
        SwingUtilities.invokeLater(new Runnable() {
                public void run() { handleMessageEvent(port, o); }
            });
    }
    
    private void handleMessageEvent(MessagePort port, Object o) {
        if (o instanceof DowncastCheckerUpdate) {
            handleUpdate((DowncastCheckerUpdate)o);
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
    
    private void handleUpdate(DowncastCheckerUpdate update) {
        LocationDescriptor location = update.getLocation();
        DowncastState state = makeState(location, update.getActualBound());
        
        switch (update.getState()) {
            case DowncastCheckerUpdate.UNKNOWN:
                state.setUnknown();
                break;
            case DowncastCheckerUpdate.SAFE:
                state.setSafe(update.getActualBound());
                break;
            case DowncastCheckerUpdate.UNSAFE:
                state.setUnsafe();
                break;
        }
        
        updateTreeNode(location);
    }
    
    public void mouseClicked(MouseEvent e) {
        if (e.getClickCount() == 2) {
            TreePath path = tree.getPathForLocation(e.getX(), e.getY());
            
            if (path != null) {
                Object lastComponent = path.getLastPathComponent();
                
                if (lastComponent instanceof DefaultMutableTreeNode) {
                    Object userObject = ((DefaultMutableTreeNode)lastComponent).getUserObject();
                    
                    if (userObject instanceof LocationDescriptor) {
                        LocationDescriptor loc = (LocationDescriptor)userObject;
                        DowncastState state = (DowncastState)locationStates.get(loc);
                        Color color = state.getColor();
                        
                        if (color == null) {
                            color = UIManager.getColor("textHighlight");
                            if (color == null) {
                                color = Color.magenta;
                            }
                        }
                        
                        sourceSet.showSource(loc.getMethodDescriptor().getClassDescriptor(),
                            loc.getLineNumber(), "(" + state.getBound().getClassName() + ")",
                            color);
                    }
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

    public void componentHidden(ComponentEvent e) {
        window.dispose();
        port.dispose();
    }

    public void componentMoved(ComponentEvent e) {
    }

    public void componentResized(ComponentEvent e) {
    }

    public void componentShown(ComponentEvent e) {
    }
}

/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.client;

import ajax.tools.protocol.*;
import java.awt.*;
import java.awt.event.*;
import java.applet.*;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.border.*;
import javax.swing.text.*;
import ajax.util.*;
import ajax.tools.misc.*;

public class Client extends JApplet
    implements ActionListener, ListSelectionListener, SwingConstants,
        MessageHandler, Runnable {
    private static final Hashtable tools = makeToolTable();
    
    private boolean active = false;
    private boolean showingContent = false;
    private JButton starter;
    private JList codeList;
    private JList toolList;
    private Hashtable programsToPorts = new Hashtable();
    private Thread registryPoller;
    
    private static Hashtable makeToolTable() {
        Hashtable table = new Hashtable();

        table.put("Browse source code", "ajax.tools.client.ClassBrowserClient");
        table.put("Check downcasts for safety", "ajax.tools.client.DowncastCheckerClient");
        table.put("Search for object references", "ajax.tools.client.JGrepClient");

        return table;
    }
    
    public void handleMessage(final MessagePort port, final Object o) {
        SwingUtilities.invokeLater(new Runnable() {
                public void run() { handleMessageEvent(port, o); }
            });
    }
    
    private void handleMessageEvent(MessagePort port, Object o) {
        if (o instanceof ServerData[]) {
            updateCodeList((ServerData[])o);
        } else if (o instanceof PortErrorMsg) {
            showDeath("Connection died: " + o);
        } else if (o instanceof PortStatusMsg) {
        } else {
            showDeath("Unknown message: " + o);
        }
    }

    private void showDeath(String s) {
        getContentPane().removeAll();
        getContentPane().setLayout(new BorderLayout());

        JTextArea text = new JTextArea(s);

        text.setEditable(false);

        JPanel holder = new JPanel();

        holder.add(text);

        getContentPane().add("South", holder);
        validate();
    }
    
    public void actionPerformed(ActionEvent e) {
        if (e.getSource() == starter) {
            String toolClass = (String)tools.get((String)toolList.getSelectedValue());
            String programName = (String)codeList.getSelectedValue();
            int portNum = ((Integer)programsToPorts.get(programName)).intValue();

            try {
                FrontEnd fe = (FrontEnd)Class.forName(toolClass).newInstance();
                
                fe.init(this, programName,
                    new SocketMessagePort(fe, getHostName(), portNum));
            } catch (ClassNotFoundException ex) {
                showDeath("Front end not found: " + ex.getMessage());
            } catch (IllegalAccessException ex) {
                showDeath("Front end not accessible: " + ex.getMessage());
            } catch (InstantiationException ex) {
                showDeath("Front end not usable: " + ex.getMessage());
            } catch (ClassCastException ex) {
                showDeath("Front end invalid: " + ex.getMessage());
            }
        }
    }
    
    public void valueChanged(ListSelectionEvent e) {
        starter.setEnabled(
            toolList != null && toolList.getSelectedValue() != null
            && codeList != null && codeList.getSelectedValue() != null);
    }

    public static Window getComponentWindow(Component c) {
        while (c != null) {
            if (c instanceof Window) {
                return (Window)c;
            }
            c = c.getParent();
        }
        return null;
    }

/**
This method must run in the UI thread.
*/
    private void updateCodeList(ServerData[] programList) {
        if (!showingContent) {
            showContent();
            showingContent = true;
        }
        
        programsToPorts = new Hashtable();
        String[] names = new String[programList.length];
        
        for (int i = 0; i < programList.length; i++) {
            String title = programList[i].getProgramTitle();
            
            names[i] = title;
            programsToPorts.put(title, new Integer(programList[i].getServerPort()));
        }
        
        String programName = (String)codeList.getSelectedValue();

        StringSorter.sort(names);
        codeList.setListData(names);
        if (programName != null) {
            codeList.setSelectedValue(programName, true);
        }
    }

    private String getHostName() {
        String host = getParameter("server");

        if (host != null) {
            return host;
        } else {
            return getDocumentBase().getHost();
        }
    }

    private void showContent() {
        getContentPane().removeAll();
        getContentPane().setLayout(new BorderLayout());

        starter = new JButton("Start Tool");
        starter.addActionListener(this);
        starter.setEnabled(false);
        starter.setMaximumSize(starter.getPreferredSize());

        JPanel starterHolder = new JPanel();

        starterHolder.add(starter);

        getContentPane().add("South", starterHolder);

        codeList = new JList();
        codeList.addListSelectionListener(this);

        String[] toolArray = new String[tools.size()];
        int i = 0;

        for (Enumeration e = tools.keys(); e.hasMoreElements(); i++) {
            toolArray[i] = (String)e.nextElement();
        }
        StringSorter.sort(toolArray);
        toolList = new JList(toolArray);
        toolList.addListSelectionListener(this);

        JPanel headers = new JPanel(new GridLayout(1, 2));
        JLabel h1 = new JLabel("Program to work with", CENTER);
        JLabel h2 = new JLabel("Task to perform", CENTER);

        h1.setLabelFor(codeList);
        h2.setLabelFor(toolList);
        headers.add(inset(h1));
        headers.add(inset(h2));

        getContentPane().add("North", headers);

        JPanel tables = new JPanel(new GridLayout(1, 2));

        tables.add(inset(new JScrollPane(codeList)));
        tables.add(inset(new JScrollPane(toolList)));

        getContentPane().add("Center", tables);
        validate();
    }

    private void showWaiting(String s) {
        getContentPane().removeAll();
        getContentPane().setLayout(new BorderLayout());

        getContentPane().add("Center", new JLabel(s));
        validate();
    }

    public void init() {
        registryPoller = new Thread(this, "Registry Poller");
        
        registryPoller.setDaemon(true);
        registryPoller.start();
        
        showWaiting("Client version 0.02; Contacting server...");
    }
    
    public void run() {
        SocketMessagePort registryPort =
            new SocketMessagePort(this, getHostName(), ServerRegistry.DEFAULT_PORT);
        
        try {
            while (true) {
                Thread.sleep(5000);
                
                synchronized (this) {
                    while (!active) {
                        wait();
                    }
                }
                
                registryPort.sendMessage("GetServerList");
            }
        } catch (InterruptedException ex) {
        } finally {
            registryPort.dispose();
        }
    }
    
    public void start() {
        synchronized (this) {
            active = true;
            notifyAll();
        }
    }
    
    public void stop() {
        synchronized (this) {
            active = false;
        }
    }
    
    public void destroy() {
        registryPoller.interrupt();
    }
    
    private static JComponent inset(JComponent component) {
        component.setBorder(new CompoundBorder(new EmptyBorder(5, 5, 5, 5), component.getBorder()));
        return component;
    }
}

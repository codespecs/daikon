package daikon.gui;

import java.io.FileInputStream;
import java.io.ObjectInputStream;
import java.util.*;
import java.awt.Frame;
import java.awt.Container;
import java.awt.GridLayout;
import javax.swing.*;
import javax.swing.tree.*;
import javax.swing.event.*;
import daikon.*;
import daikon.inv.*;

public class InvariantsGUI {

    public static void main( String args[] ) {
	DefaultTreeModel treeModel = constructTreeModel( "daikon/gui/dsaa.obj" );
	DefaultTreeSelectionModel treeSelectionModel = new DefaultTreeSelectionModel();
	JTree tree = new JTree( treeModel );
	JPanel invariantTablePanel = new JPanel();
	tree.addTreeSelectionListener( new InvariantsTableSelectionListener( invariantTablePanel ));

	setupGUI( tree, invariantTablePanel );
    }

    protected static PptMap getPptMapFromFile( String fileName ) {
	try {
	    FileInputStream istream = new FileInputStream( fileName );
	    ObjectInputStream o = new ObjectInputStream( istream );
	    PptMap pptMap = (PptMap) o.readObject();
	    istream.close();
	    return pptMap;
	} catch (Exception e) {
	    if (e.getClass().getName().equals( "java.io.FileNotFoundException" ))
		System.out.println( "Error: invariants object file not found." );
	    else if (e.getClass().getName().equals( "java.io.StreamCorruptedException" ))
		System.out.println( "Error: invariants object file is corrupted." );
	    else
		System.out.println( e.getMessage() );
	    return null;
	}
    }

    protected static DefaultTreeModel constructTreeModel( String fileName ) {
	PptMap pptMap = getPptMapFromFile( fileName );
	if (pptMap == null)
	    throw new Error( "InvariantsGUI.constructTreeModel():  error reading pptMap from " + fileName );

	DefaultMutableTreeNode root = new DefaultMutableTreeNode( "All classes" );

	//	    topLevel.print_invariants();
	//	    System.out.println( topLevel.num_samples());
	//		System.out.println( invariant.ppt.name + ":*****" + invariant.format() + " with " + invariant.ppt.num_values() + " values and " + invariant.ppt.num_samples() + " samples." );

	//  Create the first level of the tree:  classes
	for (Iterator iter = pptMap.values().iterator(); iter.hasNext(); ) {
	    PptTopLevel topLevel = (PptTopLevel) iter.next();
	    Vector invariants = topLevel.invariants_vector();
	    for (Iterator iter2 = invariants.iterator(); iter2.hasNext(); ) {
		Invariant invariant = (Invariant) iter2.next();
		String name = invariant.ppt.name;
		if (name.indexOf( "CLASS" ) != -1) { // if this is a class, not just a method
		    String className = name.substring( 0, name.indexOf( FileIO.ppt_tag_separator ));
		    DefaultMutableTreeNode classNode = getChild( root, className );
		    if (classNode == null)
			root.add( new DefaultMutableTreeNode( className )); // Create a node for this class
//  		    DefaultMutableTreeNode classNode = getChild( root, topLevel );
//  		    if (classNode == null)
//  			root.add( new DefaultMutableTreeNode( topLevel )); // Create a node for this class
		}
	    }
	}

	//  Create the second level of the tree:  method names
	for (Iterator iter = pptMap.values().iterator(); iter.hasNext(); ) {
	    PptTopLevel topLevel = (PptTopLevel) iter.next();
	    Vector invariants = topLevel.invariants_vector();
	    for (Iterator iter2 = invariants.iterator(); iter2.hasNext(); ) {
		Invariant invariant = (Invariant) iter2.next();
		String name = invariant.ppt.name;
		if (name.indexOf( "CLASS" ) == -1) { // if this is a method, not a class
		    String className = name.substring( 0, name.indexOf('.'));
		    String methodName = name.substring( name.indexOf('.')+1, name.indexOf( FileIO.ppt_tag_separator ));
		    DefaultMutableTreeNode classNode = getChild( root, className );
		    if (classNode == null)
			throw new Error( "InvariantsGUI.constructTreeModel():  cannot find class node '" + className + "'" );
		    DefaultMutableTreeNode methodNode = getChild( classNode, methodName );
		    if (methodNode == null) 
			classNode.add( new DefaultMutableTreeNode( methodName )); // Create a node for this method
		}
	    }
	}

	//  Create the third level of the tree:  method entry and exit points
	for (Iterator iter = pptMap.values().iterator(); iter.hasNext(); ) {
	    PptTopLevel topLevel = (PptTopLevel) iter.next();
	    Vector invariants = topLevel.invariants_vector();
	    for (Iterator iter2 = invariants.iterator(); iter2.hasNext(); ) {
		Invariant invariant = (Invariant) iter2.next();
		String name = invariant.ppt.name;
		if (name.indexOf( "CLASS" ) == -1) { // if this is a method, not a class
		    String className = name.substring( 0, name.indexOf('.'));
		    String methodName = name.substring( name.indexOf('.')+1, name.indexOf( FileIO.ppt_tag_separator ));
		    String programPointName = name.substring( name.indexOf( FileIO.ppt_tag_separator ));
		    programPointName = programPointName.substring( FileIO.ppt_tag_separator.length(), programPointName.indexOf('('));
		    DefaultMutableTreeNode classNode = getChild( root, className );
		    if (classNode == null)
			throw new Error( "InvariantsGUI.constructTreeModel():  cannot find class node '" + className + "'" );
		    DefaultMutableTreeNode methodNode = getChild( classNode, methodName );
		    if (methodNode == null) 
			throw new Error( "InvariantsGUI.constructTreeModel():  cannot find method node '" + methodName + "'" );
		    DefaultMutableTreeNode programPointNode = getChild( methodNode, programPointName );
		    if (programPointNode == null) 
			methodNode.add( new DefaultMutableTreeNode( programPointName )); //  Create a node for this program point
		}
	    }
	}
	return new DefaultTreeModel( root );
    }

    //  Returns node named <code>name</code> if there is one; otherwise return <code>null</code>.
    //  Used by constructTree().
    private static DefaultMutableTreeNode getChild( DefaultMutableTreeNode node, Object userObject ) {
	for (Enumeration enum = node.children(); enum.hasMoreElements(); ) {
	    DefaultMutableTreeNode child = ((DefaultMutableTreeNode)enum.nextElement());
	    if (child.getUserObject().equals( userObject ))
		return child;
	}
	return null;
    }

    protected static void setupGUI( JTree tree, JPanel invariantTablePanel ) {
	JFrame frame = new JFrame( "Daikon GUI" );
	Container contentPane = frame.getContentPane();
	contentPane.setLayout( new GridLayout( 1, 2 ));
	contentPane.add( tree );
	contentPane.add( invariantTablePanel );
 	frame.pack();
	frame.setSize( 800, 500 );
	frame.setVisible( true );
    }
}


class InvariantsTableSelectionListener implements TreeSelectionListener {
    JPanel panel;
    List invariantTables = new ArrayList();
    List invariantTableNames = new ArrayList();

    public InvariantsTableSelectionListener( JPanel invariantTablePanel ) {
	panel = invariantTablePanel;
	panel.setLayout( new BoxLayout( panel, BoxLayout.Y_AXIS ));
    }

    public void valueChanged( TreeSelectionEvent e ) {
	TreePath paths[] = e.getPaths();
	for (int i=0; i < paths.length; i++) {
	    String name = (String) ((DefaultMutableTreeNode) paths[i].getLastPathComponent()).getUserObject();
	    if (e.isAddedPath( paths[i] )) {
		JTable table = new JTable( 1, 1 );
		table.setValueAt( name, 0, 0 );
		panel.add( table );
		invariantTables.add( table );
		invariantTableNames.add( name );
	    } else {		// paths[i] has been removed.  It should already be in invariantTableNames.
		int index = invariantTableNames.indexOf( name );
		if (index == -1)
		    throw new Error( "InvariantsTableListener.valueChanged(): " + name + " table not found." );
		panel.remove( (JTable) invariantTables.get( index ));
		invariantTables.remove( index );
		invariantTableNames.remove( index );
	    }
	    panel.repaint();
	    panel.revalidate();
	}
    }
}
















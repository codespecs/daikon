package daikon.gui;

import java.io.FileInputStream;
import java.io.ObjectInputStream;
import java.util.*;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.text.DecimalFormat;
import javax.swing.*;
import javax.swing.tree.*;
import javax.swing.table.*;
import javax.swing.event.*;
import daikon.*;
import daikon.inv.*;

public class InvariantsGUI {

    public static void main( String args[] ) {
	String objectFileName = "/g1/users/mhao/daikon/inv_files/dsaa.inv"; // use this by default, for now
	if (args.length > 0)
	    objectFileName = args[0];

	DefaultTreeModel treeModel = constructTreeModel( objectFileName );
	JTree tree = new JTree( treeModel );
	JPanel invariantTablePanel = new JPanel();
	setupGUI( tree, invariantTablePanel );

	TreeSelectionModel treeSelectionModel = tree.getSelectionModel();
	DaikonTreeSelectionListener treeSelectionListener = new DaikonTreeSelectionListener( invariantTablePanel, treeSelectionModel );
	treeSelectionModel.addTreeSelectionListener( treeSelectionListener );
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
		System.out.println( "Error: " + e.getMessage() );
	    throw new Error( "InvariantsGUI.getPptMapFromFile():  error reading pptMap from '" + fileName + "'" );
	}
    }

    protected static DefaultTreeModel constructTreeModel( String fileName ) {
	PptMap pptMap = getPptMapFromFile( fileName );

	DefaultMutableTreeNode root = new DefaultMutableTreeNode( "All classes" );

	//  Create the first level of the tree:  classes
	for (Iterator iter = pptMap.nameStringSet().iterator(); iter.hasNext(); ) {
	    String name = (String) iter.next();
	    PptName pptName = new PptName( name );
	    String className = pptName.getFullClassName();
	    //	    System.out.println( "name is " + name + ", className is " + className );
	    DefaultMutableTreeNode classNode = getChildByName( root, className );
	    if (classNode == null) {
		PptTopLevel topLevel = (PptTopLevel) pptMap.get( name );
		root.add( new DefaultMutableTreeNode( className )); // Create a node for this class
	    }
	}

	//  Create the second level of the tree:  method names
	for (Iterator iter = pptMap.nameStringSet().iterator(); iter.hasNext(); ) {
	    String name = (String) iter.next();
	    if (name.indexOf( "CLASS" ) == -1  &&  name.indexOf( "OBJECT" ) == -1 ) { // if this is a method, not a class
		String className = name.substring( 0, name.indexOf('.'));
		String methodName = name.substring( name.indexOf('.')+1, name.indexOf( FileIO.ppt_tag_separator ));
		DefaultMutableTreeNode classNode = getChildByName( root, className );
		if (classNode == null)
		    throw new Error( "InvariantsGUI.constructTreeModel():  cannot find class node '" + className + "'" );
		DefaultMutableTreeNode methodNode = getChildByName( classNode, methodName );
		if (methodNode == null) 
		    classNode.add( new DefaultMutableTreeNode( methodName )); // Create a node for this method
	    }
	}

	//  Create the third level of the tree:  method entry and exit points
	for (Iterator iter = pptMap.nameStringSet().iterator(); iter.hasNext(); ) {
	    String name = (String) iter.next();
	    PptTopLevel topLevel = (PptTopLevel) pptMap.get( name );
	    if (name.indexOf( "CLASS" ) == -1  &&  name.indexOf( "OBJECT" ) == -1 ) { // if this is a method, not a class
		String className = name.substring( 0, name.indexOf('.'));
		String methodName = name.substring( name.indexOf('.')+1, name.indexOf( FileIO.ppt_tag_separator ));
		String programPointName = name.substring( name.indexOf( FileIO.ppt_tag_separator ) + FileIO.ppt_tag_separator.length());
		DefaultMutableTreeNode classNode = getChildByName( root, className );
		if (classNode == null)
		    throw new Error( "InvariantsGUI.constructTreeModel():  cannot find class node '" + className + "'" );
		DefaultMutableTreeNode methodNode = getChildByName( classNode, methodName );
		if (methodNode == null) 
		    throw new Error( "InvariantsGUI.constructTreeModel():  cannot find method node '" + methodName + "'" );
		DefaultMutableTreeNode programPointNode = getChildByName( methodNode, programPointName );
		if (programPointNode == null) 
		    methodNode.add( new DefaultMutableTreeNode( topLevel )); //  Create a node for this program point
	    }
	}
	return new DefaultTreeModel( root );
    }

    //  Returns node with name <code>name</code> if there is one; otherwise return <code>null</code>.
    //  Used by constructTreeModel().
    private static DefaultMutableTreeNode getChildByName( DefaultMutableTreeNode node, String name ) {
	for (Enumeration enum = node.children(); enum.hasMoreElements(); ) {
	    DefaultMutableTreeNode child = ((DefaultMutableTreeNode)enum.nextElement());
	    if (child.toString().equals( name ))
		return child;
	}
	return null;
    }

    protected static void setupGUI( JTree tree, JPanel invariantTablePanel ) {
	JFrame frame = new JFrame( "Daikon GUI" );
	Container contentPane = frame.getContentPane();
	contentPane.setLayout( new GridLayout( 2, 1 ));
	contentPane.add( new JScrollPane( tree ));
	contentPane.add( new JScrollPane( invariantTablePanel ));
 	frame.pack();
	frame.setSize( 600, 700 );
	frame.setVisible( true );
    }
}


class DaikonTreeSelectionListener implements TreeSelectionListener {
    JPanel panel;
    List invariantTables = new ArrayList();
    List invariantTableNames = new ArrayList();
    TreeSelectionModel treeSelectionModel;

    public DaikonTreeSelectionListener( JPanel invariantTablePanel, TreeSelectionModel treeSelectionModel ) {
	panel = invariantTablePanel;
	panel.setLayout( new BoxLayout( panel, BoxLayout.Y_AXIS ));
	this.treeSelectionModel = treeSelectionModel;
    }

    public void valueChanged( TreeSelectionEvent e ) {
	//	System.out.println( "DaikonTreeSelectionListener.valueChanged() event: " );
	TreePath paths[] = e.getPaths();
	for (int i=0; i < paths.length; i++) {
	    Object userObject = ((DefaultMutableTreeNode) paths[i].getLastPathComponent()).getUserObject();
	    //	    System.out.println( "\t" + userObject.toString() + ", " + e.isAddedPath(i));
	    if (userObject.getClass().getName().equals( "daikon.PptTopLevel" )) {
		String name = ((PptTopLevel) userObject).name;
		if (e.isAddedPath( paths[i] )) {
		    Vector invariants = ((PptTopLevel) userObject).invariants_vector();
		    JScrollPane scrollPane = setupTable( invariants );
		    invariantTables.add( scrollPane );
		    invariantTableNames.add( name );
		    System.out.println( scrollPane.getPreferredSize());
		}
		else {		// paths[i] has been removed.  It should already be in invariantTableNames.
		    int index = invariantTableNames.indexOf( name );
		    if (index == -1)
			throw new Error( "DaikonTreeSelectionListener.valueChanged(): " + name + " table not found." );
		    panel.remove( (JScrollPane) invariantTables.get( index ));
		    invariantTables.remove( index );
		    invariantTableNames.remove( index );
		}
 	    } else {		// This is a class or a method node, not a PptTopLevel node (ie leaf node).
		if (e.isAddedPath( paths[i] )) { // Add children.
		    DefaultMutableTreeNode node = (DefaultMutableTreeNode) paths[i].getLastPathComponent();
		    for (Enumeration enum = node.children(); enum.hasMoreElements(); ) {
			TreePath newPath = paths[i].pathByAddingChild( enum.nextElement());
			TreePath leadPath = treeSelectionModel.getLeadSelectionPath();
			TreeSelectionEvent event = new TreeSelectionEvent( node, newPath, true, leadPath, leadPath );
			//			System.out.println("adding path ending in " + newPath.getLastPathComponent().toString());
			treeSelectionModel.addSelectionPath( newPath );
		    }
		}
		else {		// Remove children.
		    DefaultMutableTreeNode node = (DefaultMutableTreeNode) paths[i].getLastPathComponent();
		    for (Enumeration enum = node.children(); enum.hasMoreElements(); ) {
			TreePath newPath = paths[i].pathByAddingChild( enum.nextElement());
			TreePath leadPath = treeSelectionModel.getLeadSelectionPath();
			TreeSelectionEvent event = new TreeSelectionEvent( node, newPath, true, leadPath, leadPath );
			//			System.out.println("adding path ending in " + newPath.getLastPathComponent().toString());
			treeSelectionModel.removeSelectionPath( newPath );
		    }
		}
	    }		
	}
	panel.repaint();
	panel.revalidate();
    }

    private JScrollPane setupTable( Vector invariants ) {
	//	JScrollPane scrollPane = new JScrollPane( new JTable( new InvariantTableModel( invariants )));

	TableSorter sorter = new TableSorter( new InvariantTableModel( invariants ));
	JTable table = new JTable( sorter );
	sorter.addMouseListenerToHeaderInTable( table );
	InvariantTableModel.resizeColumns( table );
	JScrollPane scrollPane = new JScrollPane( table );
	panel.add( scrollPane );
	return scrollPane;
    }
}
    

class InvariantTableModel extends AbstractTableModel {
    Vector invariants;
    final String[] columnNames = { "invariant", "# values", "# samples", "probability", "justified" };
    DecimalFormat format = new DecimalFormat( "0.##E0" ); // for displaying probabilities

    public InvariantTableModel( Vector invariants ) {
	this.invariants = invariants;
    }

    public int getRowCount() { return invariants.size(); }

    public int getColumnCount() { return columnNames.length; }

    public String getColumnName( int column ) {
	return columnNames[ column ];
    }
    
	//	    topLevel.print_invariants();
	//	    System.out.println( topLevel.num_samples());
	//		System.out.println( invariant.ppt.name + ":*****" + invariant.format() + " with " + invariant.ppt.num_values() + " values and " + invariant.ppt.num_samples() + " samples." );

    public Object getValueAt( int row, int column ) {
	Invariant invariant = (Invariant) invariants.get( row );
	if (column == 0)
	    return invariant.format();
	else if (column == 1)
	    return new Integer( invariant.ppt.num_values());
	else if (column == 2)
	    return new Integer( invariant.ppt.num_samples());
	else if (column == 3)
	    return new Double( format.format( Math.round( 100 * invariant.getProbability()) / 100.0 ));
	else if (column == 4)
	    return new Boolean( invariant.justified());
	    
	return null;
    }

    // I want the JTable customization code here, rather than in
    // DaikonTreeSelectionListener.
    public static void resizeColumns( JTable table ) {
	for (int i = 0; i < table.getColumnCount(); i++) {
	    TableColumn column = table.getColumnModel().getColumn( i );
	    if (i == 0)
		column.setPreferredWidth( 150 );
	    else
		column.setPreferredWidth( 10 );
	}
    }
}








package daikon.gui;

import java.io.FileInputStream;
import java.io.ObjectInputStream;
import java.util.*;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.*;
import java.text.DecimalFormat;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.table.*;
import javax.swing.tree.*;
import daikon.*;
import daikon.inv.*;

public class InvariantsGUI extends JFrame implements KeyListener {
    InvariantTablesPanel invariantsTablesPanel;

    public static void main( String args[] ) {
	String invFileName = "/g1/users/mhao/daikon/inv_files/jnl.inv"; // use this by default, for now
	if (args.length > 0)
	    invFileName = args[0];
	InvariantsGUI gui = new InvariantsGUI( invFileName );
    }

    public InvariantsGUI( String invFileName ) {
	DefaultTreeModel treeModel = constructTreeModel( invFileName );
	JTree tree = new JTree( treeModel );
	JScrollPane invariantTablesScrollPane = new JScrollPane();
	setupGUI( tree, invariantTablesScrollPane );
	
	TreeSelectionModel treeSelectionModel = tree.getSelectionModel();
	invariantsTablesPanel = new InvariantTablesPanel( invariantTablesScrollPane, treeSelectionModel );
	treeSelectionModel.addTreeSelectionListener( invariantsTablesPanel );
    }

    public PptMap getPptMapFromFile( String fileName ) {
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

    public DefaultTreeModel constructTreeModel( String fileName ) {
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
	
	//  Create the second level of the tree: method names OR class-level ppt.
	//  If the ppt is associated with a method, then create the method node which will
	//  later contain entry and exit ppt's as children.  If the ppt is a class-level
	//  ppt (CLASS or CLASS-STATIC or OBJECT), then create the leaf node for this ppt
	//  right away.
	for (Iterator iter = pptMap.nameStringSet().iterator(); iter.hasNext(); ) {
	    String name = (String) iter.next();
	    PptName pptName = new PptName( name );
	    String className = pptName.getFullClassName();
	    DefaultMutableTreeNode classNode = getChildByName( root, className );
	    if (classNode == null)
		throw new Error( "InvariantsGUI.constructTreeModel():  cannot find class node '" + className + "'" );
	    //	    System.out.println(name);
	    if (pptName.isObjectInstanceSynthetic() || pptName.isClassStaticSynthetic()) {
		String programPointName = pptName.getPoint();
		DefaultMutableTreeNode programPointNode = getChildByName( classNode, programPointName );
		if (programPointNode == null) {
		    PptTopLevel topLevel = (PptTopLevel) pptMap.get( name );
		    classNode.add( new DefaultMutableTreeNode( topLevel )); //  Create a node for this program point
		}
	    }
	    else {		// is a regular method ppt
		String methodName = pptName.getFullMethodName();
		DefaultMutableTreeNode methodNode = getChildByName( classNode, methodName );
		if (methodNode == null) 
		    classNode.add( new DefaultMutableTreeNode( methodName )); // Create a node for this method
	    }
	}
	
	//  Create the third level of the tree:  method entry and exit points
	for (Iterator iter = pptMap.nameStringSet().iterator(); iter.hasNext(); ) {
	    String name = (String) iter.next();
	    PptName pptName = new PptName( name );
	    String methodName = pptName.getFullMethodName();
	    if (methodName == null) // this is a CLASS or OBJECT ppt, and has no methodName associated with it
		continue;
	    String className = pptName.getFullClassName();
	    DefaultMutableTreeNode classNode = getChildByName( root, className );
	    if (classNode == null)
		throw new Error( "InvariantsGUI.constructTreeModel():  cannot find class node '" + className + "'" );
	    DefaultMutableTreeNode methodNode = getChildByName( classNode, methodName );
	    if (methodNode == null)
		throw new Error( "InvariantsGUI.constructTreeModel():  cannot find method node '" + methodName + "'" );
	    String programPointName = pptName.getPoint();
	    DefaultMutableTreeNode programPointNode = getChildByName( methodNode, programPointName );
	    if (programPointNode == null) {
		PptTopLevel topLevel = (PptTopLevel) pptMap.get( name );
		methodNode.add( new DefaultMutableTreeNode( topLevel )); //  Create a node for this program point
	    }
	}

	//  Sort the method nodes within a class.  Sort according to a method's exit number.
	
	return new DefaultTreeModel( root );
    }

    //  Returns child with name <code>name</code> if there is one; otherwise return <code>null</code>.
    //  Used by constructTreeModel().
    protected DefaultMutableTreeNode getChildByName( DefaultMutableTreeNode node, String name ) {
	for (Enumeration enum = node.children(); enum.hasMoreElements(); ) {
	    DefaultMutableTreeNode child = ((DefaultMutableTreeNode)enum.nextElement());
	    if (child.toString().equals( name ))
		return child;
	}
	return null;
    }

    protected void setupGUI( JTree tree, JScrollPane invariantTablesScrollPane ) {
	addKeyListener( this );

	//  If the user clicks on a method, the method's ppt's will be selected
	//  but we don't want the method node to expand.
	tree.setExpandsSelectedPaths( false );

	invariantTablesScrollPane.setViewportView( new JPanel());
	JSplitPane splitPane = new JSplitPane( JSplitPane.VERTICAL_SPLIT,
					       new JScrollPane( tree ),
					       invariantTablesScrollPane );
	splitPane.setOneTouchExpandable( true );
	splitPane.setDividerSize( 2 );

	setTitle( "Daikon GUI" );
	getContentPane().add( splitPane );
 	pack();
	setSize( 600, 700 );
	setVisible( true );

	splitPane.setDividerLocation( .4 );
    }

    public void keyTyped( KeyEvent e ) {}
    public void keyPressed( KeyEvent e ) {}
    public void keyReleased( KeyEvent e ) {
	if (e.isAltDown()  &&  e.getKeyCode() == 38) // up arrow
	    invariantsTablesPanel.scrollToPreviousTable();
	else if (e.isAltDown()  &&  e.getKeyCode() == 40) // down arrow
	    invariantsTablesPanel.scrollToNextTable();
    }
}


class InvariantTablesPanel implements TreeSelectionListener {
    JScrollPane scrollPane;	// the main scrollPane, which contains the main panel
    JPanel panel;		// the main panel
    TreeSelectionModel treeSelectionModel;
    List tables = new ArrayList();
    List tableNames = new ArrayList();
    List tableHeights = new ArrayList();
    int currentTableIndex;	// used by scrollToTable methods
    
    public InvariantTablesPanel( JScrollPane scrollPane, TreeSelectionModel treeSelectionModel ) {
	this.scrollPane = scrollPane;
	this.panel = (JPanel) scrollPane.getViewport().getView();
	this.panel.setLayout( new BoxLayout( panel, BoxLayout.Y_AXIS ));
	this.treeSelectionModel = treeSelectionModel;
    }

    public void valueChanged( TreeSelectionEvent e ) {
	TreePath paths[] = e.getPaths();
	for (int i=0; i < paths.length; i++) {
	    DefaultMutableTreeNode node = (DefaultMutableTreeNode) paths[i].getLastPathComponent();
	    Object userObject = node.getUserObject();

	    //  A leaf node (PptTopLevel node) was selected or deselected.  Add or remove
	    //  the appropriate invariant tables.
	    if (userObject.getClass().getName().equals( "daikon.PptTopLevel" )) {
		String name = ((PptTopLevel) userObject).name;
		if (e.isAddedPath( paths[i] )) {
		    JComponent tableContainer = setupTable( (PptTopLevel) userObject );
		    tables.add( tableContainer );
		    tableNames.add( name );
		    tableHeights.add( new Integer( (int) tableContainer.getPreferredSize().getHeight()));
		}
		else {		// paths[i] was deselected -- it should be in invariantTableNames.
		    int index = tableNames.indexOf( name );
		    if (index == -1)
			throw new Error( "DaikonTreeSelectionListener.valueChanged(): " + name + " table not found." );
		    panel.remove( (JComponent) tables.get( index ));
		    tables.remove( index );
		    tableNames.remove( index );
		    tableHeights.remove( index );
		}

      	    //  A non-leaf node was selected or deselected.  Select or deselect its children.
 	    } else {
		if (e.isAddedPath( paths[i] )) // Add children.
		    for (Enumeration enum = node.children(); enum.hasMoreElements(); ) {
			TreePath newPath = paths[i].pathByAddingChild( enum.nextElement());
			treeSelectionModel.addSelectionPath( newPath );
		    }
		else		// Remove children.
		    for (Enumeration enum = node.children(); enum.hasMoreElements(); ) {
			TreePath newPath = paths[i].pathByAddingChild( enum.nextElement());
			treeSelectionModel.removeSelectionPath( newPath );
		    }
	    }
	}

	//  Scroll to the last invariant table that was added.
	String lastTableName = "";
	TreePath leadPath = (TreePath) e.getNewLeadSelectionPath();
	if (leadPath == null)
	    leadPath = (TreePath) e.getOldLeadSelectionPath();
	DefaultMutableTreeNode leadNode = (DefaultMutableTreeNode) leadPath.getLastPathComponent();
	if (leadNode.getUserObject().getClass().getName().equals( "daikon.PptTopLevel" ))
	    lastTableName = ((PptTopLevel) leadNode.getUserObject()).name;
	else {			//  The last selected node was not a leaf node -- ie, it was a method or a class node.
	                        //  If any of this node's children are selected, display their table.
	    DefaultMutableTreeNode child;
	    for (Enumeration enum = leadNode.children(); enum.hasMoreElements(); ) {
		child = (DefaultMutableTreeNode) enum.nextElement();
		if (treeSelectionModel.isPathSelected( leadPath.pathByAddingChild( child )))
		    if (child.getUserObject().getClass().getName().equals( "daikon.PptTopLevel" )) {
			lastTableName = ((PptTopLevel) child.getUserObject()).name;
			break;
		    }
	    }
	}
	if (tableNames.indexOf( lastTableName ) == -1)
	    ;//	    System.out.println( "InvariantTablesPanel.valueChanged(): '" + lastTableName + "' not valid" );
	else {
	    currentTableIndex = tableNames.indexOf( lastTableName );
	    scrollToCurrentTable();
	}

	    //	    System.out.println("scrolling to " + height + " / " + scrollPane.getPreferredSize().getHeight() + "\t" + tableNames.get(index));
	panel.repaint();
	panel.revalidate();
    }

    private JComponent setupTable( PptTopLevel topLevel ) {
	Vector invariants = topLevel.invariants_vector();
	TableSorter sorter = new TableSorter( new InvariantTableModel( invariants ));
	JTable table = new JTable( sorter );
	sorter.addMouseListenerToHeaderInTable( table );

	//  Make invariant column (first column) wider.
	for (int i = 0; i < table.getColumnCount(); i++) {
	    TableColumn column = table.getColumnModel().getColumn( i );
	    if (i == 0)		column.setPreferredWidth( 150 );
	    else		column.setPreferredWidth( 10 );
	}

	//  These tables have to appear in scrollPane's, or the headings won't show up.
	JScrollPane scrollPane = new JScrollPane( table );
	int width = table.getPreferredSize().width;
	int height = table.getPreferredSize().height + table.getRowHeight();
	scrollPane.setPreferredSize( new Dimension( width, height ));

	JPanel tablePanel = new JPanel();
	tablePanel.setLayout( new BoxLayout( tablePanel, BoxLayout.Y_AXIS ));

	PptName pptName = new PptName( topLevel.name );
	String headingString;
	if (pptName.getShortMethodName() == null)
	    headingString = pptName.getFullClassName() + " : " + pptName.getPoint();
	else			// want SHORT method name so table headings doesn't get too wide
	    headingString = pptName.getFullClassName() + "." + pptName.getShortMethodName() + "() : " + pptName.getPoint();
	//	JEditorPane heading = new JEditorPane( "text/plain", headingString );
	JLabel heading = new JLabel( headingString );
	heading.setForeground( new Color( 50, 30, 100 ));
	heading.setAlignmentX( .5f );

	tablePanel.add( Box.createRigidArea( new Dimension( 0, 10 )));
	tablePanel.add( heading );
	tablePanel.add( Box.createRigidArea( new Dimension( 0, 10 )));
	if (invariants.size() != 0)
	    tablePanel.add( scrollPane );
	tablePanel.setBorder( BorderFactory.createCompoundBorder( BorderFactory.createEmptyBorder( 10, 10, 10, 10 ),
								  BorderFactory.createEtchedBorder()));
	panel.add( tablePanel );
	return tablePanel;
    }

    void scrollToCurrentTable() {
	int height = 0;
	for (int i=0; i < currentTableIndex; i++)
	    height += ((Integer) tableHeights.get( i )).intValue();
	scrollPane.getViewport().setViewPosition( new Point( 0, height ));
    }

    public void scrollToPreviousTable() {
	if (currentTableIndex > 0)
 	    currentTableIndex--;
	scrollToCurrentTable();
    }

    public void scrollToNextTable() {
	if (currentTableIndex  <  tables.size() - 1)
	    currentTableIndex++;
	scrollToCurrentTable();
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

    public Object getValueAt( int row, int column ) {
	Invariant invariant = (Invariant) invariants.get( row );
	if (column == 0)	    return invariant.format();
	else if (column == 1)	    return new Integer( invariant.ppt.num_values());
	else if (column == 2)	    return new Integer( invariant.ppt.num_samples());
	else if (column == 3)	    return new Double( format.format( Math.round( 100 * invariant.getProbability()) / 100.0 ));
	else if (column == 4)	    return new Boolean( invariant.justified());
	return null;
    }
}


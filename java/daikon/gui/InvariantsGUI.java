package daikon.gui;

import java.io.*;
import java.util.*;
import java.util.zip.GZIPInputStream;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Point;
import java.awt.event.*;
import java.text.DecimalFormat;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.filechooser.FileFilter;
import javax.swing.table.*;
import javax.swing.tree.*;
import daikon.*;
import daikon.inv.*;

public class InvariantsGUI extends JFrame implements ActionListener, KeyListener {
    InvariantTablesPanel invariantsTablesPanel;

    public static void main( String args[] ) {
	InvariantsGUI gui;
	if (args.length > 0)
	    gui = new InvariantsGUI( args[0] );
	else
	    gui = new InvariantsGUI();
    }

    public InvariantsGUI( String invFileName ) {
	loadInvariantsFromFile( invFileName );
    }

    public InvariantsGUI() {
	String invFileName = pickFileFromFileChooser();
	loadInvariantsFromFile( invFileName );
    }

    public void loadInvariantsFromFile( String invFileName ) {
	DefaultTreeModel treeModel = constructTreeModel( invFileName );
	JTree tree = new JTree( treeModel );
	JScrollPane invariantTablesScrollPane = new JScrollPane();
	
	TreeSelectionModel treeSelectionModel = tree.getSelectionModel();
	invariantsTablesPanel = new InvariantTablesPanel( invariantTablesScrollPane, treeSelectionModel );
	treeSelectionModel.addTreeSelectionListener( invariantsTablesPanel );

	setupGUI( tree, invariantTablesScrollPane );
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

    public PptMap getPptMapFromFile( String fileName ) {
	try {
	    InputStream istream = new FileInputStream( fileName );
	    if (fileName.endsWith( ".gz" ))
	        istream = new GZIPInputStream( istream );
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

    //  Returns child with name <code>name</code> if there is one; otherwise return <code>null</code>.
    //  Used by constructTreeModel().
    protected DefaultMutableTreeNode getChildByName( DefaultMutableTreeNode node, String name ) {
	for (Enumeration enum = node.children(); enum.hasMoreElements(); ) {
	    DefaultMutableTreeNode child = ((DefaultMutableTreeNode) enum.nextElement());
	    if (child.toString().equals( name ))
		return child;
	}
	return null;
    }

    protected void setupGUI( JTree tree, JScrollPane invariantTablesScrollPane ) {
	JMenuBar menuBar = new JMenuBar();
	setJMenuBar( menuBar );
	JMenu menu = new JMenu( "File" );
	menu.setMnemonic( KeyEvent.VK_F );
	menuBar.add( menu );
	JMenuItem menuItem = new JMenuItem( "Load", KeyEvent.VK_L );
	menuItem.addActionListener( this );
	menu.add( menuItem );
	menuItem = new JMenuItem( "Quit", KeyEvent.VK_Q );
	menuItem.addActionListener( this );
	menu.add( menuItem );

	removeKeyListener( this ); // setupGUI() might be called more than once, but we only
                                   // want to add it as KeyListener once.
	addKeyListener( this );	   // for scrolling through tables

	//  If the user clicks on a method, the method's ppt's will be selected
	//  but we don't want the method node to expand.
	tree.setExpandsSelectedPaths( false );
 
	JCheckBox showUnjustifiedCheckBox = new JCheckBox( "Show unjustified invariants" );
	showUnjustifiedCheckBox.addItemListener( invariantsTablesPanel );
	showUnjustifiedCheckBox.setName( "showUnjustifiedCheckBox" );
	showUnjustifiedCheckBox.setFont( showUnjustifiedCheckBox.getFont().deriveFont( Font.PLAIN ));
	JCheckBox showObviousCheckBox = new JCheckBox( "Show obvious invariants" );
	showObviousCheckBox.addItemListener( invariantsTablesPanel );
	showObviousCheckBox.setName( "showObviousCheckBox" );

	JPanel controlPanel = new JPanel();
	controlPanel.add( showUnjustifiedCheckBox );
	//	controlPanel.add( showObviousCheckBox );

	JPanel topPanel = new JPanel();	// includes control panel and tree
	topPanel.setLayout( new BorderLayout());
	topPanel.add( controlPanel, BorderLayout.NORTH );
	topPanel.add( new JScrollPane( tree ), BorderLayout.CENTER );

	//	invariantTablesScrollPane.setViewportView( new JPanel());
	JSplitPane splitPane = new JSplitPane( JSplitPane.VERTICAL_SPLIT,
					       topPanel, invariantTablesScrollPane );
	splitPane.setOneTouchExpandable( true );
	splitPane.setDividerSize( 2 );

	getContentPane().removeAll();
	setTitle( "Daikon GUI" );
	getContentPane().add( splitPane );
 	pack();
	setSize( 600, 700 );
	setVisible( true );
	setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );

	splitPane.setDividerLocation( .4 );
    }

    public void actionPerformed( ActionEvent e ) {
	String menuName = ((JMenuItem) (e.getSource())).getText();
	if (menuName.equals( "Load" )) {
	    String invFileName = pickFileFromFileChooser();
	    loadInvariantsFromFile( invFileName );
	}
	else if (menuName.equals( "Quit" ))
	    System.exit( 0 );
    }

    String pickFileFromFileChooser() {
	final JFileChooser fileChooser = new JFileChooser();
	fileChooser.addChoosableFileFilter( new InvFileFilter());
	int returnValue = JFileChooser.CANCEL_OPTION;
	while (returnValue != JFileChooser.APPROVE_OPTION)
	    returnValue = fileChooser.showOpenDialog( this );
	String fileName = "";
	try {
	    fileName = fileChooser.getSelectedFile().getCanonicalPath();
	} catch (IOException e) {
	    System.out.println( "InvariantsGUI.pickFileFromFileChooser():  error selecting file '" + fileName + "'" );
	    throw new Error( e.getMessage());
	}
	return fileName;
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



class InvFileFilter extends FileFilter {
    public boolean accept( File file ) {
	if (file.isDirectory())
	    return true;

	String fileName = file.getName();
	if (fileName.endsWith( ".inv" )  ||  fileName.endsWith( ".inv.gz" ))
	    return true;
	else
	    return false;
    }
    
    public String getDescription() {
        return ".inv files";
    }
}


class InvariantTablesPanel implements TreeSelectionListener, ItemListener {
    JScrollPane scrollPane;	// the main scrollPane, which contains the main panel
    JPanel panel;		// the main panel
    TreeSelectionModel treeSelectionModel;

    List tables = new ArrayList();
    List tableNames = new ArrayList();
    List tableHeights = new ArrayList();
    List tableModels = new ArrayList();
    int currentTableIndex;	// used by scrollToTable methods
    
    public InvariantTablesPanel( JScrollPane scrollPane, TreeSelectionModel treeSelectionModel ) {
	this.scrollPane = scrollPane;
	this.panel = new JPanel();
	this.panel.setLayout( new BoxLayout( panel, BoxLayout.Y_AXIS ));
	this.scrollPane.setViewportView( panel );
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
	InvariantTableModel tableModel = new InvariantTableModel( invariants );
	tableModels.add( tableModel );
	TableSorter sorter = new TableSorter( tableModel );
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

    public void itemStateChanged( ItemEvent e ) {
	if (((JCheckBox) e.getItem()).getName().equals( "showUnjustifiedCheckBox" )) {
	    if (e.getStateChange() == ItemEvent.SELECTED)
		for (Iterator iter = tableModels.iterator(); iter.hasNext(); )
		    ((InvariantTableModel) iter.next()).showUnjustifiedInvariants( true );
	    else
		for (Iterator iter = tableModels.iterator(); iter.hasNext(); )
		    ((InvariantTableModel) iter.next()).showUnjustifiedInvariants( false );
	}
	else if (((JCheckBox) e.getItem()).getName().equals( "showObviousCheckBox" )) {
	    if (e.getStateChange() == ItemEvent.SELECTED)
		for (Iterator iter = tableModels.iterator(); iter.hasNext(); )
		    ((InvariantTableModel) iter.next()).showObviousInvariants( true );
	    else
		for (Iterator iter = tableModels.iterator(); iter.hasNext(); )
		    ((InvariantTableModel) iter.next()).showObviousInvariants( false );
	}
	    panel.repaint();
	panel.revalidate();
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
    static final String[] columnNames = { "invariant", "# values", "# samples", "probability", "justified" };
    static final DecimalFormat format = new DecimalFormat( "0.##E0" ); // for displaying probabilities

    Vector allInvariants;
    Vector filteredInvariants = new Vector();	// only filtered invariants are displayed

    static boolean isShowingUnjustifiedInvariants = false;

    public InvariantTableModel( Vector invariants ) {
	allInvariants = invariants;
	showUnjustifiedInvariants( isShowingUnjustifiedInvariants );
    }

    public int getRowCount() { return filteredInvariants.size(); }

    public int getColumnCount() { return columnNames.length; }

    public String getColumnName( int column ) {
	return columnNames[ column ];
    }

    public Object getValueAt( int row, int column ) {
	Invariant invariant = (Invariant) filteredInvariants.get( row );
	if (column == 0)	    return invariant.format();
	else if (column == 1)	    return new Integer( invariant.ppt.num_values());
	else if (column == 2)	    return new Integer( invariant.ppt.num_samples());
	else if (column == 3)	    return new Double( format.format( Math.round( 100 * invariant.getProbability()) / 100.0 ));
	else if (column == 4)	    return new Boolean( invariant.justified());
	return null;
    }

    //  Methods called by InvariantsGUI to control/filter what invariants are being displayed.

    public void showUnjustifiedInvariants( boolean showUnjustifiedInvariants ) {
	//  Assume this method is called only when showUnjustifiedInvariants changes.
	if (showUnjustifiedInvariants)
	    filteredInvariants = allInvariants;
	else {
	    filteredInvariants = new Vector();
	    Invariant invariant;
	    for (Iterator iter = allInvariants.iterator(); iter.hasNext(); ) {
		invariant = (Invariant) iter.next();
		if (invariant.justified())
		    filteredInvariants.add( invariant );
	    }
	}
	this.isShowingUnjustifiedInvariants = showUnjustifiedInvariants;
	fireTableDataChanged();
    }

    public void showObviousInvariants( boolean showObviousInvariants ) {
    }
}



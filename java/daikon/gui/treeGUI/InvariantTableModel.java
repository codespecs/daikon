package daikon.gui.treeGUI;

import daikon.*;
import daikon.inv.*;
import daikon.inv.filter.*;
import utilMDE.*;

import java.util.*;
import java.awt.BorderLayout;	// not java.awt.* to avoid java.awt.List
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Point;
import java.awt.event.*;
import java.io.IOException;
import java.io.File;
import java.text.DecimalFormat;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.event.*;
import javax.swing.filechooser.FileFilter;
import javax.swing.plaf.FontUIResource;
import javax.swing.table.*;
import javax.swing.tree.*;

//  InvariantsGUI is a GUI for displaying daikon invariants.  InvariantsGUI reads in a
//  .inv file (a serialized PptMap), constructs a hierarchy of Ppt's, and allows the user
//  to view various tables of invariants by selecting points of the hierarchy.

public class InvariantsGUI extends JFrame implements ActionListener, KeyListener {

  public static final String PLEASE_REPORT_ERROR_STRING = "\nPlease report this error to daikon@sdg.lcs.mit.edu.";

  InvariantTablesPanel invariantsTablesPanel;
  InvariantFilters invariantFilters = new InvariantFilters();
  List filterCheckBoxes = new ArrayList();
  final JList variablesList = new JList( new DefaultListModel());

  public static void main( String args[] ) {
    InvariantsGUI gui;
    if (args.length > 1) {
      showErrorMessage( "The GUI must be invoked with only one argument, a .inv or .inv.gz file.\nPlease try running the gui again." );
      System.exit( 0 );
    } else if (args.length == 1)
      gui = new InvariantsGUI( args[0] );
    else
      gui = new InvariantsGUI();
  }

  public InvariantsGUI( String invFileName ) {
    displayInvariantsFromFile( invFileName );

    // Unlike displayInvariantsFromFile(), which needs to be run everytime the user specifies
    // a new .inv file, displayControlPanel() only needs to be run once.
    displayControlPanel();
  }

  public InvariantsGUI() {
    String invFileName = pickFileFromFileChooser();
    displayInvariantsFromFile( invFileName );

    // Unlike displayInvariantsFromFile(), which needs to be run everytime the user specifies
    // a new .inv file, displayControlPanel() only needs to be run once.
    displayControlPanel();
  }

  public void displayInvariantsFromFile( String invFileName ) {
    // First read in a PptMap object from a .inv or .inv.gz file.
    PptMap pptMap = null;
    while (pptMap == null) {
      try {
	pptMap = FileIO.read_serialized_pptmap( new File(invFileName),
						true // use saved config
						);
      } catch (IOException e) {
	InvariantsGUI.showErrorMessage( e.getMessage() + "\nPlease select another .inv or .inv.gz file." );
	invFileName = pickFileFromFileChooser();
      }
    }

    // Contruct the tree of Ppt's, set up the tree selection listener, and display the GUI.
    try {
      JTree tree = new JTree( constructTreeModel( pptMap ));

      TreeSelectionModel treeSelectionModel = tree.getSelectionModel();
      invariantsTablesPanel = new InvariantTablesPanel( treeSelectionModel, invariantFilters, variablesList );
      treeSelectionModel.addTreeSelectionListener( invariantsTablesPanel );

      setupGUI( tree, invariantsTablesPanel.getScrollPane());
    }
    catch (Exception e) {	// catch AssertionException's
      InvariantsGUI.showErrorMessage( "Error: Unable to display invariants." + PLEASE_REPORT_ERROR_STRING );
    }
  }

  public static void showErrorMessage( String message ) {
    JOptionPane.showMessageDialog( null, message, "Error", JOptionPane.ERROR_MESSAGE );
  }

  //  This method constructs a tree out of Ppt's.  This method is complicated and throws
  //  many Exceptions.  These Exceptions have arcane error messages that the user will not
  //  understand.  This is okay, because in displayInvariantsFromFile(), the only actual
  //  error message shown is "Error: Unable to display invariants."  I'm leaving the more
  //  descriptive, arcane error messages in the code for convenience, for when I might
  //  need them.
  public DefaultTreeModel constructTreeModel( PptMap pptMap ) {
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
	Assert.assert(className != null);
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
      Assert.assert( name != null );
      PptName pptName = new PptName( name );
      String className = pptName.getFullClassName();
      DefaultMutableTreeNode classNode = getChildByName( root, className );
      Assert.assert( classNode != null );
      //	    System.out.println(name);
      if (pptName.isObjectInstanceSynthetic() || pptName.isClassStaticSynthetic()) {
	String programPointName = pptName.getPoint();
	DefaultMutableTreeNode programPointNode = getChildByName( classNode, programPointName );
	if (programPointNode == null) {
	  PptTopLevel topLevel = (PptTopLevel) pptMap.get( name );
	  Assert.assert(topLevel != null);
	  classNode.add( new DefaultMutableTreeNode( topLevel )); //  Create a node for this program point
	}
      } else {		// is a regular method ppt
	String methodName = pptName.getFullMethodName();
	Assert.assert( methodName != null );
	DefaultMutableTreeNode methodNode = getChildByName( classNode, methodName );
	if (methodNode == null) {
	  classNode.add( new DefaultMutableTreeNode( methodName )); // Create a node for this method
	}
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
      Assert.assert( classNode != null );
      DefaultMutableTreeNode methodNode = getChildByName( classNode, methodName );
      Assert.assert( methodName != null );
      String programPointName = pptName.getPoint();
      DefaultMutableTreeNode programPointNode = getChildByName( methodNode, programPointName );
      Assert.assert( programPointNode == null );

      //  Create a node for this program point.  If this is the first program point node
      //  under this method, simply add the node.  If there are already some program point
      //  nodes, add this node in order.  Eg, make sure EXIT23 goes after ENTER and before
      //  EXIT97.
      PptTopLevel topLevel = (PptTopLevel) pptMap.get( name );
      if (methodNode.getChildCount() == 0) {
	Assert.assert(topLevel != null);
	methodNode.add( new DefaultMutableTreeNode( topLevel ));
      } else {
	int exitNumber = pptName.getPointSubscript();
	int childIndex;
	for (childIndex = 0; childIndex < methodNode.getChildCount(); childIndex++ ) {
	  Ppt currentChild = (Ppt) ((DefaultMutableTreeNode) methodNode.getChildAt( childIndex )).getUserObject();
	  int currentChildExitNumber = currentChild.ppt_name.getPointSubscript();
	  if (currentChildExitNumber > exitNumber)
	    break;
	}
	Assert.assert(topLevel != null);
	methodNode.insert( new DefaultMutableTreeNode( topLevel ), childIndex );
      }
    }

    //  TODO:  Sort the method nodes within a class.  Sort according to a method's exit number.

    return new DefaultTreeModel( root );
  }

  //  Returns child with name <code>name</code> if there is one; otherwise return <code>null</code>.
  //  Used by constructTreeModel().
  protected DefaultMutableTreeNode getChildByName( DefaultMutableTreeNode node, String name ) {
    for (Enumeration enum = node.children(); enum.hasMoreElements(); ) {
      DefaultMutableTreeNode child = ((DefaultMutableTreeNode) enum.nextElement());
      Assert.assert( child != null );
      Assert.assert( child.toString() != null );
      if (child.toString().equals( name ))
	return child;
    }
    return null;
  }

  protected void setupGUI( JTree tree, JScrollPane invariantTablesScrollPane ) {
    UIManager.put( "Button.font",   new FontUIResource( ((FontUIResource) UIManager.get( "Button.font" )).deriveFont( Font.PLAIN )));
    UIManager.put( "CheckBox.font", new FontUIResource( ((FontUIResource) UIManager.get( "CheckBox.font" )).deriveFont( Font.PLAIN )));
    UIManager.put( "RadioButton.font", new FontUIResource( ((FontUIResource) UIManager.get( "RadioButton.font" )).deriveFont( Font.PLAIN )));

    JMenuBar menuBar = new JMenuBar();
    setJMenuBar( menuBar );
    addFileMenu( menuBar );

    removeKeyListener( this ); // setupGUI() might be called more than once, but we only
    // want to add it as KeyListener once.
    addKeyListener( this );	   // for scrolling through tables

    //  If the user clicks on a method, the method's ppt's will be selected
    //  but we don't want the method node to expand.
    tree.setExpandsSelectedPaths( false );

    JPanel topPanel = new JPanel();	// includes control panel and tree
    topPanel.setLayout( new BoxLayout( topPanel, BoxLayout.Y_AXIS ));
    //	topPanel.add( controlPanel );
    topPanel.add( new JScrollPane( tree ));
    //  	topPanel.add( controlPanel, BorderLayout.NORTH );
    //  	topPanel.add( new JScrollPane( tree ), BorderLayout.CENTER );

    //	invariantTablesScrollPane.setViewportView( new JPanel());
    JSplitPane splitPane = new JSplitPane( JSplitPane.VERTICAL_SPLIT,
					   topPanel, invariantTablesScrollPane );
    splitPane.setOneTouchExpandable( true );
    splitPane.setDividerSize( 2 );

    getContentPane().removeAll();
    setTitle( "Invariants Display" );
    getContentPane().add( splitPane );
    pack();
    setSize( 550, 700 );
    setVisible( true );
    setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );

    splitPane.setDividerLocation( .4 );
  }

  void addFileMenu( JMenuBar menuBar ) {
    JMenu menu = new JMenu( "File" );
    menu.setMnemonic( KeyEvent.VK_F );
    menuBar.add( menu );
    JMenuItem menuItem = new JMenuItem( "Load file", KeyEvent.VK_L );
    menuItem.addActionListener( this );
    menu.add( menuItem );
    menuItem = new JMenuItem( "Quit", KeyEvent.VK_Q );
    menuItem.addActionListener( this );
    menu.add( menuItem );
  }

  void displayControlPanel() {
    JFrame controlPanel = new JFrame( "Control Panel" );
    controlPanel.setDefaultCloseOperation( JFrame.DO_NOTHING_ON_CLOSE );
    Container contentPane = controlPanel.getContentPane();
    contentPane.setLayout( new BoxLayout( contentPane, BoxLayout.Y_AXIS ));
    contentPane.add( createPropertyFilterSection());
    contentPane.add( createVariableFilterSection());
    controlPanel.pack();
    controlPanel.setSize( 400, 500 );
    controlPanel.setVisible( true );
  }

  JPanel createPropertyFilterSection() {
    JPanel filterButtonsPanel = new JPanel();
    filterButtonsPanel.setLayout( new BoxLayout( filterButtonsPanel, BoxLayout.X_AXIS ));
    JButton button = new JButton( "Select all filters" );
    button.addActionListener( this );
    filterButtonsPanel.add( button );
    JButton button2 = new JButton( "Deselect all filters" );
    button2.addActionListener( this );
    button2.setAlignmentX( Component.RIGHT_ALIGNMENT );
    filterButtonsPanel.add( button2 );
    filterButtonsPanel.setAlignmentX( Component.LEFT_ALIGNMENT );

    JPanel filtersPanel = new JPanel();
    filtersPanel.setBorder( createBorder( "Property filters" ));
    filtersPanel.setLayout( new BoxLayout( filtersPanel, BoxLayout.Y_AXIS ));
    for (Iterator iter = invariantFilters.getPropertyFiltersIterator(); iter.hasNext(); )
      filtersPanel.add( createFilterCheckBox( (InvariantFilter) iter.next()));
    filtersPanel.add( Box.createRigidArea( new Dimension( 10, 10 )));
    filtersPanel.add( filterButtonsPanel );
    filtersPanel.setAlignmentX( Component.LEFT_ALIGNMENT );
    return filtersPanel;
  }

  JPanel createVariableFilterSection() {
    final JTextField addVariableTextField = new JTextField();
    addVariableTextField.setPreferredSize( new Dimension( 150, 24 ));
    addVariableTextField.setMaximumSize( new Dimension( 150, 24 ));
    //	addVariableTextField.setAlignmentX( Component.LEFT_ALIGNMENT );
    JButton addVariableButton = new JButton( "Add variable" );
    JPanel addVariablePanel = new JPanel();
    addVariablePanel.setLayout( new BoxLayout( addVariablePanel, BoxLayout.X_AXIS ));
    addVariablePanel.setAlignmentX( Component.LEFT_ALIGNMENT );
    //	addVariablePanel.setLayout( new FlowLayout());
    //	addVariablePanel.setLayout( new BorderLayout());
    addVariablePanel.add( addVariableTextField );
    addVariablePanel.add( addVariableButton );

    JButton removeVariablesButton = new JButton( "Remove selected variables" );
    removeVariablesButton.setAlignmentX( Component.LEFT_ALIGNMENT );

    JLabel filterChoiceLabel = new JLabel( "Filter on: " );
    filterChoiceLabel.setForeground( Color.black );
    filterChoiceLabel.setFont( filterChoiceLabel.getFont().deriveFont( Font.PLAIN ));
    JRadioButton anyButton = new JRadioButton( "any variable" );
    anyButton.setSelected( true );
    JRadioButton allButton = new JRadioButton( "all variables" );
    ButtonGroup group = new ButtonGroup();
    group.add( anyButton );
    group.add( allButton );
    JPanel filterChoicePanel = new JPanel();
    filterChoicePanel.setLayout( new BoxLayout( filterChoicePanel, BoxLayout.Y_AXIS ));
    filterChoicePanel.add( filterChoiceLabel );
    filterChoicePanel.add( anyButton );
    filterChoicePanel.add( allButton );

    JPanel variablesControlPanel = new JPanel();
    variablesControlPanel.setLayout( new BoxLayout( variablesControlPanel, BoxLayout.Y_AXIS ));
    variablesControlPanel.setAlignmentX( Component.LEFT_ALIGNMENT );
    variablesControlPanel.add( addVariablePanel );
    variablesControlPanel.add( Box.createRigidArea( new Dimension( 25, 25 )));
    variablesControlPanel.add( removeVariablesButton );
    variablesControlPanel.add( Box.createRigidArea( new Dimension( 25, 25 )));
    variablesControlPanel.add( filterChoicePanel );

    ActionListener addVariableActionListener = new ActionListener() {
	public void actionPerformed( ActionEvent e ) {
	  if (! addVariableTextField.getText().equals( "" )) {
	    invariantFilters.addVariableFilter( addVariableTextField.getText());
	    invariantsTablesPanel.updateInvariantsDisplay();
	    DefaultListModel listModel = (DefaultListModel) variablesList.getModel();
	    listModel.addElement( addVariableTextField.getText());
	    variablesList.setModel( listModel );
	    addVariableTextField.setText( "" );
	  }
	}};
    addVariableButton.addActionListener( addVariableActionListener );
    addVariableTextField.addActionListener( addVariableActionListener );
    removeVariablesButton.addActionListener( new ActionListener() {
	public void actionPerformed( ActionEvent e ) {
	  int selectedIndices[] =  variablesList.getSelectedIndices();
	  if (selectedIndices != null) {
	    DefaultListModel listModel = (DefaultListModel) variablesList.getModel();
	    for (int i = selectedIndices.length - 1; i >= 0; i--) {
	      invariantFilters.removeVariableFilter( (String) listModel.getElementAt( i ));
	      listModel.removeElementAt( selectedIndices[ i ]);
	    }
	    invariantsTablesPanel.updateInvariantsDisplay();
	    variablesList.setModel( listModel );
	  }}});
    anyButton.addActionListener( new ActionListener() {
	public void actionPerformed( ActionEvent e ) {
	  invariantFilters.setVariableFilterType( InvariantFilters.ANY_VARIABLE );
	  invariantsTablesPanel.updateInvariantsDisplay();
	}});
    allButton.addActionListener( new ActionListener() {
	public void actionPerformed( ActionEvent e ) {
	  invariantFilters.setVariableFilterType( InvariantFilters.ALL_VARIABLES );
	  invariantsTablesPanel.updateInvariantsDisplay();
	}});


    JPanel variablesPanel = new JPanel();
    variablesPanel.setBorder( createBorder( "Variable filters" ));
    variablesPanel.setLayout( new BoxLayout( variablesPanel, BoxLayout.X_AXIS ));
    variablesPanel.setAlignmentX( Component.LEFT_ALIGNMENT );
    variablesPanel.add( new JScrollPane( variablesList ));
    variablesPanel.add( variablesControlPanel );
    Dimension size = new Dimension( 410, 200 );
    variablesPanel.setPreferredSize( size );
    variablesPanel.setMaximumSize( size );
    variablesPanel.setMinimumSize( size );
    return variablesPanel;
  }

  Border createBorder( String title ) {
    return BorderFactory.createTitledBorder( BorderFactory.createCompoundBorder( BorderFactory.createEmptyBorder( 10, 10, 10, 10 ),
										 BorderFactory.createEtchedBorder()),
					     title );
  }

  JCheckBox createFilterCheckBox( InvariantFilter invariantFilter ) {
    JCheckBox checkBox = new JCheckBox( invariantFilter.getDescription(), true );
    checkBox.addActionListener( this );
    checkBox.setAlignmentX( Component.LEFT_ALIGNMENT );
    filterCheckBoxes.add( checkBox );
    return checkBox;
  }

  void turnFilterCheckBoxesOn() {
    for (Iterator iter = filterCheckBoxes.iterator(); iter.hasNext(); )
      ((JCheckBox) iter.next()).setSelected( true );
  }

  void turnFilterCheckBoxesOff() {
    for (Iterator iter = filterCheckBoxes.iterator(); iter.hasNext(); )
      ((JCheckBox) iter.next()).setSelected( false );
  }

  public void actionPerformed( ActionEvent e ) {
    //  Handle File menu events
    if (e.getSource().getClass() == JMenuItem.class) {
      JMenuItem menuItem = (JMenuItem) e.getSource();
      String menuText = menuItem.getText();
      if (menuText.equals( "Load file" )) {
	String invFileName = pickFileFromFileChooser();
	displayInvariantsFromFile( invFileName );
      }
      else if (menuText.equals( "Quit" ))
	System.exit( 0 );
    }
    //  Handle checkbox events involving filters
    else if (e.getSource().getClass() == JCheckBox.class) {
      JCheckBox checkBox = (JCheckBox) e.getSource();
      String filterDescription = checkBox.getText();
      invariantFilters.changeFilterSetting( filterDescription, checkBox.isSelected());
      invariantsTablesPanel.updateInvariantsDisplay();
    }
    //  Handle button events
    else if (e.getSource().getClass() == JButton.class) {
      JButton button = (JButton) e.getSource();
      String buttonText = button.getText();
      if (buttonText.equals( "Select all filters" )) {
	turnFilterCheckBoxesOn();
	invariantFilters.turnFiltersOn();
      } else if (buttonText.equals( "Deselect all filters" )) {
	turnFilterCheckBoxesOff();
	invariantFilters.turnFiltersOff();
      }
      invariantsTablesPanel.updateInvariantsDisplay();
    }
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
      InvariantsGUI.showErrorMessage( "Unable to choose file." + PLEASE_REPORT_ERROR_STRING );
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



//  A simple class for filtering filenames.
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

  public String getDescription() { return ".inv files"; }
}



//  InvariantTablesPanel is the lower panel that displays tables of invariants for each
//  Ppt.  This class keeps track of all the tables that should be displayed.  This class
//  updates its list of tables when the user makes a new selection (ie, when the user
//  clicks elsewhere on the tree browser).

class InvariantTablesPanel implements TreeSelectionListener {
  JScrollPane scrollPane = new JScrollPane(); // the main scrollPane, which contains the main panel
  JPanel panel = new JPanel();	              // the main panel
  TreeSelectionModel treeSelectionModel;
  final InvariantFilters invariantFilters;
  final JList variablesList;

  List tables = new ArrayList();
  List tableNames = new ArrayList();
  List tableHeights = new ArrayList();
  List tableModels = new ArrayList();
  int currentTableIndex;	// used by scrollToTable methods

  public InvariantTablesPanel( TreeSelectionModel treeSelectionModel, InvariantFilters invariantFilters, JList variablesList ) {
    this.scrollPane.setViewportView( panel );
    this.panel.setLayout( new BoxLayout( panel, BoxLayout.Y_AXIS ));
    this.treeSelectionModel = treeSelectionModel;
    this.invariantFilters = invariantFilters;
    this.variablesList = variablesList;
  }

  public JScrollPane getScrollPane() { return scrollPane; }

  public void valueChanged( TreeSelectionEvent e ) {
    TreePath paths[] = e.getPaths();
    for (int i=0; i < paths.length; i++) {
      DefaultMutableTreeNode node = (DefaultMutableTreeNode) paths[i].getLastPathComponent();
      Object userObject = node.getUserObject();

      //  A leaf node (PptTopLevel node) was selected or deselected.  Add or remove
      //  the appropriate invariant tables.
      if (userObject.getClass() == daikon.PptTopLevel.class) {
	if (e.isAddedPath( paths[i] )) {
	  setupTable( (PptTopLevel) userObject );
	} else {	// paths[i] was deselected -- it should be in invariantTableNames.
	  String name = ((PptTopLevel) userObject).name;
	  int index = tableNames.indexOf( name );
	  if (index == -1)
	    //			throw new Exception( "DaikonTreeSelectionListener.valueChanged(): " + name + " table not found." );
	    InvariantsGUI.showErrorMessage( "Error processing deselection event." + InvariantsGUI.PLEASE_REPORT_ERROR_STRING );
	  panel.remove( (JComponent) tables.get( index ));
	  tables.remove( index );
	  tableNames.remove( index );
	  tableHeights.remove( index );
	  tableModels.remove( index );
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
    if (leadNode.getUserObject().getClass() == daikon.PptTopLevel.class)
      lastTableName = ((PptTopLevel) leadNode.getUserObject()).name;
    else {			//  The last selected node was not a leaf node -- ie, it was a method or a class node.
	                        //  If any of this node's children are selected, display their table.
      DefaultMutableTreeNode child;
      for (Enumeration enum = leadNode.children(); enum.hasMoreElements(); ) {
	child = (DefaultMutableTreeNode) enum.nextElement();
	if (treeSelectionModel.isPathSelected( leadPath.pathByAddingChild( child )))
	  if (child.getUserObject().getClass() == daikon.PptTopLevel.class) {
	    lastTableName = ((PptTopLevel) child.getUserObject()).name;
	    break;
	  }
      }
    }
    if (tableNames.indexOf( lastTableName ) == -1) {
      //	    System.out.println( "InvariantTablesPanel.valueChanged(): '" + lastTableName + "' not valid" );
    } else {
      currentTableIndex = tableNames.indexOf( lastTableName );
      scrollToCurrentTable();
    }

    //	    System.out.println("scrolling to " + height + " / " + scrollPane.getPreferredSize().getHeight() + "\t" + tableNames.get(index));
    panel.repaint();
    panel.revalidate();
  }

  private void setupTable( final PptTopLevel topLevel ) {
    //  	System.out.print("vars for " + topLevel.name + " (" + topLevel.var_infos.length + ") : ");
    //  	for (int i=0; i < topLevel.var_infos.length; i++)
    //  	    if (! topLevel.var_infos[i].isDerived())
    //  		System.out.print( topLevel.var_infos[i].name + " " );
    //  	System.out.println();

    List invariants = new ArrayList( topLevel.invariants_vector());
    InvariantTableModel tableModel = new InvariantTableModel( invariants, invariantFilters );
    TableSorter sorter = new TableSorter( tableModel );
    JTable table = new JTable( sorter );
    sorter.addMouseListenerToHeaderInTable( table );

    //  Make invariant column (first column) wider.
    table.getColumnModel().getColumn( 0 ).setPreferredWidth( 400 );
    for (int i = 1; i < table.getColumnCount(); i++)
      table.getColumnModel().getColumn( i ).setPreferredWidth( 10 );

    //  These tables have to appear in scrollPane's, or the headings won't show up.
    JScrollPane scrollPane = new JScrollPane( table );
    resizeScrollPane( scrollPane );

    JPanel tablePanel = new JPanel();
    tablePanel.setLayout( new BoxLayout( tablePanel, BoxLayout.Y_AXIS ));

    PptName pptName = topLevel.ppt_name;
    String headingString;
    if (pptName.getShortMethodName() == null)
      headingString = pptName.getFullClassName() + " : " + pptName.getPoint();
    else			// want SHORT method name so table headings doesn't get too wide
      headingString = pptName.getFullClassName() + "." + pptName.getShortMethodName() + "() : " + pptName.getPoint();
    //	JEditorPane heading = new JEditorPane( "text/plain", headingString );
    JLabel headingLabel = new JLabel( headingString );
    headingLabel.setForeground( new Color( 50, 30, 100 ));
    headingLabel.setAlignmentX( .5f );
    JButton showVariablesButton = new JButton( "Show variables..." );

    // need to declare this variable cause inner class actionPerformed() doesn't see "this"
    final InvariantTablesPanel invariantTablesPanel = this;

    showVariablesButton.addActionListener( new ActionListener() {
	//  Make this an inner class so it can see topLevel
	public void actionPerformed( ActionEvent e ) {
	  new VariableSelectionDialog( topLevel.var_infos, invariantFilters, invariantTablesPanel, variablesList );
	}});
    showVariablesButton.setAlignmentX( Component.RIGHT_ALIGNMENT );
    JPanel headingPanel = new JPanel();
    headingPanel.setLayout( new BoxLayout( headingPanel, BoxLayout.X_AXIS ));
    headingPanel.add( Box.createRigidArea( new Dimension( 10, 10 )));
    headingPanel.add( headingLabel );
    headingPanel.add( Box.createHorizontalGlue());
    headingPanel.add( showVariablesButton );

    tablePanel.add( Box.createRigidArea( new Dimension( 10, 10 )));
    tablePanel.add( headingPanel );
    tablePanel.add( Box.createRigidArea( new Dimension( 10, 10 )));
    tablePanel.add( scrollPane );
    tablePanel.setBorder( BorderFactory.createCompoundBorder( BorderFactory.createEmptyBorder( 10, 10, 10, 10 ),
							      BorderFactory.createEtchedBorder()));
    panel.add( tablePanel );

    this.tables.add( tablePanel );
    this.tableNames.add( topLevel.name );
    this.tableHeights.add( new Integer( (int) tablePanel.getPreferredSize().getHeight()));
    this.tableModels.add( tableModel );
  }

  public void updateInvariantsDisplay() {
    for (int i = 0; i < tableModels.size(); i++ ) {
      InvariantTableModel tableModel = (InvariantTableModel) tableModels.get( i );
      tableModel.updateInvariantList( invariantFilters );
      JPanel panel = (JPanel) tables.get( i );
      resizeScrollPane( (JScrollPane) panel.getComponent( panel.getComponentCount() - 1 ));
      tableHeights.set( i, new Integer( (int) panel.getPreferredSize().getHeight()));
    }
    panel.repaint();
    panel.revalidate();
  }

  void resizeScrollPane( JScrollPane scrollPane ) {
    JTable table = (JTable) scrollPane.getViewport().getView();
    int width = table.getPreferredSize().width;
    int height = table.getPreferredSize().height + table.getRowHeight();
    scrollPane.setPreferredSize( new Dimension( width, height ));
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



//  Each Ppt table is associated with a tableModel, which controls what information is
//  displayed (which invariants, and what data from the invariants), as well as how the
//  information is displayed (column headings, etc).  When the user changes the filter
//  settings, this class recomputes which invariants should be displayed.

class InvariantTableModel extends AbstractTableModel {
  static final String[] columnNames = { "invariant", "# values", "# samples", "probability", "justified" };
  static final Class[] columnClasses = { String.class, Integer.class, Integer.class, Double.class, Boolean.class };
  static final DecimalFormat format = new DecimalFormat( "0.##E0" ); // for displaying probabilities

  List allInvariants;
  List filteredInvariants;	// only filtered invariants are displayed

  public InvariantTableModel( List invariants, InvariantFilters invariantFilters ) {
    allInvariants = invariants;
    updateInvariantList( invariantFilters );
  }

  public int getRowCount() { return filteredInvariants.size(); }

  public int getColumnCount() { return columnNames.length; }

  public String getColumnName( int column ) { return columnNames[ column ]; }

  public Object getValueAt( int row, int column ) {
    Assert.assert( column >= 0  &&  column <= 4 );
    Invariant invariant = (Invariant) filteredInvariants.get( row );
    if (column == 0)	    return invariant.format();
    else if (column == 1)	    return new Integer( invariant.ppt.num_values());
    else if (column == 2)	    return new Integer( invariant.ppt.num_samples());
    else if (column == 3)	    return new Double( format.format( Math.round( 100 * invariant.getProbability()) / 100.0 ));
    else /* (column == 4) */	    return new Boolean( invariant.justified());
  }

  //  Must override this method so TableSorter will sort numerical columns properly.
  public Class getColumnClass( int column ) {
    return columnClasses[ column ];
  }

  public void updateInvariantList( InvariantFilters invariantFilters ) {
    filteredInvariants = new ArrayList();
    for (Iterator iter = allInvariants.iterator(); iter.hasNext(); ) {
      Invariant invariant = (Invariant) iter.next();
      if (invariantFilters.shouldKeep( invariant ))
	filteredInvariants.add( invariant );
    }
    filteredInvariants = InvariantFilters.addEqualityInvariants( filteredInvariants );

    fireTableDataChanged();
  }
}



//  A custom JDialog class to help with variable filtering.  This dialog box displays all
//  the variables in a Ppt, and allows the user to select variables of interest.

class VariableSelectionDialog extends JDialog {
  public VariableSelectionDialog( VarInfo vInfos[], InvariantFilters iFilters, InvariantTablesPanel iTablesPanel, JList vList ) {
    super();
    VarInfo[] varInfos = vInfos;
    final InvariantFilters invariantFilters = iFilters;
    final InvariantTablesPanel invariantsTablesPanel = iTablesPanel;
    final JList variablesList = vList;
    final List variableCheckBoxes = new ArrayList();
    this.setDefaultCloseOperation( JFrame.DO_NOTHING_ON_CLOSE );
    JPanel variablesPanel = new JPanel();
    variablesPanel.setLayout( new BoxLayout( variablesPanel, BoxLayout.Y_AXIS ));
    variablesPanel.setBorder( BorderFactory.createEmptyBorder( 10, 10, 10, 10 ));
    variablesPanel.setAlignmentX( Component.CENTER_ALIGNMENT );
    variablesPanel.add( new JLabel( "Select the variables of interest: " ));
    for (int i=0; i < varInfos.length; i++)
      if (! varInfos[i].isDerived()) {
	final VarInfo varInfo = varInfos[i];
	JCheckBox checkBox = new JCheckBox( varInfo.name.name() );
	variablesPanel.add( checkBox );
	variableCheckBoxes.add( checkBox );
      }

    JButton cancelButton = new JButton( "Cancel" );

    // need to declare this variable cause inner class actionPerformed() doesn't see "this"
    final VariableSelectionDialog variableSelectionDialog = this;

    cancelButton.addActionListener( new ActionListener() {
	public void actionPerformed( ActionEvent e ) {
	  variableSelectionDialog.setVisible( false );
	}});
    final JButton okButton = new JButton( "Filter on selected variables" );
    okButton.addActionListener( new ActionListener() {
	public void actionPerformed( ActionEvent e ) {
	  DefaultListModel listModel = (DefaultListModel) variablesList.getModel();
	  for (int i=0; i < variableCheckBoxes.size(); i++ )
	    if (((JCheckBox) variableCheckBoxes.get( i )).isSelected()) {
	      invariantFilters.addVariableFilter( ((JCheckBox) variableCheckBoxes.get( i )).getText());
	      invariantsTablesPanel.updateInvariantsDisplay();
	      listModel.addElement( ((JCheckBox) variableCheckBoxes.get( i )).getText());
	    }
	  variableSelectionDialog.setVisible( false );
	  variablesList.setModel( listModel );
	}});
    getRootPane().setDefaultButton( okButton );

    JPanel buttonsPanel = new JPanel();
    buttonsPanel.setLayout( new BoxLayout( buttonsPanel, BoxLayout.X_AXIS ));
    buttonsPanel.setBorder( BorderFactory.createEmptyBorder( 0, 10, 10, 10 ));
    buttonsPanel.add( Box.createHorizontalGlue());
    buttonsPanel.add( cancelButton);
    buttonsPanel.add( Box.createRigidArea( new Dimension( 10, 10 )));
    buttonsPanel.add( okButton );

    Container contentPane = getContentPane();
    contentPane.add( variablesPanel, BorderLayout.CENTER);
    contentPane.add( buttonsPanel, BorderLayout.SOUTH);

    this.pack();
    this.setVisible( true );
  }
}

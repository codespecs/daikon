package daikon.gui.treeGUI;

import daikon.*;
import daikon.inv.*;
import daikon.inv.filter.*;
import utilMDE.*;

import java.util.*;
import java.awt.BorderLayout;   // not java.awt.* to avoid java.awt.List
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

public class InvariantsGUI extends JFrame implements ActionListener, KeyListener, VariableListChangeListener {

  public static final String PLEASE_REPORT_ERROR_STRING = "\nPlease report this error to daikon-developers@pag.lcs.mit.edu.";

  InvariantTablesPanel invariantsTablesPanel;
  InvariantFilters invariantFilters = new InvariantFilters();
  List filterCheckBoxes = new ArrayList();
  final JList variablesList = new JList( new DefaultListModel());
  JScrollPane variablesListScrollPane = null;

  public static void main( String args[] ) {
    InvariantsGUI gui;
    daikon.Logger.setupLogs (daikon.Logger.INFO);
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
        invariantFilters.ppt_map = pptMap; // haha this sucks
      } catch (IOException e) {
        InvariantsGUI.showErrorMessage( e.getMessage() + "\nPlease select another .inv or .inv.gz file." );
        invFileName = pickFileFromFileChooser();
      }
    }

    // Contruct the tree of Ppt's, set up the tree selection listener, and display the GUI.
    try {
      JTree tree = new JTree( constructTreeModel( pptMap ));

      TreeSelectionModel treeSelectionModel = tree.getSelectionModel();
      invariantsTablesPanel = new InvariantTablesPanel( treeSelectionModel, invariantFilters, variablesList, this );
      treeSelectionModel.addTreeSelectionListener( invariantsTablesPanel );

      setupGUI( tree, invariantsTablesPanel.getScrollPane());
    }
    catch (Exception e) {       // catch AssertionException's
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
    for (Iterator iter = (new TreeSet(pptMap.nameStringSet())).iterator(); iter.hasNext(); ) {
      String name = (String) iter.next();
      PptName pptName = new PptName( name );
      String className = pptName.getFullClassName();
      //            System.out.println( "name is " + name + ", className is " + className );
      DefaultMutableTreeNode classNode = getChildByName( root, className );
      if (classNode == null) {
        PptTopLevel topLevel = (PptTopLevel) pptMap.get( name );
        Assert.assertTrue(className != null);
        root.add( new DefaultMutableTreeNode( className )); // Create a node for this class
      }
    }

    //  Create the second level of the tree: method names OR class-level ppt.
    //  If the ppt is associated with a method, then create the method node which will
    //  later contain entry and exit ppt's as children.  If the ppt is a class-level
    //  ppt (CLASS or CLASS-STATIC or OBJECT), then create the leaf node for this ppt
    //  right away.
    for (Iterator iter = (new TreeSet(pptMap.nameStringSet())).iterator(); iter.hasNext(); ) {
      String name = (String) iter.next();
      Assert.assertTrue( name != null );
      PptName pptName = new PptName( name );
      String className = pptName.getFullClassName();
      DefaultMutableTreeNode classNode = getChildByName( root, className );
      Assert.assertTrue( classNode != null );
      //            System.out.println(name);
      if (pptName.isObjectInstanceSynthetic() || pptName.isClassStaticSynthetic()) {
        String programPointName = pptName.getPoint();
        DefaultMutableTreeNode programPointNode = getChildByName( classNode, programPointName );
        if (programPointNode == null) {
          PptTopLevel topLevel = (PptTopLevel) pptMap.get( name );
          Assert.assertTrue(topLevel != null);
          classNode.add( new DefaultMutableTreeNode( topLevel )); //  Create a node for this program point
        }
      } else {          // is a regular method ppt
        String methodName = pptName.getFullMethodName();
        Assert.assertTrue( methodName != null );
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
      Assert.assertTrue( classNode != null );
      DefaultMutableTreeNode methodNode = getChildByName( classNode, methodName );
      Assert.assertTrue( methodName != null );
      String programPointName = pptName.getPoint();
      DefaultMutableTreeNode programPointNode = getChildByName( methodNode, programPointName );
      Assert.assertTrue( programPointNode == null );

      //  Create a node for this program point.  If this is the first program point node
      //  under this method, simply add the node.  If there are already some program point
      //  nodes, add this node in order.  Eg, make sure EXIT23 goes after ENTER and before
      //  EXIT97.
      PptTopLevel topLevel = (PptTopLevel) pptMap.get( name );
      if (methodNode.getChildCount() == 0) {
        Assert.assertTrue(topLevel != null);
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
        Assert.assertTrue(topLevel != null);
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
      Assert.assertTrue( child != null );
      Assert.assertTrue( child.toString() != null );
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
    addKeyListener( this );        // for scrolling through tables

    //  If the user clicks on a method, the method's ppt's will be selected
    //  but we don't want the method node to expand.
    tree.setExpandsSelectedPaths( false );

    JPanel topPanel = new JPanel();     // includes control panel and tree
    topPanel.setLayout( new BoxLayout( topPanel, BoxLayout.Y_AXIS ));
    //  topPanel.add( controlPanel );
    topPanel.add( new JScrollPane( tree ));
    //          topPanel.add( controlPanel, BorderLayout.NORTH );
    //          topPanel.add( new JScrollPane( tree ), BorderLayout.CENTER );

    //  invariantTablesScrollPane.setViewportView( new JPanel());
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

  public void updateVariableList(Vector newList) {
    //System.out.println("IGUI : newList is " + newList);
    DefaultListModel listModel = (DefaultListModel) variablesList.getModel();
    listModel.removeAllElements();
    for(int i = 0; i < newList.size(); i++) {
      listModel.addElement(newList.elementAt(i));
    }
    variablesList.setModel( listModel );
  }

  JPanel createVariableFilterSection() {
    final JTextField addVariableTextField = new JTextField();
    addVariableTextField.setPreferredSize( new Dimension( 150, 24 ));
    addVariableTextField.setMaximumSize( new Dimension( 150, 24 ));
    //  addVariableTextField.setAlignmentX( Component.LEFT_ALIGNMENT );
    JButton addVariableButton = new JButton( "Add variable" );
    JPanel addVariablePanel = new JPanel();
    addVariablePanel.setLayout( new BoxLayout( addVariablePanel, BoxLayout.X_AXIS ));
    addVariablePanel.setAlignmentX( Component.LEFT_ALIGNMENT );
    //  addVariablePanel.setLayout( new FlowLayout());
    //  addVariablePanel.setLayout( new BorderLayout());
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
            if (!invariantFilters.containsVariableFilter( addVariableTextField.getText() )) {
              invariantFilters.addVariableFilter( addVariableTextField.getText());
              invariantsTablesPanel.updateInvariantsDisplay();
              DefaultListModel listModel = (DefaultListModel) variablesList.getModel();
              listModel.addElement( addVariableTextField.getText());
              variablesList.setModel( listModel );
            }
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

    variablesListScrollPane = new JScrollPane( variablesList );
    variablesPanel.add( variablesListScrollPane );
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

  // fileChooser is field so that it remembers where it was between loads
  private JFileChooser fileChooser;
  String pickFileFromFileChooser() {
    if (fileChooser == null) {
      String currentDir = System.getProperty("user.dir");
      if (currentDir != null) {
        fileChooser = new JFileChooser(currentDir);
      } else {
        fileChooser = new JFileChooser();
      }
    }
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

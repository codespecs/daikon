package daikon.gui.treeGUI;

import java.util.*;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.tree.*;
import daikon.PptName;
import daikon.PptTopLevel;
import daikon.inv.filter.InvariantFilters;
import daikon.inv.Invariant;

/**
 * InvariantTablesPanel is the lower panel that displays tables of
 * invariants for each Ppt.  This class keeps track of all the tables that
 * should be displayed.  This class updates its list of tables when the
 * user makes a new selection (i.e., when the user clicks elsewhere on the
 * tree browser).
 **/
class InvariantTablesPanel implements TreeSelectionListener, VariableListChangeListener, InvariantsUpdateListener {
  JScrollPane scrollPane = new JScrollPane(); // the main scrollPane, which contains the main panel
  JPanel panel = new JPanel();	              // the main panel
  TreeSelectionModel treeSelectionModel;
  final InvariantFilters invariantFilters;
  final JList variablesList;

  List<JPanel> tables = new ArrayList<JPanel>();
  List<String> tableNames = new ArrayList<String>();
  List<Integer> tableHeights = new ArrayList<Integer>();
  List<InvariantTableModel> tableModels = new ArrayList<InvariantTableModel>();
  int currentTableIndex;	// used by scrollToTable methods

  VariableListChangeListener variableListChangeSink;

  public InvariantTablesPanel( TreeSelectionModel treeSelectionModel, InvariantFilters invariantFilters, JList variablesList, VariableListChangeListener listChangeListener ) {
    this.scrollPane.setViewportView( panel );
    this.panel.setLayout( new BoxLayout( panel, BoxLayout.Y_AXIS ));
    this.treeSelectionModel = treeSelectionModel;
    this.invariantFilters = invariantFilters;
    this.variablesList = variablesList;
    this.variableListChangeSink = listChangeListener;
  }

  public void updateVariableList(Vector newVars) {
    variableListChangeSink.updateVariableList(newVars);
  }

  public JScrollPane getScrollPane() { return scrollPane; }

  public void valueChanged( TreeSelectionEvent tse ) {
    TreePath[] paths = tse.getPaths();
    for (int i=0; i < paths.length; i++) {
      DefaultMutableTreeNode node = (DefaultMutableTreeNode) paths[i].getLastPathComponent();
      Object userObject = node.getUserObject();

      //  A leaf node (PptTopLevel node) was selected or deselected.  Add or remove
      //  the appropriate invariant tables.
      if (userObject.getClass() == daikon.PptTopLevel.class) {
	if (tse.isAddedPath( paths[i] )) {
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
	if (tse.isAddedPath( paths[i] )) // Add children.
	  for (Enumeration e = node.children(); e.hasMoreElements(); ) {
	    TreePath newPath = paths[i].pathByAddingChild( e.nextElement());
	    treeSelectionModel.addSelectionPath( newPath );
	  }
	else		// Remove children.
	  for (Enumeration e = node.children(); e.hasMoreElements(); ) {
	    TreePath newPath = paths[i].pathByAddingChild( e.nextElement());
	    treeSelectionModel.removeSelectionPath( newPath );
	  }
      }
    }

    //  Scroll to the last invariant table that was added.
    String lastTableName = "";
    TreePath leadPath = tse.getNewLeadSelectionPath();
    if (leadPath == null)
      leadPath = tse.getOldLeadSelectionPath();
    DefaultMutableTreeNode leadNode = (DefaultMutableTreeNode) leadPath.getLastPathComponent();
    if (leadNode.getUserObject().getClass() == daikon.PptTopLevel.class)
      lastTableName = ((PptTopLevel) leadNode.getUserObject()).name;
    else {			//  The last selected node was not a leaf node -- i.e., it was a method or a class node.
	                        //  If any of this node's children are selected, display their table.
      DefaultMutableTreeNode child;
      for (Enumeration e = leadNode.children(); e.hasMoreElements(); ) {
	child = (DefaultMutableTreeNode) e.nextElement();
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

    List<Invariant> invariants = new ArrayList<Invariant>( topLevel.getInvariants());
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
    if (pptName.getMethodName() == null)
      headingString = pptName.getFullClassName() + " : " + pptName.getPoint();
    else			// want SHORT method name so table headings doesn't get too wide
      headingString = pptName.getFullClassName() + "." + pptName.getMethodName() + "() : " + pptName.getPoint();
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
	  new VariableSelectionDialog( topLevel.var_infos, invariantFilters, invariantTablesPanel, variablesList, invariantTablesPanel);
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
      InvariantTableModel tableModel = tableModels.get( i );
      tableModel.updateInvariantList( invariantFilters );
      JPanel panel = tables.get( i );
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
      height += tableHeights.get( i ).intValue();
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

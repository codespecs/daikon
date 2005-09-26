package daikon.gui.treeGUI;

import java.util.*;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.*;
import daikon.VarInfo;
import daikon.inv.filter.InvariantFilters;

//  A custom JDialog class to help with variable filtering.  This dialog box displays all
//  the variables in a Ppt, and allows the user to select variables of interest.

class VariableSelectionDialog extends JDialog {
  static final long serialVersionUID = 20050923L;

  final Vector<String> selectedVarNames = new Vector<String>();

  public VariableSelectionDialog( VarInfo[] vInfos, InvariantFilters iFilters, InvariantsUpdateListener iTablesPanel, JList vList, VariableListChangeListener listChangeListenerIn ) {
    super();
    final VariableListChangeListener listChangeListener = listChangeListenerIn;
    VarInfo[] varInfos = vInfos;
    final InvariantFilters invariantFilters = iFilters;
    final InvariantsUpdateListener invariantsTablesPanel = iTablesPanel;
    final JList variablesList = vList;
    final List<JCheckBox> variableCheckBoxes = new ArrayList<JCheckBox>();
    this.setDefaultCloseOperation( JFrame.DO_NOTHING_ON_CLOSE );
    JPanel variablesPanel = new JPanel();
    variablesPanel.setLayout( new BoxLayout( variablesPanel, BoxLayout.Y_AXIS ));
    variablesPanel.setBorder( BorderFactory.createEmptyBorder( 10, 10, 10, 10 ));
    variablesPanel.setAlignmentX( Component.CENTER_ALIGNMENT );
    variablesPanel.add( new JLabel( "Select the variables of interest: " ));
    for (int i=0; i < varInfos.length; i++)
      if (! varInfos[i].isDerived()) {
	final VarInfo varInfo = varInfos[i];
	JCheckBox checkBox = new JCheckBox( varInfo.name.name(), invariantFilters.containsVariableFilter( varInfo.name.name() ));
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
	  selectedVarNames.removeAllElements();
	  DefaultListModel listModel = (DefaultListModel) variablesList.getModel();
	  for (int i=0; i < variableCheckBoxes.size(); i++ ) {
	    if (((JCheckBox) variableCheckBoxes.get( i )).isSelected()) {
	      invariantFilters.addVariableFilter( ((JCheckBox) variableCheckBoxes.get( i )).getText());
	      invariantsTablesPanel.updateInvariantsDisplay();
	      listModel.addElement( ((JCheckBox) variableCheckBoxes.get( i )).getText());
	      selectedVarNames.add( ((JCheckBox) variableCheckBoxes.get( i )).getText());
	    } else if (invariantFilters.containsVariableFilter( ((JCheckBox) variableCheckBoxes.get( i )).getText() )) {
	      invariantFilters.removeVariableFilter( ((JCheckBox) variableCheckBoxes.get( i )).getText() );
	    }
	  }
	  // System.out.println("VSD : " + selectedVarNames);
	  listChangeListener.updateVariableList(selectedVarNames);
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

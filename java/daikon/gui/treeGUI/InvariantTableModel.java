package daikon.gui.treeGUI;

import java.util.*;
import java.text.DecimalFormat;
import javax.swing.table.*;
import utilMDE.Assert;
import daikon.inv.Invariant;
import daikon.inv.filter.InvariantFilters;

//  Each Ppt table is associated with a tableModel, which controls what information is
//  displayed (which invariants, and what data from the invariants), as well as how the
//  information is displayed (column headings, etc).  When the user changes the filter
//  settings, this class recomputes which invariants should be displayed.

class InvariantTableModel extends AbstractTableModel {
  static final long serialVersionUID = 20050923L;

  static final String[] columnNames = { "invariant", "# values", "# samples", "confidence", "justified" };
  static final Class[] columnClasses = { String.class, Integer.class, Integer.class, Double.class, Boolean.class };
  private static DecimalFormat CONFIDENCE_FORMAT =
    new DecimalFormat("0.####");

  List<Invariant> allInvariants;
  List<Invariant> filteredInvariants;	// only filtered invariants are displayed

  public InvariantTableModel( List<Invariant> invariants, InvariantFilters invariantFilters ) {
    allInvariants = invariants;
    updateInvariantList( invariantFilters );
  }

  public int getRowCount() { return filteredInvariants.size(); }

  public int getColumnCount() { return columnNames.length; }

  public String getColumnName( int column ) { return columnNames[ column ]; }

  public Object getValueAt( int row, int column ) {
    Assert.assertTrue( column >= 0  &&  column <= 4 );
    Invariant invariant = filteredInvariants.get( row );
    if (column == 0)        return invariant.format();
    else if (column == 1)           return new Double(Double.NaN);
    else if (column == 2)           return new Integer( invariant.ppt.num_samples());
    else if (column == 3) {
      double val = invariant.getConfidence();
      return new Double( Math.round( 100 * val) / 100.0 );
    }
    else /* (column == 4) */        return Boolean.valueOf( invariant.justified());
  }

  //  Must override this method so TableSorter will sort numerical columns properly.
  public Class getColumnClass( int column ) {
    return columnClasses[ column ];
  }

  public void updateInvariantList( InvariantFilters invariantFilters ) {
    filteredInvariants = new ArrayList<Invariant>();
    for (Invariant invariant : allInvariants) {
      if (invariantFilters.shouldKeep( invariant ) == null)
	filteredInvariants.add( invariant );
    }
    filteredInvariants = InvariantFilters.addEqualityInvariants( filteredInvariants );

    fireTableDataChanged();
  }
}

package daikon.gui.treeGUI;

import java.util.Vector;

/**
 * This listener should be invoked when the list of variable filters
 * which are selected has changed.  It's used when the user has made a
 * final selection, so that the invariants which are displayed are
 * synchronized with the variables shown in the variable panel (after
 * the user makes changes using the variable checkbox dialog).
 **/

interface VariableListChangeListener {
  public void updateVariableList(Vector newList);
}

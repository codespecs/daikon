package daikon.tools.jtb;

import java.util.*;
import utilMDE.*;
import jtb.syntaxtree.*;
import jtb.visitor.*;

/**
 * InsertCommentFormatter is a visitor that does not actually insert
 * comments, but instead corrects positioning fields of all the tokens
 * in the tree to accomodate already-inserted comments, while
 * modifying the formatting as little as possible.  (It edits the
 * {begin,end}{Line,Column} fields).
 * <p>
 *
 * Each inserted comment either affects only the rest of its line
 * -- by shifting all subsequent characters rightward -- or only
 * subsequent lines -- by shifting lines downward.
 * <p>
 *
 * The caller must supply the collection of inserted comments for
 * recognition by this visitor.
 **/
public class InsertCommentFormatter
  extends DepthFirstVisitor
{

  private Vector comments;
  private int columnshift = 0;
  private int lineshift = 0;
  private int columnshiftline = -1; // the line currently being column-shifted.

  // column shifting only applies to a single line, then is turned off again.
  // States for the variables:
  // columnshift == 0, columnshiftline == -1:
  //    no column shifting being done
  // columnshift != 0, columnshiftline == -1:
  //    column shifting being done, but first real token not yet found
  // columnshift != 0, columnshiftline != -1:
  //    column shifting being done, applies only to specified line


  public final static String lineSep = System.getProperty("line.separator");

  public InsertCommentFormatter(Vector comments) {
    this.comments = comments;
  }

  private static int numLines(NodeToken n) {
    String image = n.tokenImage;
    return UtilMDE.count(image, lineSep);
  }

  private static int numColumns(NodeToken n) {
    if (numLines(n) > 0) {
      return 0;
    } else {
      return n.tokenImage.length();
    }
  }

  public void visit(NodeToken n) {
    // System.out.println("Visit (at " + n.beginLine + "," + n.beginColumn + ") " + n.tokenImage);

    // Handle special tokens first
    if ( n.numSpecials() > 0 )
      for ( Enumeration e = n.specialTokens.elements(); e.hasMoreElements(); )
        visit((NodeToken)e.nextElement());

    if ((columnshift == 0) && (lineshift == 0)) {
      // nothing to do
    } else {
      if (columnshift != 0) {
        if (columnshiftline == -1) {
          columnshiftline = n.beginLine;
        }
        if (columnshiftline != n.beginLine) {
          columnshift = 0;
          columnshiftline = -1;
        }
      }
      n.beginLine += lineshift;
      n.endLine += lineshift;
      n.beginColumn += columnshift;
      n.endColumn += columnshift;
      // System.out.println("Shifted by " + lineshift + "," + columnshift + ": " + n.tokenImage);
    }
    if (comments.contains(n)) {
      columnshift += numColumns(n);
      lineshift += numLines(n);
    }
    // System.out.println("End visit (at " + n.beginLine + "," + n.beginColumn + ") " + n.tokenImage);

  }
}


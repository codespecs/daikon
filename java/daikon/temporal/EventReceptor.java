package daikon.temporal;

import java.util.*;

/**
 * This class captures behavior common to all members of the scope/invariant
 * tree -- they all have a parent (possibly null), they all receive events,
 * they may have special behavior if their parent closes or opens, etc.
 * It also provides some utility code for treewalks and for handling
 * duplications (a part of the dynamic instantiation process).
 **/

abstract class EventReceptor
{
    Scope mParent; // Reference to the parent scope of this EventReceptor

    // This stores the other EventReceptors which must be deleted if this invariant
    // is deleted (e.g. if it was created inside a between scope which never
    // got realized)
    Vector tiedTo;

    // To be called by the InvariantManager when an event should be processed
    // by this receptor. This is where much of the meat of overriding classes lives.
    abstract void processEvent(Event e);

    // This method is called when the EventReceptor needs to be cloned (for the purpose
    // of new invariant instantiation).
    abstract EventReceptor produceDuplicate();

    EventReceptor()
    {
	tiedTo = new Vector();
    }

    /**
     * This method is called when the parent scope is entered. Used to eg automatically
     * activate certain kinds of scopes.
     **/
    void parentScopeEntering()
    {
    }

    // This method is called when the parent scope is exited.
    void parentScopeExiting()
    {
    }

    // Recurses up the tree to check if we descend from the supplied scope.
    boolean isChildOf(Scope s)
    {
	boolean res = false;

	if (mParent == null)
	    return false;

	if (mParent.equals(s))
	    return true;

	return mParent.isChildOf(s);
    }

    // Checks if we're the parent of something of the given class. Should probably
    // go away in later versions.
    boolean childOfClass(Class cl)
    {
	if (mParent == null)
	    return false;
	else if (cl.isInstance(mParent))
	    return true;
	else return mParent.childOfClass(cl);
    }

    // Ties the supplied event receptor to this one (if necessary)
    void setTiedTo(EventReceptor r)
    {
	if (childOfClass(Scope.ScopeBetween.class))
	    {
		tiedTo.add(r);
	    }
	else
	    {
		tiedTo = null;
	    }
    }

    // Tells us how deep down in the tree we are
    int getDepth()
    {
	if (mParent == null)
	{
		return -1;
	} else if (mParent instanceof Scope.ScopeGlobal)
	{
		return 0;
	} else {
		return mParent.getDepth() + 1;
	}
    }


    // Generates a snapshot of the current state of this receptor (used in saving/restoring
    // necessary to cope w/ between invariants)
    abstract Object generateStateSnapshot();

    // Restores using said state
    abstract void restoreState(Object snapshot);

    // Deletes this receptor and all things tied to it
    void delete()
    {
	mParent.deleteChild(this);

	// Now kill anything tied to this guy
	for (Iterator i = tiedTo.iterator(); i.hasNext(); )
	    {
		((EventReceptor)i.next()).delete();
	    }
    }
}

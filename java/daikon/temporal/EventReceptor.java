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
    Scope mParent;

    // This stores the other EventReceptors which must be deleted if this invariant
    // is deleted (e.g. if it was created inside a between scope which never
    // got realized)
    Vector tiedTo;

    abstract void processEvent(Event e);
    abstract EventReceptor produceDuplicate();

    EventReceptor()
    {
	tiedTo = new Vector();
    }

    void parentScopeEntering()
    {
    }

    void parentScopeExiting()
    {
    }

    boolean isChildOf(Scope s)
    {
	boolean res = false;

	if (mParent == null)
	    return false;

	if (mParent.equals(s))
	    return true;

	return mParent.isChildOf(s);
    }

    boolean childOfClass(Class cl)
    {
	if (mParent == null)
	    return false;
	else if (cl.isInstance(mParent))
	    return true;
	else return mParent.childOfClass(cl);
    }

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


    abstract Object generateStateSnapshot();
    abstract void restoreState(Object snapshot);

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

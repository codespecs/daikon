package daikon.chicory;

import java.util.*;

/**
 */
public class NonsensicalList extends AbstractList implements List
{

    /**
     *
     */
    private NonsensicalList()
    {
        super();
    }

    public static NonsensicalList getInstance()
    {
        return theList;
    }

    public Object get(int index)
    {
        return NonsensicalObject.getInstance();
    }

    public int size()
    {
        return -1;
    }

    private final static NonsensicalList theList = new NonsensicalList();
}

package daikon.chicory;

/*
 * Created on Feb 2, 2005
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */

/**
 * @author Eric Fellheimer
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class NonsensicalObject
{
    private static NonsensicalObject instance = new NonsensicalObject();

    private NonsensicalObject(){}

    public static NonsensicalObject getInstance(){return instance;}

}

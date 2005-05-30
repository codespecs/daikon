package daikon.chicory;

/*
 * Created on Feb 2, 2005
 *
 */

/**
 * @author Eric Fellheimer
 *
 */
public class NonsensicalObject
{
    private static NonsensicalObject instance = new NonsensicalObject();

    private NonsensicalObject(){}

    public static NonsensicalObject getInstance(){return instance;}

}

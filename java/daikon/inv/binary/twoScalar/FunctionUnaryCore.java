package daikon.inv.binary.twoScalar;

import daikon.*;
import daikon.inv.*;
import daikon.inv.Invariant.OutputFormat;
import java.io.*;
import java.lang.reflect.*;
import utilMDE.*;

// Two basic options here:
//     * take java.lang.reflect.Method objects
//        + that fits in well with pre-existing methods.  It also does wrapping
//          and unwrapping as appropriate
//        - I must provide an object or arrange that the methods are all static,
//          so I might end up with a new object of my own type after all.
//     * take Invokable objects, where I define a new interface with invoke() method
//        + I might not want wrapping, unwrapping, and args wrapped in Object[]:
//          more efficient not to do so
//        + might be more efficient to define my own interface with specialized
//          types, not Object
//        - must convert existing functions to this format.

public final class FunctionUnaryCore
  implements Serializable, Cloneable
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  transient public Method function;  // transient:  Method not serializable
  public final String methodname;   // provided to permit serialization


  // false if we're looking for y=fn(x), true if we're looking for x=fn(y)
  public boolean inverse;

  private ValueTracker values_cache = new ValueTracker(8);

  public Invariant wrapper;

  public FunctionUnaryCore(Invariant wrapper, String methodname, Method function, boolean inverse) {
    this.wrapper = wrapper;
    this.methodname = methodname;
    this.function = function;
    this.inverse = inverse;
  }

  public FunctionUnaryCore(Invariant wrapper, String methodname, boolean inverse) throws ClassNotFoundException, NoSuchMethodException {
    this(wrapper, methodname, UtilMDE.methodForName(methodname), inverse);
  }

  public Object clone() {
    try {
      FunctionUnaryCore result = (FunctionUnaryCore) super.clone();
      result.function = function;
      result.values_cache = (ValueTracker) values_cache.clone();
      return result;
    } catch (CloneNotSupportedException e) {
      throw new Error(); // can't happen
    }
  }

  // swapping during resurrection
  public void swap() {
    inverse = !inverse;
  }

  private void readObject(ObjectInputStream in) throws IOException, ClassNotFoundException, NoSuchMethodException {
    in.defaultReadObject();
    this.set_function();
  }

  private void set_function() throws ClassNotFoundException, NoSuchMethodException {
    Assert.assert(function == null);
    function = UtilMDE.methodForName(methodname);
  }

  public void add_modified(long x_int, long y_int, int count) {

    Long x = new Long(x_int);
    Long y = new Long(y_int);

    boolean ok = false;
    try {
      ok = inverse ?
	x.equals(function.invoke(null, new Object[] { y })) :
	y.equals(function.invoke(null, new Object[] { x }));
    } catch (Exception e) {
      // ok == false
    }

    if (! ok) {
      wrapper.flowThis();
      wrapper.destroy();
    }

    values_cache.add(x_int, y_int);
  }

  public double computeProbability() {
    if (wrapper.no_invariant)
      return Invariant.PROBABILITY_NEVER;
    // For now, only depend on number of samples.
    // But if this prob = 0, should depend on the function as well.
    return Invariant.prob_is_ge(values_cache.num_values(), 5);
  }


  public String format_using(OutputFormat format,
			     VarInfoName vname1,
			     VarInfoName vname2)
  {
    String argname = (inverse ? vname2 : vname1).name_using(format);
    String resultname = (inverse ? vname1 : vname2).name_using(format);
    if ((format == OutputFormat.DAIKON)
	|| (format == OutputFormat.IOA))
    {
      String eq = " == ";
      if (format == OutputFormat.IOA) eq = " = ";
      String result = resultname + eq + function + "(" + argname + ")";
      if (format == OutputFormat.IOA) result += " ***";
      return result;
    }

    return wrapper.format_unimplemented(format);
  }

  public String repr() {
    return "FunctionUnaryCore" + wrapper.varNames() + ": "
      + "function=" + function
      + ",inverse=" + inverse;
  }

  public boolean isSameFormula(FunctionUnaryCore other)
  {
    return methodname.equals(other.methodname);
  }
}

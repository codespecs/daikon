package daikon.inv.unary.sequence;

import daikon.*;
import daikon.inv.*;
import daikon.inv.binary.twoSequence.*;

import utilMDE.*;

import java.util.*;


public class CommonSequence extends SingleSequence {

  final static boolean debugCommonSequence = false;

  private int elts;
  private long[] intersect = null;

  protected CommonSequence(PptSlice ppt) {
    super(ppt);
  }
  
  public static CommonSequence instantiate(PptSlice ppt) {
    return new CommonSequence(ppt);
  }

  public String repr() {
    return "CommonSequence " + varNames() + ": "
      + "elts=\"" + elts;
  }

  private String printIntersect() {
    if (intersect==null)
      return "{}";

    String result = "{";
    for (int i=0; i<intersect.length; i++) {
      result += intersect[i];
      if (i!=intersect.length-1)
	result += ", ";
    }
    result += "}";
    return result;
  }
    

  public String format() {
    if (debugCommonSequence) {
      System.out.println(repr());
    }
    return (printIntersect() + " subset of " + var().name);
  }

  /* IOA */
  public String format_ioa(String classname) {
    String vname = var().name.ioa_name(classname);
    if (var().isIOASet())
      return printIntersect() + " \\in " + vname;
    else 
      return "(" + printIntersect() + " \\in " + vname + ") ***";
  }

  public String format_esc() {
    return "format_esc " + this.getClass() + " needs to be changed: " + format();
  }

  public String format_simplify() {
    return "format_simplify " + this.getClass() + " needs to be changed: " + format();
  }

  public void add_modified(long[] a, int count) {
    if (intersect==null)
      intersect = a;
    else {
      long[] tmp = new long[intersect.length];
      int    size = 0;
      for (int i=1; i<a.length; i++) 
	if ((ArraysMDE.indexOf(intersect, a[i])!=-1) &&
	    ((size==0) || 
	     (ArraysMDE.indexOf(ArraysMDE.subarray(tmp,0,size), a[i])==-1)))
	  tmp[size++] = a[i];
      
      if (size==0) {
	destroy();
	return;
      }

      intersect = ArraysMDE.subarray(tmp, 0, size);
    }

    intersect = (long []) Intern.intern(intersect);
    elts++;
  }

  protected double computeProbability() {
    if (no_invariant) {
      return Invariant.PROBABILITY_NEVER;
    } else {
      return Math.pow(.9, elts);
    }
  }

  public boolean isObviousImplied() {
    return false;
  }

  public boolean isSameFormula(Invariant other) {
    Assert.assert(other instanceof CommonSequence);
    return true;
  }
}

package daikon.chicory;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import org.checkerframework.checker.initialization.qual.Initialized;
import org.checkerframework.checker.lock.qual.GuardedBy;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;

/**
 * The PureMethodInfo class is a subtype of DaikonVariableInfo used for "variable types" which
 * correspond to the values of pure method invocations.
 */
public class PureMethodInfo extends DaikonVariableInfo {

  /** The MethodInfo object for this pure method. */
  private MethodInfo minfo;

  /** An array containing the parameters of this pure method. */
  private DaikonVariableInfo[] args;

  public PureMethodInfo(
      String name,
      MethodInfo methInfo,
      String typeName,
      String repTypeName,
      String receiverName,
      boolean inArray) {
    this(name, methInfo, typeName, repTypeName, receiverName, inArray, new DaikonVariableInfo[0]);
  }

  public PureMethodInfo(
      String name,
      MethodInfo methInfo,
      String typeName,
      String repTypeName,
      String receiverName,
      boolean inArray,
      DaikonVariableInfo[] args) {
    super(name, typeName, repTypeName, inArray);

    assert methInfo.isPure() : "Method " + methInfo + " is not pure";

    minfo = methInfo;

    this.args = args;

    // Update function_args
    function_args = receiverName;
    if (this.args.length != 0) {
      for (int i = 0; i < args.length; i++) {
        function_args += " " + args[i].getName();
      }
    }
  }

  /** Invokes this pure method on the given parentVal. This is safe because the method is pure! */
  @Override
  @SuppressWarnings({
    "unchecked",
    "deprecation" // in Java 9+, use canAccess instead of isAccessible
  })
  public @Nullable Object getMyValFromParentVal(Object parentVal) {
    @SuppressWarnings("nullness") // not a class initializer, so meth != null
    @NonNull Method meth = (Method) minfo.member;
    boolean changedAccess = false;
    Object retVal;

    // we want to access all methods...
    if (!meth.isAccessible()) {
      changedAccess = true;
      meth.setAccessible(true);
    }

    if (isArray) {
      // First check if parentVal is null or nonsensical
      if (parentVal == null || parentVal instanceof NonsensicalList) {
        retVal = NonsensicalList.getInstance();
      } else {
        ArrayList<@Nullable Object> retList = new ArrayList<>();

        for (Object val : (List<Object>) parentVal) { // unchecked cast
          if (val == null || val instanceof NonsensicalObject) {
            retList.add(NonsensicalObject.getInstance());
          } else {
            retList.add(executePureMethod(meth, val, getArgVals(parentVal)));
          }
        }

        retVal = retList;
      }
    } else {
      // First check if parentVal is null or nonsensical
      if (parentVal == null || parentVal instanceof NonsensicalObject) {
        retVal = NonsensicalObject.getInstance();
      } else {
        retVal = executePureMethod(meth, parentVal, getArgVals(parentVal));
      }
    }

    if (changedAccess) {
      meth.setAccessible(false);
    }

    return retVal;
  }

  /**
   * Returns the current values of this pure method's arguments based on {@code parentVal}.
   *
   * @param parentVal the parent of the current method
   * @return an Object the values of this method's arguments
   */
  private @Nullable Object[] getArgVals(Object parentVal) {
    @Nullable Object[] params = new @Nullable Object[args.length];

    for (int i = 0; i < args.length; i++) {
      Object currentVal = args[i].getMyValFromParentVal(parentVal);

      if (currentVal instanceof Runtime.PrimitiveWrapper) {
        // Convert Chicory primitive wrapper to java.lang's primitive wrapper
        Runtime.PrimitiveWrapper x = (Runtime.PrimitiveWrapper) currentVal;
        params[i] = x.getJavaWrapper();
      } else {
        params[i] = currentVal;
      }
    }
    return params;
  }

  /**
   * Returns the result of invoking the method.
   *
   * @param meth a method
   * @param receiverVal the receiver value
   * @param argVals the argument values
   * @return the result of invoking the method
   */
  @SuppressWarnings("LockOnNonEnclosingClassLiteral") // `synchronize on Runtime.class`
  private static @Nullable Object executePureMethod(
      Method meth, Object receiverVal, @Nullable Object[] argVals) {
    // Between startPure() and endPure(), no output is done to the trace file.
    // Without this synchronization, other threads would observe that
    // startPure has been called and wouldn't do any output.
    synchronized (Runtime.class) {
      Object retVal;
      try {
        // TODO is this the best way to handle this problem?
        // (when we invoke a pure method, Runtime.Enter should not be
        // called)
        Runtime.startPure();

        @SuppressWarnings("nullness") // argVals is declared Nullable
        @NonNull @Initialized @GuardedBy({}) Object tmp_retVal = meth.invoke(receiverVal, argVals);
        retVal = tmp_retVal;

        if (meth.getReturnType().isPrimitive()) {
          retVal = convertWrapper(retVal);
        }

      } catch (IllegalArgumentException e) {
        throw new Error(e);
      } catch (IllegalAccessException e) {
        throw new Error(e);
      } catch (InvocationTargetException e) {
        retVal = NonsensicalObject.getInstance();
      } catch (Throwable e) {
        throw new Error(e);
      } finally {
        Runtime.endPure();
      }

      return retVal;
    }
  }

  /**
   * Convert standard wrapped (boxed) Objects (i.e., Integers) to Chicory wrappers (ie,
   * Runtime.IntWrap). Should not be called if the Object was not auto-boxed from from a primitive!
   */
  public static @Nullable Object convertWrapper(@Nullable Object obj) {
    if (obj == null || obj instanceof NonsensicalObject || obj instanceof NonsensicalList) {
      return obj;
    }

    if (obj instanceof Integer) {
      return new Runtime.IntWrap((Integer) obj);
    } else if (obj instanceof Boolean) {
      return new Runtime.BooleanWrap((Boolean) obj);
    } else if (obj instanceof Byte) {
      return new Runtime.ByteWrap((Byte) obj);
    } else if (obj instanceof Character) {
      return new Runtime.CharWrap((Character) obj);
    } else if (obj instanceof Float) {
      return new Runtime.FloatWrap((Float) obj);
    } else if (obj instanceof Double) {
      return new Runtime.DoubleWrap((Double) obj);
    } else if (obj instanceof Long) {
      return new Runtime.LongWrap((Long) obj);
    } else if (obj instanceof Short) {
      return new Runtime.ShortWrap((Short) obj);
    } else {
      // Not a primitive object (wrapper), so just keep it the same
      return obj;
    }
  }

  @Override
  public VarKind get_var_kind() {
    return VarKind.FUNCTION;
  }

  /** Return the short name of the method as the relative name. */
  @Override
  public String get_relative_name() {
    return minfo.method_name;
  }
}

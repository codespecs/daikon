package daikon.chicory;

import daikon.dcomp.DCRuntime;
import java.lang.reflect.*;
import java.lang.reflect.Field;
import java.util.*;

/*>>>
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

/**
 * The OjbectInfo class is a subtype of DaikonVariableInfo used for variable types which are class
 * fields.
 */
public class FieldInfo extends DaikonVariableInfo {
  /** The corresponding Field */
  private Field field;

  /** The offset of this field in its containing class */
  private int field_num;

  /** whether or not this is a static field */
  private boolean is_static;

  /** whether or not this field is final */
  private boolean is_final;

  /** whether or not this field is of a primitive type */
  private boolean is_primitive;

  /** whether or not this field is an outer this variable */
  private boolean is_outer_this;

  /**
   * Class that gets the tags for fields. Used by DynComp. Accessed only by methods
   * DCRuntime.get_field_tag and DCRuntime.get_field_tag_refs_only.
   */
  public DCRuntime./*@MonotonicNonNull*/ FieldTag field_tag = null;

  public FieldInfo(
      String theName, Field field, String typeName, String repTypeName, boolean isArr) {
    super(theName, typeName, repTypeName, isArr);
    this.field = field;

    is_static = Modifier.isStatic(field.getModifiers());
    is_final = Modifier.isFinal(field.getModifiers());
    is_primitive = field.getType().isPrimitive();
    is_outer_this = field.getName().startsWith("this$");

    // Calculate the offset of this field in its class
    Class<?> clazz = field.getDeclaringClass();
    if (!field.getType().isPrimitive() || clazz.isInterface()) {
      field_num = -1;
      return;
    }
    @SuppressWarnings(
        "nullness") // Object declares no fields, so clazz != object and so superclass != null
    /*@NonNull*/ Class<?> superclass = clazz.getSuperclass();
    field_num = num_prim_fields(superclass);
    for (Field f : clazz.getDeclaredFields()) {
      if (f.equals(field)) {
        // System.out.printf ("field %s has field num %d\n", field,
        //                   field_num);
        return;
      }
      if (Modifier.isStatic(f.getModifiers())) {
        continue;
      }
      if (f.getType().isPrimitive()) field_num++;
    }
    throw new Error("Can't find " + field + " in " + field.getDeclaringClass());
  }

  /** Return the number of primitive fields in clazz and all of its superclasses. */
  public static int num_prim_fields(Class<?> clazz) {
    if (clazz == Object.class) {
      return 0;
    } else {
      @SuppressWarnings("nullness") // clazz != object and so superclass != null
      int field_cnt = num_prim_fields(clazz.getSuperclass());
      for (Field f : clazz.getDeclaredFields()) {
        if (Modifier.isStatic(f.getModifiers())) {
          continue;
        }
        if (f.getType().isPrimitive()) field_cnt++;
      }
      return field_cnt;
    }
  }

  /** Returns true iff the corresponding field is static. */
  /*@Pure*/
  @Override
  public boolean isStatic() {
    return is_static;
  }

  /** Returns true iff the corresponding field is final. */
  /*@Pure*/
  public boolean isFinal() {
    return is_final;
  }

  @Override
  @SuppressWarnings("unchecked")
  public Object getMyValFromParentVal(Object val) {
    if (isArray) {
      @SuppressWarnings("unchecked")
      List<Object> valAsList = (List<Object>) val;
      return DTraceWriter.getFieldValues(field, valAsList);
    } else {
      if (Modifier.isStatic(field.getModifiers())) {
        return DTraceWriter.getStaticValue(field);
      } else {
        return DTraceWriter.getValue(field, val);
      }
    }
  }

  public Field getField() {
    return field;
  }

  public Class<?> getType() {
    return (field.getType());
  }

  public int get_field_num() {
    return field_num;
  }

  /*@Pure*/
  public boolean isPrimitive() {
    return is_primitive;
  }

  /*@MonotonicNonNull*/ Field tag_field = null;

  public Field get_tag_field(String tag_field_name, Class<?> parent_class) {
    if (tag_field == null) {
      try {
        tag_field = parent_class.getDeclaredField(tag_field_name);
      } catch (Exception e) {
        throw new Error("can't get field " + tag_field_name + " in " + parent_class, e);
      }
    }
    return tag_field;
  }

  /**
   * Returns the kind of this variable. Statics are top level variables, instance variables are
   * fields.
   */
  @Override
  public VarKind get_var_kind() {
    if (isStatic() || is_outer_this) {
      return VarKind.VARIABLE;
    } else {
      return VarKind.FIELD;
    }
  }

  /**
   * Returns the name of this field. Since statics are top level, they have no relative name. Fields
   * return their field name.
   */
  @Override
  public /*@Nullable*/ String get_relative_name() {
    if (isStatic() || is_outer_this) {
      return null;
    } else {
      String theName = field.getName();
      // Convert the internal reflection name for an outer class
      // 'this' field to the Java language format.
      if (theName.startsWith("this$")) {
        theName = field.getType().getName() + ".this";
      }
      return theName;
    }
  }

  /* Don't include 'this' in instance variable names
  public String getName() {
      if (isStatic()) {
          return super.getName();
      } else {
          return get_relative_name();
          }
  }
  */

  /** static final fields are NOMOD. */
  @Override
  public EnumSet<VarFlags> get_var_flags() {
    EnumSet<VarFlags> flags = super.get_var_flags();
    int modbits = field.getModifiers();
    if (Modifier.isFinal(modbits) && Modifier.isStatic(modbits)) {
      flags.add(VarFlags.NOMOD);
    }
    if (is_outer_this) {
      flags.add(VarFlags.NON_NULL);
    }
    return flags;
  }
}

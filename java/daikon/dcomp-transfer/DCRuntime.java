// Post JDK 21, restrictions have been introduced that prevent developers from
// explicitly creating or modifying classes in the java.lang package (or any
// package in java.base) at compile time. This change enforces stricter modular
// integrity and is part of ongoing efforts to enhance the security of the JDK.
// However, runtime class redefinition with Instrumentation.redefineClasses()
// still works, so with a small change to the build process we can continue to
// use the method described in daikon/java/daikon/dcomp/README to instrument
// the classes contained in java.base.

// We declare the package to be "jaxa.lang" instead of "java.lang" and then
// use sed to edit the classfile after compilation back to "java.lang".

package jaxa.lang;

import org.checkerframework.checker.nullness.qual.Nullable;

/**
 * Runtime support for DynComp, a comparability front end for Chicory. This class is a collection of
 * methods; it should never be instantiated.
 */
public final class DCRuntime {

  public static boolean dcomp_equals(Object o1, Object o2) {
    return daikon.dcomp.DCRuntime.dcomp_equals(o1, o2);
  }

  public static boolean dcomp_super_equals(Object o1, Object o2) {
    return daikon.dcomp.DCRuntime.dcomp_super_equals(o1, o2);
  }

  public static Object dcomp_clone(Object orig_obj, Class<?> target_class) throws Throwable {
    return daikon.dcomp.DCRuntime.dcomp_clone(orig_obj, target_class);
  }

  public static Object dcomp_super_clone(Object orig_obj, Class<?> target_class) throws Throwable {
    return daikon.dcomp.DCRuntime.dcomp_super_clone(orig_obj, target_class);
  }

  public static boolean object_eq(Object obj1, Object obj2) {
    return daikon.dcomp.DCRuntime.object_eq(obj1, obj2);
  }

  public static boolean object_ne(Object obj1, Object obj2) {
    return daikon.dcomp.DCRuntime.object_ne(obj1, obj2);
  }

  public static Object[] create_tag_frame(String params) {
    return daikon.dcomp.DCRuntime.create_tag_frame(params);
  }

  public static void normal_exit(Object[] tag_frame) {
    daikon.dcomp.DCRuntime.normal_exit(tag_frame);
  }

  public static void normal_exit_primitive(Object[] tag_frame) {
    daikon.dcomp.DCRuntime.normal_exit_primitive(tag_frame);
  }

  public static void exception_exit(Object throwable) {
    daikon.dcomp.DCRuntime.exception_exit(throwable);
  }

  public static void throw_op() {
    daikon.dcomp.DCRuntime.throw_op();
  }

  public static void push_local_tag(Object[] tag_frame, int index) {
    daikon.dcomp.DCRuntime.push_local_tag(tag_frame, index);
  }

  public static void pop_local_tag(Object[] tag_frame, int index) {
    daikon.dcomp.DCRuntime.pop_local_tag(tag_frame, index);
  }

  public static void push_static_tag(int static_num) {
    daikon.dcomp.DCRuntime.push_static_tag(static_num);
  }

  public static void push_array_tag(Object arr_ref) {
    daikon.dcomp.DCRuntime.push_array_tag(arr_ref);
  }

  public static void pop_static_tag(int static_num) {
    daikon.dcomp.DCRuntime.pop_static_tag(static_num);
  }

  public static void discard_tag(int cnt) {
    daikon.dcomp.DCRuntime.discard_tag(cnt);
  }

  public static void aastore(Object[] arr, int index, Object val) {
    daikon.dcomp.DCRuntime.aastore(arr, index, val);
  }

  public static void bastore(byte[] arr, int index, byte val) {
    daikon.dcomp.DCRuntime.bastore(arr, index, val);
  }

  public static void zastore(boolean[] arr, int index, boolean val) {
    daikon.dcomp.DCRuntime.zastore(arr, index, val);
  }

  public static void castore(char[] arr, int index, char val) {
    daikon.dcomp.DCRuntime.castore(arr, index, val);
  }

  public static void dastore(double[] arr, int index, double val) {
    daikon.dcomp.DCRuntime.dastore(arr, index, val);
  }

  public static void fastore(float[] arr, int index, float val) {
    daikon.dcomp.DCRuntime.fastore(arr, index, val);
  }

  public static void iastore(int[] arr, int index, int val) {
    daikon.dcomp.DCRuntime.iastore(arr, index, val);
  }

  public static void lastore(long[] arr, int index, long val) {
    daikon.dcomp.DCRuntime.lastore(arr, index, val);
  }

  public static void sastore(short[] arr, int index, short val) {
    daikon.dcomp.DCRuntime.sastore(arr, index, val);
  }

  public static void multianewarray2(int count1, int count2, Object[] arr) {
    daikon.dcomp.DCRuntime.multianewarray2(count1, count2, arr);
  }

  public static void enter(Object[] tag_frame, @Nullable Object obj, int mi_index, Object[] args) {
    daikon.dcomp.DCRuntime.enter(tag_frame, obj, mi_index, args);
  }

  public static void exit(
      Object[] tag_frame,
      @Nullable Object obj,
      int mi_index,
      Object[] args,
      Object ret_val,
      int exit_line_number) {
    daikon.dcomp.DCRuntime.exit(tag_frame, obj, mi_index, args, ret_val, exit_line_number);
  }

  public static void push_field_tag(Object obj, int field_num) {
    daikon.dcomp.DCRuntime.push_field_tag(obj, field_num);
  }

  public static void push_field_tag_null_ok(Object obj, int field_num) {
    daikon.dcomp.DCRuntime.push_field_tag_null_ok(obj, field_num);
  }

  public static void pop_field_tag(Object obj, int field_num) {
    daikon.dcomp.DCRuntime.pop_field_tag(obj, field_num);
  }

  public static void binary_tag_op() {
    daikon.dcomp.DCRuntime.binary_tag_op();
  }

  public static void cmp_op() {
    daikon.dcomp.DCRuntime.cmp_op();
  }

  public static void dup() {
    daikon.dcomp.DCRuntime.dup();
  }

  public static void dup_x1() {
    daikon.dcomp.DCRuntime.dup_x1();
  }

  public static void dup_x2() {
    daikon.dcomp.DCRuntime.dup_x2();
  }

  public static void dup2() {
    daikon.dcomp.DCRuntime.dup2();
  }

  public static void dup2_x1() {
    daikon.dcomp.DCRuntime.dup2_x1();
  }

  public static void dup2_x2() {
    daikon.dcomp.DCRuntime.dup2_x2();
  }

  public static void swap() {
    daikon.dcomp.DCRuntime.swap();
  }

  public static void primitive_array_load(Object arr_ref, int index) {
    daikon.dcomp.DCRuntime.primitive_array_load(arr_ref, index);
  }

  public static void primitive_array_load_null_ok(Object arr_ref, int index) {
    daikon.dcomp.DCRuntime.primitive_array_load_null_ok(arr_ref, index);
  }

  public static void ref_array_load(Object arr_ref, int index) {
    daikon.dcomp.DCRuntime.ref_array_load(arr_ref, index);
  }

  public static void push_const() {
    daikon.dcomp.DCRuntime.push_const();
  }

  public static void set_class_initialized(String classname) {
    daikon.dcomp.DCRuntime.set_class_initialized(classname);
  }
}

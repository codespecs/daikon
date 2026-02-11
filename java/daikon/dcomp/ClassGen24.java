package daikon.dcomp;

import daikon.chicory.Runtime;
import daikon.plumelib.reflection.Signatures;
import java.lang.classfile.AccessFlags;
import java.lang.classfile.ClassBuilder;
import java.lang.classfile.ClassModel;
import java.lang.classfile.MethodModel;
import java.lang.classfile.constantpool.ClassEntry;
import java.lang.constant.ClassDesc;
import java.lang.reflect.AccessFlag;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.signature.qual.BinaryName;
import org.checkerframework.checker.signature.qual.Identifier;
import org.checkerframework.checker.signature.qual.MethodDescriptor;

/**
 * ClassGen24 represents a class. ClassGen24 is analogous to the BCEL ClassGen class. The similarity
 * makes it easier to keep DCInstrument.java and DCInstrument24.java in sync.
 *
 * <p>ClassGen24 uses Java's {@code java.lang.classfile} APIs for reading and modifying .class
 * files. Those APIs were added in JDK 24. Compared to BCEL, these APIs are more complete and robust
 * (no more fiddling with StackMaps) and are always up to date with any .class file changes (since
 * they are part of the JDK). (We will need to continue to support Instrument.java using BCEL, as we
 * anticipate our clients using JDK 21 or less for quite some time.)
 */
public class ClassGen24 {

  /**
   * Models the body of the class.
   *
   * <p>Several fields of ClassModel are cached as fields of ClassGen24 to better correspond to
   * BCEL's version of ClassGen and to reduce re-computation.
   */
  private final ClassModel classModel;

  //
  // Start of ClassModel items.
  //

  /** The class's access flags. */
  private final AccessFlags accessFlags;

  /** The list of interfaces this class implements. */
  private final List<ClassEntry> interfaceList;

  /** The name of the class's superclass, in binary name format. */
  private final @BinaryName String superclassName;

  /** The name of the class, in binary name format. */
  private final @BinaryName String className;

  //
  // End of ClassModel items.
  //

  /** The ClassBuilder for this class. */
  private final ClassBuilder classBuilder;

  /** True if this class is an interface. */
  private final boolean isInterface;

  /** True if this class is static. */
  private final boolean isStatic;

  /**
   * Creates a ClassGen24 object.
   *
   * @param classModel for the class
   * @param className class name, in binary name format
   * @param classBuilder for the class
   */
  public ClassGen24(
      final ClassModel classModel,
      final @BinaryName String className,
      final ClassBuilder classBuilder) {

    this.classModel = classModel;
    this.className = className;
    this.classBuilder = classBuilder;

    accessFlags = classModel.flags();
    isInterface = accessFlags.has(AccessFlag.INTERFACE);
    isStatic = accessFlags.has(AccessFlag.STATIC);

    superclassName = getSuperclassName(classModel);

    // The original interface list is immutable, so we need to make a copy, to accommodate method
    // `addInterface()`.
    interfaceList = new ArrayList<ClassEntry>(classModel.interfaces());
  }

  /**
   * Add an interface to this class.
   *
   * @param name the interface name, in binary format
   */
  public void addInterface(@BinaryName String name) {
    String internalName = Runtime.binaryNameToInternalForm(name);
    for (ClassEntry existing : interfaceList) {
      if (existing.asInternalName().equals(internalName)) {
        return;
      }
    }
    ClassDesc ue = ClassDesc.of(name);
    ClassEntry ce = classBuilder.constantPool().classEntry(ue);
    interfaceList.add(ce);
  }

  /**
   * Returns the indicatated method, or null if this class does not contain it.
   *
   * @param name the method's name
   * @param descriptor the method's type descriptor
   * @return the MethodModel if found, null otherwise
   */
  public @Nullable MethodModel containsMethod(
      @Identifier String name, @MethodDescriptor String descriptor) {
    for (MethodModel mm : classModel.methods()) {
      if (mm.methodName().stringValue().equals(name)
          && mm.methodType().stringValue().equals(descriptor)) {
        return mm;
      }
    }
    return null;
  }

  /**
   * Returns this class's access flags.
   *
   * @return the access flags
   */
  public AccessFlags getAccessFlags() {
    return accessFlags;
  }

  /**
   * Returns true if this class is an interface.
   *
   * @return true if this class is an interface
   */
  public final boolean isInterface() {
    return isInterface;
  }

  /**
   * Returns true if this class is static.
   *
   * @return true if this class is static
   */
  public final boolean isStatic() {
    return isStatic;
  }

  /**
   * Returns this class's name, in binary format.
   *
   * @return this class's name, in binary format
   */
  public @BinaryName String getClassName() {
    return className;
  }

  /**
   * Returns a {@code ClassModel}'s class name, in binary format.
   *
   * @return the class's name, in binary format
   */
  public static @BinaryName String getClassName(ClassModel classModel) {
    return Signatures.internalFormToBinaryName(classModel.thisClass().asInternalName());
  }

  /**
   * Returns the name of the superclass of this class. If this class is {@link Object}, it will
   * return itself ({@link Object}). This is probably incorrect but is consistent with the BCEL
   * version of getSuperclassName.
   *
   * @return the binary name of this class's superclass
   */
  public @BinaryName String getSuperclassName() {
    return superclassName;
  }

  /**
   * Returns the name of the superclass of the argument. If the argument class is {@link Object}, it
   * will return itself ({@link Object}). This is probably incorrect but is consistent with the BCEL
   * version of getSuperclassName.
   *
   * @param classModel the class to check
   * @return the binary name of the superclass of classModel or "java.lang.Object" if it has no
   *     superclass
   */
  public static @BinaryName String getSuperclassName(ClassModel classModel) {
    Optional<ClassEntry> ce = classModel.superclass();
    if (ce.isPresent()) {
      return Runtime.internalFormToBinaryName(ce.get().asInternalName());
    } else {
      return "java.lang.Object";
    }
  }

  /**
   * Returns the interfaces of this class.
   *
   * @return the interfaces of this class
   */
  public List<ClassEntry> getInterfaceList() {
    return interfaceList;
  }

  /**
   * Returns the class builder.
   *
   * @return the class builder
   */
  public ClassBuilder getClassBuilder() {
    return classBuilder;
  }

  /**
   * Returns the class name.
   *
   * @return the class name
   */
  @Override
  public final String toString(@GuardSatisfied ClassGen24 this) {
    return className;
  }
}

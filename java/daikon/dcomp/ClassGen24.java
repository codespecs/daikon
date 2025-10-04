package daikon.dcomp;

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
import org.checkerframework.checker.signature.qual.InternalForm;
import org.checkerframework.checker.signature.qual.MethodDescriptor;

/**
 * ClassGen24 collects and stores all the relevant information about a class that DCInstrument24
 * might need. ClassGen24 is analogous to the BCEL ClassGen class. The similarity makes it easier to
 * keep DCInstrument.java and DCInstrument24.java in sync.
 *
 * <p>ClassGen24 uses Java's ({@code java.lang.classfile}) APIs for reading and modifying .class
 * files. Those APIs were added in JDK 24. Compared to BCEL, these APIs are more complete and robust
 * (no more fiddling with StackMaps) and are always up to date with any .class file changes (since
 * they are part of the JDK). (We will need to continue to support Instrument.java using BCEL, as we
 * anticipate our clients using JDK 21 or less for quite some time.)
 */
public class ClassGen24 {

  /**
   * Models the body of the class.
   *
   * <p>Several fields of ClassModel are declared as fields of ClassGen24 to better correspond to
   * BCEL's version of ClassGen and to reduce re-computation. Currently we set these fields in the
   * constructor, but they could be calculated lazily on first reference.
   */
  private ClassModel classModel;

  /** The class's access flags. */
  private AccessFlags accessFlags;

  /** The class's name. */
  private @BinaryName String className;

  /** The ClassBuilder for this class. */
  private ClassBuilder classBuilder;

  /** True if the class is an interface. */
  private boolean isInterface;

  /** True if the class is static. */
  private boolean isStatic;

  /** The name of the class's enclosing class, in binary name format. */
  private @BinaryName String superclassName;

  /** The list of interfaces this class implements. */
  private List<ClassEntry> interfaceList;

  /**
   * Creates a ClassGen24 object.
   *
   * @param classModel the class
   * @param className the containing class, in binary name format
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

    // The original interface list is immutable, so we need to make a copy.
    interfaceList = new ArrayList<ClassEntry>(classModel.interfaces());
  }

  /**
   * Add an interface to this class.
   *
   * @param name the interface name (binary format)
   */
  public void addInterface(@BinaryName String name) {
    ClassDesc ue = ClassDesc.of(name);
    ClassEntry ce = classBuilder.constantPool().classEntry(ue);
    if (interfaceList.contains(ce)) {
      return;
    }
    interfaceList.add(ce);
  }

  /**
   * Determine if this class contains the indicatated method.
   *
   * @param name the method's name
   * @param descriptor the method's type descriptor
   * @return the MethodModel if found, null otherwise
   */
  public @Nullable MethodModel containsMethod(String name, @MethodDescriptor String descriptor) {
    for (MethodModel mm : classModel.methods()) {
      if (mm.methodName().stringValue().equals(name)
          && mm.methodType().stringValue().equals(descriptor)) {
        return mm;
      }
    }
    return null;
  }

  /**
   * Return the class's access flags.
   *
   * @return the access flags
   */
  public AccessFlags getAccessFlags() {
    return accessFlags;
  }

  /**
   * Return true if the class is an interface.
   *
   * @return interface flag
   */
  public final boolean isInterface() {
    return isInterface;
  }

  /**
   * Return true if the class is static.
   *
   * @return static flag
   */
  public final boolean isStatic() {
    return isStatic;
  }

  /**
   * Return the class's name.
   *
   * @return the class's name
   */
  public @BinaryName String getClassName() {
    return className;
  }

  /**
   * Returns a {@code ClassModel}'s class name.
   *
   * @return the class's name
   */
  public static @BinaryName String getClassName(ClassModel classModel) {
    @SuppressWarnings("signature:assignment") // type conversion
    @InternalForm String temp = classModel.thisClass().asInternalName();
    return Signatures.internalFormToBinaryName(temp);
  }

  /**
   * Returns the name of the super class of this class. In the case that this class is {@link
   * Object}, it will return itself ({@link Object}). This is probably incorrect but is consistent
   * with the BCEL version of getSuperclassName.
   *
   * @return the binary name of the class that contains this class
   */
  public @BinaryName String getSuperclassName() {
    return superclassName;
  }

  /**
   * Returns the name of the super class of the argument. In the case that the argument class is
   * {@link Object}, it will return itself ({@link Object}). This is probably incorrect but is
   * consistent with the BCEL version of getSuperclassName.
   *
   * @param classModel the class to check
   * @return the binary name of the superclass of classModel or "java.lang.Object" if it has no
   *     superclass
   */
  public static @BinaryName String getSuperclassName(ClassModel classModel) {
    Optional<ClassEntry> ce = classModel.superclass();
    if (ce.isPresent()) {
      @SuppressWarnings("signature:assignment") // type conversion
      @BinaryName String scn = ce.get().asInternalName().replace('/', '.');
      return scn;
    } else {
      return "java.lang.Object";
    }
  }

  /**
   * Return the interfaceList.
   *
   * @return the interfaceList
   */
  public List<ClassEntry> getInterfaceList() {
    return interfaceList;
  }

  /**
   * Return the class builder.
   *
   * @return the class builder
   */
  public ClassBuilder getClassBuilder() {
    return classBuilder;
  }

  /**
   * Return the class name.
   *
   * @return the class name
   */
  @Override
  public final String toString(@GuardSatisfied ClassGen24 this) {
    return className;
  }
}

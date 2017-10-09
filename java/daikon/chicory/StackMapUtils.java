package daikon.chicory;

import daikon.util.*;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import org.apache.bcel.Const;
import org.apache.bcel.classfile.*;
import org.apache.bcel.generic.*;

/*>>>
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.checker.signature.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

/**
 * BCEL should automatically build and maintain the StackMapTable in a manner similar to the
 * LineNumberTable and the LocalVariableTable. However, for historical reasons it does not. Hence,
 * we provide a set of methods to make it easier to manipulate the StackMapTable.
 */
@SuppressWarnings("nullness")
public abstract class StackMapUtils {

  /*
   * NOMENCLATURE
   *
   * The code in chicory/Instrument and dcomp/DCInstrument were not consistent with
   * respect to variable naming.  They used different terms for the same item as well
   * as the same term for different items.  I have tried to modify the code to be consistent,
   * but I am sure I have missed some poorly named variables.
   *
   * The main issue is with the terms 'index' and 'offset'.
   * My intent is the following:
   *
   * 'index' is an item's subscript into a data structure.
   *
   * 'offset' is an item's runtime address as an offset (for example)
   * from the start of a method's byte codes or from the start
   * of a method's stack frame. The Java Virtual Machine Specification
   * uses 'index into the local variable array of the current frame'
   * or 'slot number' to describe this later case.
   *
   * Unfortunately, BCEL uses the method names getIndex and setIndex
   * to refer to 'offset's into the local stack frame.
   * It uses getPosition and setPosition to refer to 'offset's into
   * the byte codes.
   */

  /** The pool for the method currently being processed. Must be set by the client. */
  protected /*@Nullable*/ ConstantPoolGen pool = null;

  protected SimpleLog debug_instrument = new SimpleLog(false);
  protected boolean needStackMap = false;

  /** Working copy of StackMapTable; set by fetch_current_stack_map_table. */
  protected StackMapEntry /*@Nullable*/ [] stack_map_table = null;

  /** Original stack map table attribute; set by fetch_current_stack_map_table. */
  protected /*@Nullable*/ StackMap smta = null;

  /** Initial state of StackMapTypes for locals on method entry. */
  protected StackMapType[] initial_type_list;

  protected int initial_locals_count;

  protected int number_active_locals;
  protected int running_offset;

  /**
   * The index of the first 'true' local in the local variable table. (after 'this' and any
   * parameters)
   */
  protected int first_local_index;

  private StackMapEntry[] empty_stack_map_table = {};
  private Map<InstructionHandle, Integer> uninitialized_NEW_map =
      new HashMap<InstructionHandle, Integer>();

  /**
   * Returns a String array with new_string added to the end of arr.
   *
   * @param arr original string array
   * @param new_string string to be added
   * @return the new string array
   */
  protected String[] add_string(String[] arr, String new_string) {
    String[] new_arr = new String[arr.length + 1];
    for (int ii = 0; ii < arr.length; ii++) {
      new_arr[ii] = arr[ii];
    }
    new_arr[arr.length] = new_string;
    return new_arr;
  }

  /**
   * Return the attribute name for the specified attribute.
   *
   * @param a the attribute
   * @return the attribute name for the specified attribute
   */
  protected final String get_attribute_name(Attribute a) {
    int con_index = a.getNameIndex();
    Constant c = pool.getConstant(con_index);
    String att_name = ((ConstantUtf8) c).getBytes();
    return att_name;
  }

  /**
   * Returns whether or not the specified attribute is a LocalVariableTypeTable.
   *
   * @param a the attribute
   * @return true iff the attribute is a LocalVariableTypeTable
   */
  /*@Pure*/
  protected final boolean is_local_variable_type_table(Attribute a) {
    return (get_attribute_name(a).equals("LocalVariableTypeTable"));
  }

  /**
   * Returns whether or not the specified attribute is a StackMapTable.
   *
   * @param a the attribute
   * @return true iff the attribute is a StackMapTable
   */
  /*@Pure*/
  protected final boolean is_stack_map_table(Attribute a) {
    return (get_attribute_name(a).equals("StackMapTable"));
  }

  /**
   * Find the StackMapTable attribute for a method. Return null if there isn't one.
   *
   * @param mgen the method
   * @return the StackMapTable attribute for the method (or null if not present)
   */
  protected final /*@Nullable*/ Attribute get_stack_map_table_attribute(MethodGen mgen) {
    for (Attribute a : mgen.getCodeAttributes()) {
      if (is_stack_map_table(a)) {
        return a;
      }
    }
    return null;
  }

  /**
   * Find the LocalVariableTypeTable attribute for a method. Return null if there isn't one.
   *
   * @param mgen the method
   * @return the LocalVariableTypeTable attribute for the method (or null if not present)
   */
  protected final /*@Nullable*/ Attribute get_local_variable_type_table_attribute(MethodGen mgen) {
    for (Attribute a : mgen.getCodeAttributes()) {
      if (is_local_variable_type_table(a)) {
        return a;
      }
    }
    return null;
  }

  /**
   * Remove the local variable type table attribute (LVTT) from mgen. Some instrumentation changes
   * require this to be updated, but without BCEL support that would be hard to do. It should be
   * safe to just delete it since it is optional and really only of use to a debugger.
   *
   * @param mgen the method to clear out
   */
  protected final void remove_local_variable_type_table(MethodGen mgen) {
    mgen.removeLocalVariableTypeTable();
  }

  /**
   * We have inserted additional byte(s) into the instruction list; update the StackMaps, if
   * required. Also sets running_offset.
   *
   * @param position the location of insertion
   * @param delta the size of the insertion
   */
  protected final void update_stack_map_offset(int position, int delta) {

    running_offset = -1; // no +1 on first entry
    for (int i = 0; i < stack_map_table.length; i++) {
      running_offset = stack_map_table[i].getByteCodeOffset() + running_offset + 1;

      if (running_offset > position) {
        stack_map_table[i].updateByteCodeOffset(delta);
        // Only update the first StackMap that occurs after the given
        // offset as map offsets are relative to previous map entry.
        return;
      }
    }
  }

  /**
   * Find the StackMap entry who's offset matches the input argument. Also sets running_offset.
   *
   * @param offset byte code offset
   * @return the corresponding StackMapEntry
   */
  protected final StackMapEntry find_stack_map_equal(int offset) {

    running_offset = -1; // no +1 on first entry
    for (int i = 0; i < stack_map_table.length; i++) {
      running_offset = stack_map_table[i].getByteCodeOffset() + running_offset + 1;

      if (running_offset > offset) {
        throw new RuntimeException("Invalid StackMap offset 1");
      }

      if (running_offset == offset) {
        return stack_map_table[i];
      }
      // try next map entry
    }

    // no offset matched
    throw new RuntimeException("Invalid StackMap offset 2");
  }

  /**
   * Find the index of the StackMap entry who's offset is the last one before the input argument.
   * Return -1 if there isn't one. Also sets running_offset and number_active_locals.
   *
   * @param offset byte code offset
   * @return the corresponding StackMapEntry index
   */
  protected final int find_stack_map_index_before(int offset) {

    number_active_locals = initial_locals_count;
    running_offset = -1; // no +1 on first entry
    for (int i = 0; i < stack_map_table.length; i++) {
      running_offset = running_offset + stack_map_table[i].getByteCodeOffset() + 1;
      if (running_offset >= offset) {
        if (i == 0) {
          // reset offset to previous
          running_offset = -1;
          return -1;
        } else {
          // back up offset to previous
          running_offset = running_offset - stack_map_table[i].getByteCodeOffset() - 1;
          // return previous
          return (i - 1);
        }
      }

      // Update number of active locals based on this StackMap entry.
      int frame_type = stack_map_table[i].getFrameType();
      if (frame_type >= Const.APPEND_FRAME && frame_type <= Const.APPEND_FRAME_MAX) {
        number_active_locals += frame_type - 251;
      } else if (frame_type >= Const.CHOP_FRAME && frame_type <= Const.CHOP_FRAME_MAX) {
        number_active_locals -= 251 - frame_type;
      } else if (frame_type == Const.FULL_FRAME) {
        number_active_locals = stack_map_table[i].getNumberOfLocals();
      }
      // All other frame_types do not modify locals.
    }

    if (stack_map_table.length == 0) {
      return -1;
    } else {
      return stack_map_table.length - 1;
    }
  }

  /**
   * Find the index of the StackMap entry who's offset is the first one after the input argument.
   * Return -1 if there isn't one. Also sets running_offset.
   *
   * @param offset byte code offset
   * @return the corresponding StackMapEntry index
   */
  protected final int find_stack_map_index_after(int offset) {

    running_offset = -1; // no +1 on first entry
    for (int i = 0; i < stack_map_table.length; i++) {
      running_offset = running_offset + stack_map_table[i].getByteCodeOffset() + 1;
      if (running_offset > offset) {
        return i;
      }
      // try next map entry
    }

    // no such entry found
    return -1;
  }

  /**
   * Check to see if there have been any changes in a switch statement's padding bytes. If so, we
   * need to update the corresponding StackMap.
   *
   * @param ih where to start looking for a switch instruction
   * @param il instruction list to search
   */
  protected final void modify_stack_maps_for_switches(InstructionHandle ih, InstructionList il) {
    Instruction inst;
    short opcode;

    if (!needStackMap) return;

    // Make sure all instruction offsets are uptodate.
    il.setPositions();

    // Loop through each instruction looking for a switch
    while (ih != null) {
      inst = ih.getInstruction();
      opcode = inst.getOpcode();

      if (opcode == Const.TABLESWITCH || opcode == Const.LOOKUPSWITCH) {
        int current_offset = ih.getPosition();
        int index = find_stack_map_index_after(current_offset);
        if (index == -1) {
          throw new RuntimeException("Invalid StackMap offset 3");
        }
        StackMapEntry stack_map = stack_map_table[index];
        int delta = (current_offset + inst.getLength()) - running_offset;
        if (delta != 0) {
          stack_map.updateByteCodeOffset(delta);
        }
        // Since StackMap offsets are relative to the previous one
        // we only have to do the first one after a switch.
        // But we do need to look at all the switch instructions.
      }

      // Go on to the next instruction in the list
      ih = ih.getNext();
    }
  }

  /**
   * Find the live range of the compiler temp(s) at the given offset and create a LocalVariableGen
   * for each. Note the compiler might generate temps of different sizes at the same offset (must
   * have disjoint lifetimes).
   *
   * @param mgen the method
   * @param offset compiler assigned local offset of hidden temp
   * @return offset incremented by size of smallest temp found at offset
   */
  protected final int gen_temp_locals(MethodGen mgen, int offset) {
    int live_start = 0;
    Type live_type = null;
    InstructionList il = mgen.getInstructionList();
    il.setPositions();

    // Set up inital state of StackMap info on entry to method.
    int locals_offset_height = 0;
    int byte_code_offset = -1;
    LocalVariableGen new_lvg;
    int min_size = 3; // only sizes are 1 or 2; start with something larger.

    number_active_locals = initial_locals_count;
    StackMapType[] types_of_active_locals = new StackMapType[number_active_locals];
    for (int ii = 0; ii < number_active_locals; ii++) {
      types_of_active_locals[ii] = initial_type_list[ii];
      locals_offset_height += getSize(initial_type_list[ii]);
    }

    // update state for each StackMap entry
    for (StackMapEntry smte : stack_map_table) {
      int frame_type = smte.getFrameType();
      byte_code_offset += smte.getByteCodeOffset() + 1;

      if (frame_type >= Const.APPEND_FRAME && frame_type <= Const.APPEND_FRAME_MAX) {
        // number to append is frame_type - 251
        types_of_active_locals =
            Arrays.copyOf(types_of_active_locals, number_active_locals + frame_type - 251);
        for (StackMapType smt : smte.getTypesOfLocals()) {
          types_of_active_locals[number_active_locals++] = smt;
          locals_offset_height += getSize(smt);
        }
      } else if (frame_type >= Const.CHOP_FRAME && frame_type <= Const.CHOP_FRAME_MAX) {
        int number_to_chop = 251 - frame_type;
        while (number_to_chop > 0) {
          locals_offset_height -= getSize(types_of_active_locals[--number_active_locals]);
          number_to_chop--;
        }
        types_of_active_locals = Arrays.copyOf(types_of_active_locals, number_active_locals);
      } else if (frame_type == Const.FULL_FRAME) {
        locals_offset_height = 0;
        number_active_locals = 0;
        types_of_active_locals = new StackMapType[smte.getNumberOfLocals()];
        for (StackMapType smt : smte.getTypesOfLocals()) {
          types_of_active_locals[number_active_locals++] = smt;
          locals_offset_height += getSize(smt);
        }
      }
      // all other frame_types do not modify locals.

      //System.out.printf("byte_code_offset: %d, temp offset: %d, locals_offset_height: %d, number_active_locals: %d, local types: %s%n",
      //       byte_code_offset, offset, locals_offset_height, number_active_locals, Arrays.toString(types_of_active_locals));

      if (live_start == 0) {
        // did the latest StackMap entry define the temp in question?
        if (offset < locals_offset_height) {
          live_start = byte_code_offset;
          int running_offset = 0;
          for (StackMapType smt : types_of_active_locals) {
            if (running_offset == offset) {
              live_type = generate_Type_from_StackMapType(smt);
              break;
            }
            running_offset += getSize(smt);
          }
          if (live_type == null) {
            // No matching offset in stack maps so this offset must be
            // second half of long or double. Just skip to next stack map.
            live_start = 0;
          }
        }
      } else {
        // did the latest StackMap entry undefine the temp in question?
        if (offset >= locals_offset_height) {
          // create the temp variable
          new_lvg =
              mgen.addLocalVariable(
                  "DaIkOnTeMp" + offset,
                  live_type,
                  offset,
                  il.findHandle(live_start),
                  il.findHandle(byte_code_offset));
          debug_instrument.log(
              "Added local  %s, %d, %d : %s, %s%n",
              new_lvg.getIndex(),
              new_lvg.getStart().getPosition(),
              new_lvg.getEnd().getPosition(),
              new_lvg.getName(),
              new_lvg.getType());
          min_size = Math.min(min_size, live_type.getSize());
          // reset to look for more temps at same offset
          live_start = 0;
          live_type = null;
        }
      }
      // go on to next StackMap entry
    }
    // we are done with stack maps; need to see if any temps still active
    if (live_start != 0) {
      // live range is to end of method; create the temp variable
      new_lvg =
          mgen.addLocalVariable(
              "DaIkOnTeMp" + offset, live_type, offset, il.findHandle(live_start), null);
      debug_instrument.log(
          "Added local  %s, %d, %d : %s, %s%n",
          new_lvg.getIndex(),
          new_lvg.getStart().getPosition(),
          il.getEnd().getPosition(),
          new_lvg.getName(),
          new_lvg.getType());
      min_size = Math.min(min_size, live_type.getSize());
    } else {
      if (min_size == 3) {
        // did not find a temp in any of the stack maps; that must mean the
        // temp live range is in between two stack maps or after last stack map
        // need to scan all byte codes to get live range and type
        // HACK: for now, just make a guess and put it after last stack map
        // Note we may be defining a bogus temp for second half of long or double.
        if (byte_code_offset == -1) {
          // no stack maps at all, set start to 0
          byte_code_offset = 0;
        }
        new_lvg =
            mgen.addLocalVariable(
                "DaIkOnTeMp" + offset, Type.OBJECT, offset, il.findHandle(byte_code_offset), null);
        debug_instrument.log(
            "Added local  %s, %d, %d : %s, %s%n",
            new_lvg.getIndex(),
            new_lvg.getStart().getPosition(),
            il.getEnd().getPosition(),
            new_lvg.getName(),
            new_lvg.getType());
        min_size = Math.min(min_size, Type.OBJECT.getSize());
      }
    }
    return offset + min_size;
  }

  /**
   * We need to locate and remember any NEW instructions that create uninitialized objects. Their
   * offset may be contained in a StackMap entry and will probably need to be updated as we add
   * instrumentation code. Note that these instructions are fairly rare.
   *
   * @param il instruction list to search
   */
  protected final void build_unitialized_NEW_map(InstructionList il) {

    uninitialized_NEW_map.clear();
    il.setPositions();

    for (StackMapEntry smte : stack_map_table) {
      int frame_type = smte.getFrameType();

      if ((frame_type >= Const.APPEND_FRAME && frame_type <= Const.APPEND_FRAME_MAX)
          || (frame_type == Const.FULL_FRAME)) {

        if (smte.getNumberOfLocals() > 0) {
          for (StackMapType smt : smte.getTypesOfLocals()) {
            if (smt.getType() == Const.ITEM_NewObject) {
              int i = smt.getIndex();
              uninitialized_NEW_map.put(il.findHandle(i), i);
            }
          }
        }
        if (smte.getNumberOfStackItems() > 0) {
          for (StackMapType smt : smte.getTypesOfStackItems()) {
            if (smt.getType() == Const.ITEM_NewObject) {
              int i = smt.getIndex();
              uninitialized_NEW_map.put(il.findHandle(i), i);
            }
          }
        }
      }
    }
  }

  /**
   * One of these special NEW instructions has moved. Update it's offset in StackMap entries. Note
   * that more than one entry could refer to the same instruction.
   *
   * @param old_offset original location of NEW instruction
   * @param new_offset new location of NEW instruction
   */
  private final void update_NEW_object_stack_map_entries(int old_offset, int new_offset) {

    for (StackMapEntry smte : stack_map_table) {
      int frame_type = smte.getFrameType();

      if ((frame_type >= Const.APPEND_FRAME && frame_type <= Const.APPEND_FRAME_MAX)
          || (frame_type == Const.FULL_FRAME)) {

        if (smte.getNumberOfLocals() > 0) {
          for (StackMapType smt : smte.getTypesOfLocals()) {
            if (smt.getType() == Const.ITEM_NewObject) {
              if (old_offset == smt.getIndex()) {
                smt.setIndex(new_offset);
              }
            }
          }
        }
        if (smte.getNumberOfStackItems() > 0) {
          for (StackMapType smt : smte.getTypesOfStackItems()) {
            if (smt.getType() == Const.ITEM_NewObject) {
              if (old_offset == smt.getIndex()) {
                smt.setIndex(new_offset);
              }
            }
          }
        }
      }
    }
  }

  /**
   * Check to see if any of these special NEW instructions has moved. Again, these are rare, so
   * linear pass is fine.
   *
   * @param il instruction list to search
   */
  protected final void update_uninitialized_NEW_offsets(InstructionList il) {

    il.setPositions();

    for (Map.Entry<InstructionHandle, Integer> e : uninitialized_NEW_map.entrySet()) {
      InstructionHandle ih = e.getKey();
      int old_offset = e.getValue().intValue();
      int new_offset = ih.getPosition();
      if (old_offset != new_offset) {
        update_NEW_object_stack_map_entries(old_offset, new_offset);
        e.setValue(new_offset);
      }
    }
  }

  /**
   * Process the instruction list, adding size (1 or 2) to the index of each Instruction that
   * references a local that is equal or higher in the local map than index_first_moved_local. Size
   * should be the size of the new local that was just inserted at index_first_moved_local.
   *
   * @param mgen MethodGen to be modified
   * @param index_first_moved_local original index of first local moved "up"
   * @param size size of new local added (1 or 2)
   */
  protected final void adjust_code_for_locals_change(
      MethodGen mgen, int index_first_moved_local, int size) {

    InstructionList il = mgen.getInstructionList();
    for (InstructionHandle ih = il.getStart(); ih != null; ih = ih.getNext()) {
      Instruction inst = ih.getInstruction();
      int orig_length = inst.getLength();
      int operand;

      if ((inst instanceof RET) || (inst instanceof IINC)) {
        IndexedInstruction index_inst = (IndexedInstruction) inst;
        if (index_inst.getIndex() >= index_first_moved_local) {
          index_inst.setIndex(index_inst.getIndex() + size);
        }
      } else if (inst instanceof LocalVariableInstruction) {
        // BCEL handles all the details of which opcode and if index
        // is implicit or explicit; also, and if needs to be WIDE.
        operand = ((LocalVariableInstruction) inst).getIndex();
        if (operand >= index_first_moved_local) {
          ((LocalVariableInstruction) inst).setIndex(operand + size);
        }
      }
      // Unfortunately, BCEL doesn't take care of incrementing the
      // offset within StackMapEntrys.
      int delta = inst.getLength() - orig_length;
      if (delta > 0) {
        il.setPositions();
        update_stack_map_offset(ih.getPosition(), delta);
        modify_stack_maps_for_switches(ih, il);
      }
    }
  }

  /**
   * Get existing StackMapTable (if present); otherwise creates a new empty one. Sets both smta and
   * stack_map_table. Must be called prior to any other methods that manipulate the stack_map_table.
   *
   * @param mgen MethodGen to search
   * @param java_class_version Java version for the classfile; stack_map_table is optional before
   *     Java 1.7
   */
  /*@EnsuresNonNull({"stack_map_table"})*/
  protected final void fetch_current_stack_map_table(MethodGen mgen, int java_class_version) {

    smta = (StackMap) get_stack_map_table_attribute(mgen);
    if (smta != null) {
      // get a deep copy of the original StackMapTable.
      stack_map_table = ((StackMap) (smta.copy(smta.getConstantPool()))).getStackMap();
      needStackMap = true;

      debug_instrument.log(
          "Attribute tag: %s length: %d nameIndex: %d%n",
          smta.getTag(), smta.getLength(), smta.getNameIndex());
      // Delete existing stack map - we'll add a new one later.
      mgen.removeCodeAttribute(smta);
    } else {
      stack_map_table = empty_stack_map_table;
      if (java_class_version > Const.MAJOR_1_6) {
        needStackMap = true;
      }
    }
    print_stack_map_table("Original");
  }

  /**
   * Print the contents of the StackMapTable to the Debug_instrument log.
   *
   * @param prefix label to display with table
   */
  protected final void print_stack_map_table(String prefix) {

    debug_instrument.log("%nStackMap(%s) %s items:%n", prefix, stack_map_table.length);
    running_offset = -1; // no +1 on first entry
    for (int i = 0; i < stack_map_table.length; i++) {
      running_offset = stack_map_table[i].getByteCodeOffset() + running_offset + 1;
      debug_instrument.log("@%03d %s %n", running_offset, stack_map_table[i]);
    }
  }

  /**
   * Create a new StackMap code attribute from stack_map_table.
   *
   * @param mgen MethodGen to add attribute to
   */
  protected final void create_new_stack_map_attribute(MethodGen mgen) throws IOException {

    if (!needStackMap) return;
    print_stack_map_table("Final");

    // Build new StackMapTable attribute
    StackMap map_table =
        new StackMap(pool.addUtf8("StackMapTable"), 0, null, pool.getConstantPool());
    map_table.setStackMap(stack_map_table);
    mgen.addCodeAttribute(map_table);
  }

  /**
   * Convert a Type name to a Class name.
   *
   * @param t type whose name is to be converted
   */
  @SuppressWarnings("signature") // conversion routine
  protected static /*@ClassGetName*/ String typeToClassGetName(Type t) {

    if (t instanceof ObjectType) {
      return ((ObjectType) t).getClassName();
    } else if (t instanceof BasicType) {
      // Use reserved keyword for basic type rather than signature to
      // avoid conflicts with user defined types.
      return t.toString();
    } else {
      // Array type: just convert '/' to '.'
      return t.getSignature().replace('/', '.');
    }
  }

  /**
   * Convert a Type to a StackMapType.
   *
   * @param t Type to be converted
   * @return result StackMapType
   */
  protected final StackMapType generate_StackMapType_from_Type(Type t) {

    switch (t.getType()) {
      case Const.T_BOOLEAN:
      case Const.T_CHAR:
      case Const.T_BYTE:
      case Const.T_SHORT:
      case Const.T_INT:
        return new StackMapType(Const.ITEM_Integer, -1, pool.getConstantPool());
      case Const.T_FLOAT:
        return new StackMapType(Const.ITEM_Float, -1, pool.getConstantPool());
      case Const.T_DOUBLE:
        return new StackMapType(Const.ITEM_Double, -1, pool.getConstantPool());
      case Const.T_LONG:
        return new StackMapType(Const.ITEM_Long, -1, pool.getConstantPool());
      case Const.T_ARRAY:
      case Const.T_OBJECT:
        return new StackMapType(
            Const.ITEM_Object, pool.addClass(typeToClassGetName(t)), pool.getConstantPool());
        // UNKNOWN seems to be used for Uninitialized objects.
        // The second argument to the constructor should be the code offset
        // of the corresponding 'new' instruction.  Just using 0 for now.
      case Const.T_UNKNOWN:
        return new StackMapType(Const.ITEM_NewObject, 0, pool.getConstantPool());
      default:
        throw new RuntimeException("Invalid type: " + t + t.getType());
    }
  }

  /**
   * Convert a StackMapType to a Type.
   *
   * @param smt StackMapType to be converted
   * @return result Type
   */
  protected final Type generate_Type_from_StackMapType(StackMapType smt) {

    switch (smt.getType()) {
      case Const.ITEM_Integer:
      case Const.ITEM_Bogus: // not sure about this; reresents 'top'?
        return Type.INT;
      case Const.ITEM_Float:
        return Type.FLOAT;
      case Const.ITEM_Double:
        return Type.DOUBLE;
      case Const.ITEM_Long:
        return Type.LONG;
      case Const.ITEM_Object:
        return Type.OBJECT;
      default:
        Thread.dumpStack();
        assert false : "Invalid StackMapType: " + smt + smt.getType();
        throw new RuntimeException("Invalid StackMapType: " + smt + smt.getType());
    }
  }

  /**
   * @param smt a StackMapType object
   * @return operand size of this type (2 for long and double, 1 otherwise)
   */
  protected final int getSize(StackMapType smt) {
    switch (smt.getType()) {
      case Const.ITEM_Double:
      case Const.ITEM_Long:
        return 2;
      default:
        return 1;
    }
  }

  /**
   * Update any FULL_FRAME StackMap entries to include a new local var. The locals array is a copy
   * of the local variables PRIOR to the addition of the new local in question.
   *
   * @param offset offset into stack of the new variable we are adding
   * @param type_new_var type of new variable we are adding
   * @param locals a copy of the local variable table prior to this modification
   */
  protected final void update_full_frame_stack_map_entries(
      int offset, Type type_new_var, LocalVariableGen[] locals) {
    int index; // locals index

    for (int i = 0; i < stack_map_table.length; i++) {
      if (stack_map_table[i].getFrameType() == Const.FULL_FRAME) {

        int num_locals = stack_map_table[i].getNumberOfLocals();
        StackMapType[] new_local_types = new StackMapType[num_locals + 1];
        StackMapType[] old_local_types = stack_map_table[i].getTypesOfLocals();

        // System.out.printf ("update_full_frame %s %s %s %n", offset, num_locals, locals.length);

        for (index = 0; index < num_locals; index++) {
          if (index >= locals.length) {
            // there are hidden compiler temps in map
            break;
          }
          if (locals[index].getIndex() >= offset) {
            // we've reached the point of insertion
            break;
          }
          new_local_types[index] = old_local_types[index];
        }
        new_local_types[index++] = generate_StackMapType_from_Type(type_new_var);
        while (index <= num_locals) {
          new_local_types[index] = old_local_types[index - 1];
          index++;
        }

        stack_map_table[i].setTypesOfLocals(new_local_types);
      }
    }
  }

  /**
   * Create a new argument to the method. This will be added after last current argument and before
   * the first local variable. This might have the side effect of causing us to rewrite the method
   * byte codes to adjust the offsets for the local variables - see below for details.
   *
   * <p>Must call fix_local_variable_table (just once per method) before calling this routine.
   *
   * @param mgen MethodGen to be modified
   * @param arg_name name of new argument
   * @param arg_type type of new argument
   */
  protected final LocalVariableGen add_new_argument(
      MethodGen mgen, String arg_name, Type arg_type) {
    // We add a new argument, after any current ones, and then
    // we need to make a pass over the byte codes to update the local
    // offset values of all the locals we just shifted up.  This may have
    // a 'knock on' effect if we are forced to change an instruction that
    // references implict local #3 to an instruction with an explict
    // reference to local #4 as this would require the insertion of an
    // offset into the byte codes. This means we would need to make an
    // additional pass to update branch targets (no - BCEL does this for
    // us) and the StackMapTable (yes - BCEL should do this, but it doesn't).
    //

    LocalVariableGen arg_new = null;
    // get a copy of the locals before modification
    LocalVariableGen[] locals = mgen.getLocalVariables();
    Type[] arg_types = mgen.getArgumentTypes();
    int new_index = 0;
    int new_offset = 0;

    boolean has_code = (mgen.getInstructionList() != null);

    if (has_code) {
      if (!mgen.isStatic()) {
        // Skip the 'this' pointer argument.
        new_index++;
        new_offset++; // size of 'this' is 1
      }

      if (arg_types.length > 0) {
        LocalVariableGen last_arg;
        ;
        new_index = new_index + arg_types.length;
        last_arg = locals[new_index - 1];
        new_offset = last_arg.getIndex() + (last_arg.getType()).getSize();
      }

      // Insert our new local variable into existing table at 'new_offset'.
      arg_new = mgen.addLocalVariable(arg_name, arg_type, new_offset, null, null);

      // Update the index of the first 'true' local in the local variable table.
      first_local_index++;
    }
    initial_locals_count++;

    // Update the method's argument information.
    arg_types = BCELUtil.postpendToArray(arg_types, arg_type);
    String[] arg_names = add_string(mgen.getArgumentNames(), arg_name);
    mgen.setArgumentTypes(arg_types);
    mgen.setArgumentNames(arg_names);

    if (has_code) {
      // we need to adjust the offset of any locals after our insertion
      for (int i = new_index; i < locals.length; i++) {
        LocalVariableGen lv = locals[i];
        lv.setIndex(lv.getIndex() + arg_type.getSize());
      }
      mgen.setMaxLocals(mgen.getMaxLocals() + arg_type.getSize());

      debug_instrument.log(
          "Added arg    %s%n",
          arg_new.getIndex() + ": " + arg_new.getName() + ", " + arg_new.getType());

      // Now process the instruction list, adding one to the offset
      // within each LocalVariableInstruction that references a
      // local that is 'higher' in the local map than new local
      // we just inserted.
      adjust_code_for_locals_change(mgen, new_offset, arg_type.getSize());

      // Finally, we need to update any FULL_FRAME StackMap entries to
      // add in the new local variable type.
      update_full_frame_stack_map_entries(new_offset, arg_type, locals);

      debug_instrument.log("New LocalVariableTable:%n%s%n", mgen.getLocalVariableTable(pool));
    }
    return arg_new;
  }

  /**
   * Create a new local with a scope of the full method. This means we need to search the existing
   * locals to find the proper index for our new local. This might have the side effect of causing
   * us to rewrite the method byte codes to adjust the offsets for the existing local variables -
   * see below for details.
   *
   * <p>Must call fix_local_variable_table (just once per method) before calling this routine.
   *
   * @param mgen MethodGen to be modified
   * @param local_name name of new local
   * @param local_type type of new local
   */
  protected final LocalVariableGen create_method_scope_local(
      MethodGen mgen, String local_name, Type local_type) {
    // BCEL sorts local vars and presents them in offset order.  Search
    // locals for first var with start != 0. If none, just add the new
    // var at the end of the table and exit. Otherwise, insert the new
    // var just prior to the local we just found.
    // Now we need to make a pass over the byte codes to update the local
    // offset values of all the locals we just shifted up.  This may have
    // a 'knock on' effect if we are forced to change an instruction that
    // references implict local #3 to an instruction with an explict
    // reference to local #4 as this would require the insertion of an
    // offset into the byte codes. This means we would need to make an
    // additional pass to update branch targets (no - BCEL does this for
    // us) and the StackMapTable (yes - BCEL should do this, but it doesn't).
    //
    // We never want to insert our local prior to any parameters.  This would
    // happen naturally, but some old class files have non zero addresses
    // for 'this' and/or the parameters so we need to add an explicit
    // check to make sure we skip these variables.

    LocalVariableGen lv_new;
    int max_offset = 0;
    int new_offset = -1;
    // get a copy of the local before modification
    LocalVariableGen[] locals = mgen.getLocalVariables();
    int compiler_temp_i = -1;
    int new_index = -1;
    int i;

    for (i = 0; i < locals.length; i++) {
      LocalVariableGen lv = locals[i];
      if (i >= first_local_index) {
        if (lv.getStart().getPosition() != 0) {
          if (new_offset == -1) {
            if (compiler_temp_i != -1) {
              new_offset = locals[compiler_temp_i].getIndex();
              new_index = compiler_temp_i;
            } else {
              new_offset = lv.getIndex();
              new_index = i;
            }
          }
        }
      }

      // calculate max local size seen so far (+1)
      max_offset = lv.getIndex() + lv.getType().getSize();

      if (lv.getName().startsWith("DaIkOnTeMp")) {
        // Remember the index of a compiler temp.  We may wish
        // to insert our new local prior to a temp to simplfy
        // the generation of StackMaps.
        if (compiler_temp_i == -1) {
          compiler_temp_i = i;
        }
      } else {
        // If there were compiler temps prior to this local, we don't
        // need to worry about them as the compiler will have already
        // included them in the StackMaps. Reset the indicators.
        compiler_temp_i = -1;
      }
    }

    // We have looked at all the local variables; if we have not found
    // a place for our new local, check to see if last local was a
    // compiler temp and insert prior.
    if ((new_offset == -1) && (compiler_temp_i != -1)) {
      new_offset = locals[compiler_temp_i].getIndex();
      new_index = compiler_temp_i;
    }

    // If new_offset is still unset, we can just add our local at the
    // end.  There may be unnamed compiler temps here, so we need to
    // check for this (via max_offset) and move them up.
    if (new_offset == -1) {
      new_offset = max_offset;
      if (new_offset < mgen.getMaxLocals()) {
        mgen.setMaxLocals(mgen.getMaxLocals() + local_type.getSize());
      }
      lv_new = mgen.addLocalVariable(local_name, local_type, new_offset, null, null);
    } else {
      // insert our new local variable into existing table at 'new_offset'
      lv_new = mgen.addLocalVariable(local_name, local_type, new_offset, null, null);
      // we need to adjust the offset of any locals after our insertion
      for (i = new_index; i < locals.length; i++) {
        LocalVariableGen lv = locals[i];
        lv.setIndex(lv.getIndex() + local_type.getSize());
      }
      mgen.setMaxLocals(mgen.getMaxLocals() + local_type.getSize());
    }

    debug_instrument.log(
        "Added local  %s%n", lv_new.getIndex() + ": " + lv_new.getName() + ", " + lv_new.getType());

    // Now process the instruction list, adding one to the offset
    // within each LocalVariableInstruction that references a
    // local that is 'higher' in the local map than new local
    // we just inserted.
    adjust_code_for_locals_change(mgen, new_offset, local_type.getSize());

    // Finally, we need to update any FULL_FRAME StackMap entries to
    // add in the new local variable type.
    update_full_frame_stack_map_entries(new_offset, local_type, locals);

    debug_instrument.log("New LocalVariableTable:%n%s%n", mgen.getLocalVariableTable(pool));
    return lv_new;
  }

  /**
   * Under some circumstances, there may be problems with the local variable table.
   *
   * <ol>
   *   <li>In some special cases where parameters are added by the Java compiler (eg, constructors
   *       for inner classes), the local variable table is missing the entry for this additional
   *       parameter.
   *   <li>The Java compiler allocates unnamed local temps for:
   *       <ul>
   *         <li>saving the exception in a finally clause
   *         <li>the lock for a synchronized block
   *         <li>interators
   *         <li>(others?)
   *       </ul>
   *       We will create a 'fake' local for these cases.
   * </ol>
   *
   * @param mgen MethodGen to be modified
   */
  protected final void fix_local_variable_table(MethodGen mgen) {
    InstructionList il = mgen.getInstructionList();
    if (il == null) {
      // no code so nothing to do
      first_local_index = 0;
      return;
    }

    // Get the current local variables (includes 'this' and parameters)
    LocalVariableGen[] locals = mgen.getLocalVariables();
    LocalVariableGen l;
    LocalVariableGen new_lvg;

    // We need a deep copy
    for (int ii = 0; ii < locals.length; ii++) {
      locals[ii] = (LocalVariableGen) (locals[ii].clone());
    }

    // The arg types are correct and include all parameters.
    Type[] arg_types = mgen.getArgumentTypes();

    // Initial offset into the stack frame
    int offset = 0;

    // Index into locals of the first parameter
    int loc_index = 0;

    // Rarely, the java compiler gets the max locals count wrong (too big).
    // This would cause problems for us later, so we need to recalculate
    // the highest local used based on looking at code offsets.
    mgen.setMaxLocals();
    int max_locals = mgen.getMaxLocals();
    // Remove the existing locals
    mgen.removeLocalVariables();
    // Reset MaxLocals to 0 and let code below rebuild it.
    mgen.setMaxLocals(0);

    // Determine the first 'true' local index into the local variables.
    // The object 'this' pointer and the parameters form the first n
    // entries in the list.
    first_local_index = arg_types.length;

    if (!mgen.isStatic()) {
      // Add the 'this' pointer argument back in.
      l = locals[0];
      new_lvg = mgen.addLocalVariable(l.getName(), l.getType(), l.getIndex(), null, null);
      debug_instrument.log(
          "Added <this> %s%n",
          new_lvg.getIndex() + ": " + new_lvg.getName() + ", " + new_lvg.getType());
      loc_index = 1;
      offset = 1;
      first_local_index++;
    }

    // Loop through each argument
    for (int ii = 0; ii < arg_types.length; ii++) {

      // If this parameter doesn't have a matching local
      if ((loc_index >= locals.length) || (offset != locals[loc_index].getIndex())) {

        // Create a local variable to describe the missing argument
        new_lvg = mgen.addLocalVariable("$hidden$" + offset, arg_types[ii], offset, null, null);
      } else {
        l = locals[loc_index];
        new_lvg = mgen.addLocalVariable(l.getName(), l.getType(), l.getIndex(), null, null);
        loc_index++;
      }
      debug_instrument.log(
          "Added param  %s%n",
          new_lvg.getIndex() + ": " + new_lvg.getName() + ", " + new_lvg.getType());
      offset += arg_types[ii].getSize();
    }

    // At this point the LocalVaraibles contain:
    //   the 'this' pointer (if present)
    //   the arguments to the method
    // This will be used to construct the initial state of the
    // StackMapTypes.
    LocalVariableGen[] initial_locals = mgen.getLocalVariables();
    initial_locals_count = initial_locals.length;
    initial_type_list = new StackMapType[initial_locals_count];
    for (int ii = 0; ii < initial_locals_count; ii++) {
      initial_type_list[ii] = generate_StackMapType_from_Type(initial_locals[ii].getType());
    }

    // Add back the true locals
    //
    // NOTE that the Java compiler uses unnamed local temps for:
    //   saving the exception in a finally clause
    //   the lock for a synchronized block
    //   iterators
    //   (others?)
    // We will create a 'fake' local for these cases.

    for (int ii = first_local_index; ii < locals.length; ii++) {
      l = locals[ii];
      if (l.getIndex() > offset) {
        // A gap in index values indicates a compiler allocated temp.
        // (if offset is 0, probably a lock object)
        // there is at least one hidden compiler temp before the next local
        offset = gen_temp_locals(mgen, offset);
        ii--; // need to revisit same local
      } else {
        new_lvg =
            mgen.addLocalVariable(l.getName(), l.getType(), l.getIndex(), l.getStart(), l.getEnd());
        debug_instrument.log(
            "Added local  %s%n",
            new_lvg.getIndex() + ": " + new_lvg.getName() + ", " + new_lvg.getType());
        offset = new_lvg.getIndex() + (new_lvg.getType()).getSize();
      }
    }

    // check for hidden temps after last declared local.
    while (offset < max_locals) {
      offset = gen_temp_locals(mgen, offset);
    }

    // Recalculate the highest local used based on looking at code offsets.
    mgen.setMaxLocals();
  }
}

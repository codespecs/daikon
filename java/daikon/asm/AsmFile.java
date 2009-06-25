package daikon.asm;

import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;


/**
 * Stores the instructions associated with an asm file.
 *
 * An asm file consists of a list of records. Each record contains assembly
 * instructions corresponding to a basic block. Each record has the form
 *
 * ppt <name>
 *  <instruction>
 *  ...
 *  <instruction>
 *
 * Two recods are separated by one or more empty lines.
 * Each record represents a basic block, and contains its name followed by the
 * list of instructions comprising the block. The list of instructions can be
 * empty.
 */
public class AsmFile {

    private Map<String, List<X86Instruction>> instructionsForBlock
        = new LinkedHashMap<String, List<X86Instruction>>();

    public static AsmFile getAsmFile(String fileName) {
        try {
            FileReader fileReader = new FileReader(fileName);
            LineNumberReader reader = new LineNumberReader(fileReader);
            return new AsmFile(reader);
        } catch (IOException e) {
            System.out.println("I/O error while reading assemblies.");
            throw new RuntimeException(e);
        }
    }

    // Used for testing.
    public AsmFile(LineNumberReader reader) throws IOException {
        while (moveToNextNonEmptyLine(reader) != false) {
            readOneBlock(reader);
        }
    }

    private static final String errorMessage1 = "Record does not start with string \"ppt \".";
    private static final String errorMessage2 = "End of file reached while reading record name.";
    private static final String errorMessage3 = "A record name must end with \"BB\".";
    private static final String errorMessage4 = "A record cannot have an empty name.";
    private static final String errorMessage5 = "End of file reached before reading first instruction.";

    // Precondition: the reader is positioned at the first
    //               non-empty line of the record.
    private void readOneBlock(LineNumberReader reader) throws IOException {

        String name = null;
        List<X86Instruction> instructions = new ArrayList<X86Instruction>();

        // Read "BASIC BLOCK"
        String line = reader.readLine();
        if (line == null || !line.startsWith("ppt ")) { // Malformed record file.
            System.out.println(line);
            throw new RuntimeException(parseError(reader.getLineNumber(),
                                                  errorMessage1));
        }

        // Read name.
        line = line.substring(4);
        if (line == null) // Malformed record file.
            throw new RuntimeException(parseError(reader.getLineNumber(),
                                                  errorMessage2));
        if (!line.endsWith("BB"))
            throw new RuntimeException(parseError(reader.getLineNumber(),
                                                  errorMessage3));

        if (line.trim().length() == 0) // Name should not be empty.
            throw new RuntimeException(parseError(reader.getLineNumber(),
                                                  errorMessage4));
        name = line;

        // Read instructions
        boolean isFirstInstruction = true;
        line = reader.readLine();
        if (line == null)
            throw new RuntimeException(parseError(reader.getLineNumber(),
                                                  errorMessage5));
        while (line != null && (line.length() != 0)) {
            X86Instruction instr = X86Instruction.parseInstruction(line);
            if (isFirstInstruction) {
              instr.isFirstInBlock = true;
              isFirstInstruction = false;
            }
            instr.owner = name;
            instructions.add(instr);
            line = reader.readLine();
        }

        instructionsForBlock.put(name, instructions);
    }

    private String parseError(int lastLineRead, String errorMessage) {
        return "Parse error (line " + lastLineRead + ") : " + errorMessage;
    }

    private static boolean moveToNextNonEmptyLine(LineNumberReader reader) throws IOException {
        for (;;) {
            reader.mark(1000); // Arbitrary number, hopefully big enough.
            String line = reader.readLine();
            if (line == null)
                return false;
            if (line.trim().length() == 0)
                continue;
            reader.reset();
            return true;
        }
    }

    public List<X86Instruction> getInstructions(String blockShortName) {
        if (! instructionsForBlock.containsKey(blockShortName)) {
            throw new RuntimeException(
              "Assembly file does not contain any instructions for ppt "
              + blockShortName);
        }
        assert instructionsForBlock.containsKey(blockShortName); // XXX checker bug
        return instructionsForBlock.get(blockShortName);
    }

    public int numBasicBlocks() {
        return instructionsForBlock.size();
    }

}

package daikon.tools.runtimechecker;

import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Reader;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import jtb.JavaParser;
import jtb.syntaxtree.*;
import jtb.visitor.TreeDumper;
import jtb.visitor.TreeFormatter;
import utilMDE.Assert;
import utilMDE.UtilMDE;
import daikon.Daikon;
import daikon.FileIO;
import daikon.Global;
import daikon.LogHelper;
import daikon.PptMap;
import daikon.tools.jtb.*;
import daikon.tools.runtimechecker.InstrumentVisitor;
import gnu.getopt.Getopt;
import gnu.getopt.LongOpt;

/**
 * Instruments a file to check invariant violations at runtime. Violated
 * invariants are stored in a list [...]. The control flow of the class remains
 * unchanged from the original.
 *
 * @author Carlos Pacheco
 */
public class InstrumentHandler extends CommandHandler {

    public boolean handles(String command) {
        if (command.equals("instrument")) {
            return true;
        }
        return false;
    }

    private static final String lineSep = System.getProperty("line.separator");

    private static final Logger debug = Logger.getLogger("daikon.tools.runtimechecker.InstrumentHandler");

    // If the --max_invariants_pp option is given, this variable is set
    // to the maximum number of invariants out annotate per program point.
    protected static int maxInvariantsPP = -1;

    private static final String max_invariants_pp_SWITCH = "max_invariants_pp";

    // Whether should print debugging information as it is executing.
    private static String debug_SWITCH = "debug";

    public boolean handle(String[] args) {

        Assert.assertTrue(args[0].equals("instrument"));

        // Create instrumented-classes dir.
        File outputDir = new File("instrumented-classes");

        // If instrumented-classes already exists, tell user to remove it, and
        // exit.
        if (outputDir.exists()) {
            System.err
                    .println("The directory \"instrumented-classes\" already exists. "
                            + "Please remove it before instrumenting.");
            System.exit(1);
        }

        String[] realArgs = new String[args.length - 1];
        for (int i = 0; i < realArgs.length; i++) {
            realArgs[i] = args[i + 1];
        }
        Arguments arguments = readArguments(realArgs);

        System.out.println("Reading invariant file: " + arguments.invFile);
        PptMap ppts = null;
        try {
            ppts = FileIO.read_serialized_pptmap(new File(arguments.invFile),
                    true /* use saved config */);
        } catch (IOException e) {
            System.err
                    .println("While trying to read invariant file, an IOException "
                            + "occurred. Here is the message and stack trace: "
                            + e.getMessage());
            e.printStackTrace();
            return false;
        }

        // Compile original sources (because daikon.tools.jtb.Ast accesses
        // them via reflection).
        //compile(arguments.javaFileNames, "");

        // Create filenames including temp directory and pakage directories.
        List/* ParseResults */parseResults = parse(arguments.javaFileNames);

        // Instrument files
        InstrumentVisitor instrumentVisitor = new InstrumentVisitor(ppts);

        List/* String */instrumentedFileNames = new ArrayList/* String */();

        for (Iterator i = parseResults.iterator(); i.hasNext();) {
            ParseResults oneClass = (ParseResults) i.next();

            System.out.println("Instrumenting " + oneClass.packageName
                    + (oneClass.packageName.equals("") ? "" : ".")
                    + oneClass.className);

            oneClass.root.accept(instrumentVisitor);
            oneClass.root.accept(new TreeFormatter(2, 80));

            File instrumentedFileDir = new File(outputDir.getPath()
                    + File.separator
                    + oneClass.packageName.replaceAll("\\.", File.separator));

            if (!instrumentedFileDir.exists()) {
                instrumentedFileDir.mkdirs();
            }

            String instrumentedFileName = oneClass.className + ".java";

            File instrumentedFile = new File(instrumentedFileDir,
                    instrumentedFileName);

            debug.fine("instrumented file name: " + instrumentedFile.getPath());
            instrumentedFileNames.add(instrumentedFile.getPath());

            try {
                Writer output = new FileWriter(instrumentedFile);

                oneClass.root.accept(new TreeFormatter());
                oneClass.root.accept(new TreeDumper(output));
                output.close();
            } catch (IOException e) {
                System.err
                        .println("While trying to write instrumented file, an IOException "
                                + "occurred. Here is the message and stack trace: "
                                + e.getMessage());
                e.printStackTrace();
                return false;
            }

        }

        return true;
    }

    private static class Arguments {
        public String invFile;

        public List/* String */javaFileNames;
    }

    private Arguments readArguments(String[] args) {

        LongOpt[] longopts = new LongOpt[] {
                new LongOpt(Daikon.debugAll_SWITCH, LongOpt.NO_ARGUMENT, null,
                        0),
                new LongOpt(Daikon.debug_SWITCH, LongOpt.REQUIRED_ARGUMENT,
                        null, 0),
                new LongOpt(max_invariants_pp_SWITCH,
                        LongOpt.REQUIRED_ARGUMENT, null, 0),
                new LongOpt(debug_SWITCH, LongOpt.NO_ARGUMENT, null, 0) };
        Getopt g = new Getopt("daikon.tools.runtimechecker.InstrumentHandler", args, "hs", longopts);
        int c;
        while ((c = g.getopt()) != -1) {
            switch (c) {
            case 0:
                // got a long option
                String option_name = longopts[g.getLongind()].getName();

                if (debug_SWITCH.equals(option_name)) {
                    debug.setLevel(Level.FINE);
                } else if (max_invariants_pp_SWITCH.equals(option_name)) {
                    try {
                        maxInvariantsPP = Integer.parseInt(g.getOptarg());
                    } catch (NumberFormatException e) {
                        System.err
                                .println("Annotate: found the --max_invariants_pp option "
                                        + "followed by an invalid numeric argument. Annotate "
                                        + "will run without the option.");
                        maxInvariantsPP = -1;
                    }
                } else if (Daikon.debugAll_SWITCH.equals(option_name)) {
                    Global.debugAll = true;
                } else if (Daikon.debug_SWITCH.equals(option_name)) {
                    LogHelper.setLevel(g.getOptarg(), LogHelper.FINE);
                } else {
                    throw new RuntimeException("Unknown long option received: "
                            + option_name);
                }
                break;
            case 'h':
                usageMessage();
                System.exit(1);
                break;
            case '?':
                break; // getopt() already printed an error
            default:
                System.out.println("getopt() returned " + c);
                break;
            }
        }
        // The index of the first non-option argument -- the name of the
        // invariant file.
        int argindex = g.getOptind();
        if (argindex >= args.length) {
            System.out
                    .println("Error: No .inv file or .java file arguments supplied.");
            usageMessage();
            System.exit(1);
        }
        String invfile = args[argindex];
        argindex++;
        if (!(invfile.endsWith(".inv") || invfile.endsWith(".inv.gz"))) {
            System.out.println("Error: first argument must be a"
                    + "file ending in .inv or .inv.gz.");
            System.exit(1);
        }
        if (argindex >= args.length) {
            System.out.println("Error: No .java file arguments supplied.");
            usageMessage();
            System.exit(1);
        }
        List/* String */javaFileNames = new ArrayList();
        for (; argindex < args.length; argindex++) {
            String javafile = args[argindex];
            if (!javafile.endsWith(".java")) {
                System.out.println("File does not end in .java: " + javafile);
                System.exit(1);
            }
            javaFileNames.add(javafile);
        }

        Arguments ret = new Arguments();
        ret.invFile = invfile;
        ret.javaFileNames = javaFileNames;
        return ret;
    }

//     private void compile(List/* String */javaFileNames, String extraClasspath) {

//         try {

//             System.out
//                     .println("Executing command: "
//                             + "javac "
//                             + (extraClasspath.equals("") ? ""
//                                     : ("-classpath " + extraClasspath
//                                             + File.pathSeparator + "<original classpath>"))
//                             + " " + UtilMDE.join(javaFileNames, " "));

//             // [[ TODO: let user give extra arguments to this call ]]
//             eclat.util.Command.exec("javac "
//                     + (extraClasspath.equals("") ? "" : ("-classpath "
//                             + extraClasspath + File.pathSeparator + System
//                             .getProperty("java.class.path"))) + " "
//                     + UtilMDE.join(javaFileNames, " "));

//         } catch (Throwable e) {
//             System.err.println("javac command failed.");
//             // [[ TODO; improve error message. ]]
//             System.exit(1);
//         }
//     }

    /**
     * The wrapped result of parsing a .java source file. The packageName and
     * className arguments can be obtained from root, but they are returned here
     * for convenience.
     */
    public static class ParseResults {
        public String packageName;

        public String className;

        public CompilationUnit root;

        public String toString() {
            return "package name: " + packageName + ", " + "class name: "
                    + className;
        }
    }

    /**
     * If one of the files declares an interfaces, an error will occur.
     *
     * This method assumes that each java file contains only one top-level
     * jtb.syntaxTree.TypeDeclaration, and that such declaration matches the
     * name given the source file. (The grammar in jtb.syntaxtree allows for
     * none or multiple type declaration.)
     */
    public static List parse(List/* String */javaFileNames) {

        List/* ParseResults */retval = new ArrayList/* ParseResults */();

        for (Iterator i = javaFileNames.iterator(); i.hasNext();) {
            String javaFileName = (String) i.next();
            ParseResults results = new ParseResults();

            System.out.println("Parsing file " + javaFileName);

            try {
                Reader input = new FileReader(javaFileName);
                JavaParser parser = new JavaParser(input);
                results.root = parser.CompilationUnit();
                input.close();
            } catch (Exception e) {
                e.printStackTrace();
                throw new Error(e);
            }

            // Construct the package name.
            NodeOptional packageDeclarationMaybe = results.root.f0;
            if (packageDeclarationMaybe.present()) {
                PackageDeclaration packageDeclaration = (PackageDeclaration) packageDeclarationMaybe.node;
                Name packageName = packageDeclaration.f1;
                StringWriter stringWriter = new StringWriter();
                TreeDumper dumper = new TreeDumper(stringWriter);
                dumper.visit(packageName);
                results.packageName = stringWriter.toString().trim();
            } else {
                results.packageName = "";
            }

            // Find the class name.
            NodeListOptional typeDeclarationMaybe = results.root.f2;
            Assert.assertTrue(typeDeclarationMaybe.size() == 1,
                    "The runtime-check instrumenter assumes that every source file only defines "
                            + "one top-level class, and this file doesn't: "
                            + javaFileName);
            TypeDeclaration typeDeclaration = (TypeDeclaration) typeDeclarationMaybe
                    .elementAt(0);
            NodeSequence sequence = (NodeSequence)typeDeclaration.f0.choice;
            NodeChoice nodeChoice = (NodeChoice)sequence.elementAt(1);
            ClassOrInterfaceDeclaration decl = (ClassOrInterfaceDeclaration)nodeChoice.choice;

            Assert.assertTrue(!Ast.isInterface(decl),
                              "Do not give .java files that declare interfaces "
                              + "to the instrumenter: " + javaFileName);

            results.className = decl.f1.tokenImage;

            // Add to the list of ParseResults that we'll return.
            retval.add(results);
        }
        debug.fine("Parse results: " + retval.toString());
        return retval;
    }

}

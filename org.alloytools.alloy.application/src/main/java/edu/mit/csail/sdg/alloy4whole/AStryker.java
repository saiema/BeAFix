package edu.mit.csail.sdg.alloy4whole;

import edu.mit.csail.sdg.alloy4.*;
import edu.mit.csail.sdg.ast.Command;
import edu.mit.csail.sdg.translator.A4Options;
import org.alloytools.alloy.core.AlloyCore;

import java.io.File;
import java.io.IOException;
import java.util.*;
import java.util.logging.FileHandler;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

import static edu.mit.csail.sdg.alloy4.A4Preferences.*;

public class AStryker {

    public static final String BEAFIX_VERSION = "2.10.1rc";

    private static final Logger logger = Logger.getLogger(AStryker.class.getName());

    /**
     * The system-specific file separator (forward-slash on UNIX, back-slash on
     * Windows, etc.)
     */
    private static final String   fs                     = System.getProperty("file.separator");

    static {
        try {
            // This block configure the logger with handler and formatter
            FileHandler fh = new FileHandler("AStryker.log");
            logger.addHandler(fh);
            SimpleFormatter formatter = new SimpleFormatter();
            fh.setFormatter(formatter);
        } catch (SecurityException | IOException e) {
            e.printStackTrace();
        }
    }

    private final List<Command> commands;
    private final String sourcefile;
    private final String source;
    private boolean initSuccessful;

    public AStryker(List<Command> commands, String sourcefile, String source) {
        this.commands = commands;
        this.sourcefile = sourcefile;
        this.source = source;
        initSuccessful = false;
    }

    public void init() {
        copyFromJAR();
        final String binary = alloyHome() + fs + "binary";
        // Add the new JNI location to the java.library.path
        try {
            System.setProperty("java.library.path", binary);
            // The above line is actually useless on Sun JDK/JRE (see Sun's bug
            // ID 4280189)
            // The following 4 lines should work for Sun's JDK/JRE (though they
            // probably won't work for others)
            String[] newarray = new String[] {
                    binary
            };
            java.lang.reflect.Field old = ClassLoader.class.getDeclaredField("usr_paths");
            old.setAccessible(true);
            old.set(null, newarray);
        } catch (Throwable ex) {
            ex.printStackTrace();
        }
        initSuccessful = true;
    }

    /**
     * This method executes a reparation on a marked source.
     */

    public void doAStryker(SimpleReporter.SimpleTaskRepair1.ASTRYKER_MODE mode) {
        if (!initSuccessful) {
            logger.severe("Either the initialization was not made or it was not successful");
            return;
        }
        if (WorkerEngine.isBusy())
            return;
        // To update the accelerator to point to the command actually chosen
        //  doRefreshRun();
        if (commands == null)
            return;
        if (commands.size() == 0 ) {
            logger.info("There are no commands for repair validation.\n");
            return;
        }
        SimpleCallback cb = new SimpleCallback(logger);
        cb.setRepairing(true);
        SimpleReporter.SimpleTaskRepair1 repair1 = new SimpleReporter.SimpleTaskRepair1();
        repair1.mode = mode;
        A4Options opt = new A4Options();
        opt.tempDirectory = alloyHome() + fs + "tmp";
        opt.solverDirectory = alloyHome() + fs + "binary";
        opt.recordKodkod = RecordKodkod.get();
        opt.noOverflow = NoOverflow.get();
        opt.unrolls = Version.experimental ? Unrolls.get() : (-1);
        opt.skolemDepth = SkolemDepth.get();
        opt.coreMinimization = CoreMinimization.get();
        opt.inferPartialInstance = InferPartialInstance.get();
        opt.coreGranularity = CoreGranularity.get();
        opt.originalFilename = Util.canon(sourcefile);
        if (checkMiniSatLib()) {
            opt.solver = A4Options.SatSolver.MiniSatJNI;
        } else {
            opt.solver = Solver.get();
        }
        repair1.map = new HashMap<>();
        repair1.map.put(sourcefile, source);
        repair1.options = opt.dup();
        repair1.resolutionMode = (Version.experimental && ImplicitThis.get()) ? 2 : 1;
        repair1.tempdir = maketemp();
        try {
            int newmem = 16384, newstack = 65536;
            if (AlloyCore.isDebug())
                WorkerEngine.runLocally(repair1, cb);
            else
                WorkerEngine.run(repair1, newmem, newstack, alloyHome() + fs + "binary", "", cb);
        } catch (Throwable ex) {
            WorkerEngine.stop();
            logger.severe("Fatal Error: Solver failed due to unknown reason.\n" + "One possible cause is that, in the Options menu, your specified\n" + "memory size is larger than the amount allowed by your OS.\n" + "Also, please make sure \"java\" is in your program path.\n");
        }
    }

    private static boolean checkMiniSatLib() {
        boolean loaded = _loadLibrary("minisat");
        String libName = System.mapLibraryName("minisat");
        if (loaded)
            System.out.println("Loaded: " + libName);
        else
            System.out.println("Failed to load: " + libName);
        return loaded;
    }

    private static boolean _loadLibrary(String library) {
        try {
            System.loadLibrary(library);
            return true;
        } catch (UnsatisfiedLinkError ex) {}
        try {
            System.loadLibrary(library + "x1");
            return true;
        } catch (UnsatisfiedLinkError ex) {}
        try {
            System.loadLibrary(library + "x2");
            return true;
        } catch (UnsatisfiedLinkError ex) {}
        try {
            System.loadLibrary(library + "x3");
            return true;
        } catch (UnsatisfiedLinkError ex) {}
        try {
            System.loadLibrary(library + "x4");
            return true;
        } catch (UnsatisfiedLinkError ex) {}
        try {
            System.loadLibrary(library + "x5");
            return true;
        } catch (UnsatisfiedLinkError ex) {
            return false;
        }
    }


    /**
     * This variable caches the result of alloyHome() function call.
     */
    private static String alloyHome = null;

    /**
     * Find a temporary directory to store Alloy files; it's guaranteed to be a
     * canonical absolute path.
     */
    private static synchronized String alloyHome() {
        if (alloyHome != null)
            return alloyHome;
        String temp = System.getProperty("java.io.tmpdir");
        if (temp == null || temp.length() == 0)
            OurDialog.fatal("Error. JVM need to specify a temporary directory using java.io.tmpdir property.");
        String username = System.getProperty("user.name");
        File tempfile = new File(temp + File.separatorChar + "alloy4tmp40-" + (username == null ? "" : username));
        tempfile.mkdirs();
        String ans = Util.canon(tempfile.getPath());
        if (!tempfile.isDirectory()) {
            OurDialog.fatal("Error. Cannot create the temporary directory " + ans);
        }
        if (!Util.onWindows()) {
            String[] args = {
                    "chmod", "700", ans
            };
            try {
                Runtime.getRuntime().exec(args).waitFor();
            } catch (Throwable ex) {
                ex.printStackTrace();
            } // We only intend to make a best effort.
        }
        return alloyHome = ans;
    }

    /**
     * Create an empty temporary directory for use, designate it "deleteOnExit",
     * then return it. It is guaranteed to be a canonical absolute path.
     */
    private static String maketemp() {
        Random r = new Random(new Date().getTime());
        while (true) {
            int i = r.nextInt(1000000);
            String dest = alloyHome() + File.separatorChar + "tmp" + File.separatorChar + i;
            File f = new File(dest);
            if (f.mkdirs()) {
                f.deleteOnExit();
                return Util.canon(dest);
            }
        }
    }

    /**
     * Copy the required files from the JAR into a temporary directory.
     */
    private void copyFromJAR() {
        // Compute the appropriate platform
        String os = System.getProperty("os.name").toLowerCase(Locale.US).replace(' ', '-');
        if (os.startsWith("mac-"))
            os = "mac";
        else if (os.startsWith("windows-"))
            os = "windows";
        String arch = System.getProperty("os.arch").toLowerCase(Locale.US).replace(' ', '-');
        if (arch.equals("powerpc"))
            arch = "ppc-" + os;
        else
            arch = arch.replaceAll("\\Ai[3456]86\\z", "x86") + "-" + os;
        if (os.equals("mac"))
            arch = "x86-mac"; // our pre-compiled binaries are all universal
        // binaries
        // Find out the appropriate Alloy directory
        final String platformBinary = alloyHome() + fs + "binary";
        // Write a few test files
        try {
            (new File(platformBinary)).mkdirs();
            Util.writeAll(platformBinary + fs + "tmp.cnf", "p cnf 3 1\n1 0\n");
        } catch (Err er) {
            // The error will be caught later by the "berkmin" or "spear" test
        }
        // Copy the platform-dependent binaries
        Util.copy(true, false, platformBinary, arch + "/libminisat.so", arch + "/libminisatx1.so", arch + "/libminisat.jnilib", arch + "/libminisat.dylib", arch + "/libminisatprover.so", arch + "/libminisatproverx1.so", arch + "/libminisatprover.jnilib", arch + "/libminisatprover.dylib", arch + "/libzchaff.so", arch + "/libzchaffmincost.so", arch + "/libzchaffx1.so", arch + "/libzchaff.jnilib", arch + "/liblingeling.so", arch + "/liblingeling.dylib", arch + "/liblingeling.jnilib", arch + "/plingeling", arch + "/libglucose.so", arch + "/libglucose.dylib", arch + "/libglucose.jnilib", arch + "/libcryptominisat.so", arch + "/libcryptominisat.la", arch + "/libcryptominisat.dylib", arch + "/libcryptominisat.jnilib", arch + "/berkmin", arch + "/spear", arch + "/cryptominisat");
        Util.copy(false, false, platformBinary, arch + "/minisat.dll", arch + "/cygminisat.dll", arch + "/libminisat.dll.a", arch + "/minisatprover.dll", arch + "/cygminisatprover.dll", arch + "/libminisatprover.dll.a", arch + "/glucose.dll", arch + "/cygglucose.dll", arch + "/libglucose.dll.a", arch + "/zchaff.dll", arch + "/berkmin.exe", arch + "/spear.exe");
        // Copy the model files
        Util.copy(false, true, alloyHome(), "models/book/appendixA/addressBook1.als", "models/book/appendixA/addressBook2.als", "models/book/appendixA/barbers.als", "models/book/appendixA/closure.als", "models/book/appendixA/distribution.als", "models/book/appendixA/phones.als", "models/book/appendixA/prison.als", "models/book/appendixA/properties.als", "models/book/appendixA/ring.als", "models/book/appendixA/spanning.als", "models/book/appendixA/tree.als", "models/book/appendixA/tube.als", "models/book/appendixA/undirected.als", "models/book/appendixE/hotel.thm", "models/book/appendixE/p300-hotel.als", "models/book/appendixE/p303-hotel.als", "models/book/appendixE/p306-hotel.als", "models/book/chapter2/addressBook1a.als", "models/book/chapter2/addressBook1b.als", "models/book/chapter2/addressBook1c.als", "models/book/chapter2/addressBook1d.als", "models/book/chapter2/addressBook1e.als", "models/book/chapter2/addressBook1f.als", "models/book/chapter2/addressBook1g.als", "models/book/chapter2/addressBook1h.als", "models/book/chapter2/addressBook2a.als", "models/book/chapter2/addressBook2b.als", "models/book/chapter2/addressBook2c.als", "models/book/chapter2/addressBook2d.als", "models/book/chapter2/addressBook2e.als", "models/book/chapter2/addressBook3a.als", "models/book/chapter2/addressBook3b.als", "models/book/chapter2/addressBook3c.als", "models/book/chapter2/addressBook3d.als", "models/book/chapter2/theme.thm", "models/book/chapter4/filesystem.als", "models/book/chapter4/grandpa1.als", "models/book/chapter4/grandpa2.als", "models/book/chapter4/grandpa3.als", "models/book/chapter4/lights.als", "models/book/chapter5/addressBook.als", "models/book/chapter5/lists.als", "models/book/chapter5/sets1.als", "models/book/chapter5/sets2.als", "models/book/chapter6/hotel.thm", "models/book/chapter6/hotel1.als", "models/book/chapter6/hotel2.als", "models/book/chapter6/hotel3.als", "models/book/chapter6/hotel4.als", "models/book/chapter6/mediaAssets.als", "models/book/chapter6/memory/abstractMemory.als", "models/book/chapter6/memory/cacheMemory.als", "models/book/chapter6/memory/checkCache.als", "models/book/chapter6/memory/checkFixedSize.als", "models/book/chapter6/memory/fixedSizeMemory.als", "models/book/chapter6/memory/fixedSizeMemory_H.als", "models/book/chapter6/ringElection.thm", "models/book/chapter6/ringElection1.als", "models/book/chapter6/ringElection2.als", "models/examples/algorithms/dijkstra.als", "models/examples/algorithms/dijkstra.thm", "models/examples/algorithms/messaging.als", "models/examples/algorithms/messaging.thm", "models/examples/algorithms/opt_spantree.als", "models/examples/algorithms/opt_spantree.thm", "models/examples/algorithms/peterson.als", "models/examples/algorithms/ringlead.als", "models/examples/algorithms/ringlead.thm", "models/examples/algorithms/s_ringlead.als", "models/examples/algorithms/stable_mutex_ring.als", "models/examples/algorithms/stable_mutex_ring.thm", "models/examples/algorithms/stable_orient_ring.als", "models/examples/algorithms/stable_orient_ring.thm", "models/examples/algorithms/stable_ringlead.als", "models/examples/algorithms/stable_ringlead.thm", "models/examples/case_studies/INSLabel.als", "models/examples/case_studies/chord.als", "models/examples/case_studies/chord2.als", "models/examples/case_studies/chordbugmodel.als", "models/examples/case_studies/com.als", "models/examples/case_studies/firewire.als", "models/examples/case_studies/firewire.thm", "models/examples/case_studies/ins.als", "models/examples/case_studies/iolus.als", "models/examples/case_studies/sync.als", "models/examples/case_studies/syncimpl.als", "models/examples/puzzles/farmer.als", "models/examples/puzzles/farmer.thm", "models/examples/puzzles/handshake.als", "models/examples/puzzles/handshake.thm", "models/examples/puzzles/hanoi.als", "models/examples/puzzles/hanoi.thm", "models/examples/systems/file_system.als", "models/examples/systems/file_system.thm", "models/examples/systems/javatypes_soundness.als", "models/examples/systems/lists.als", "models/examples/systems/lists.thm", "models/examples/systems/marksweepgc.als", "models/examples/systems/views.als", "models/examples/toys/birthday.als", "models/examples/toys/birthday.thm", "models/examples/toys/ceilingsAndFloors.als", "models/examples/toys/ceilingsAndFloors.thm", "models/examples/toys/genealogy.als", "models/examples/toys/genealogy.thm", "models/examples/toys/grandpa.als", "models/examples/toys/grandpa.thm", "models/examples/toys/javatypes.als", "models/examples/toys/life.als", "models/examples/toys/life.thm", "models/examples/toys/numbering.als", "models/examples/toys/railway.als", "models/examples/toys/railway.thm", "models/examples/toys/trivial.als", "models/examples/tutorial/farmer.als", "models/util/boolean.als", "models/util/graph.als", "models/util/integer.als", "models/util/natural.als", "models/util/ordering.als", "models/util/relation.als", "models/util/seqrel.als", "models/util/sequence.als", "models/util/sequniv.als", "models/util/ternary.als", "models/util/time.als");
        // Record the locations
        System.setProperty("alloy.theme0", alloyHome() + fs + "models");
        System.setProperty("alloy.home", alloyHome());
    }


    public static final class SimpleCallback implements WorkerEngine.WorkerCallback {

        private final Logger logger;
        private final Set<ErrorWarning> warnings = new HashSet<>();
        private boolean repairing = false;
        public void setRepairing(boolean v) {
            repairing = v;
        }

        public SimpleCallback(Logger logger) {
            this.logger = logger;
        }

        @Override
        public void callback(Object msg) {
            if (msg == null) {
                logger.info("Done\n");
                return;
            }
            if (msg instanceof String) {
                logger.info(((String) msg).trim() + "\n");
                return;
            }
            if (msg instanceof Throwable) {
                for (Throwable ex = (Throwable) msg; ex != null; ex = ex.getCause()) {
                    if (ex instanceof OutOfMemoryError) {
                        logger.severe("\nFatal Error: the solver ran out of memory!\n" + "Try simplifying your model or reducing the scope,\n" + "or increase memory under the Options menu.\n");
                        return;
                    }
                    if (ex instanceof StackOverflowError) {
                        logger.severe("\nFatal Error: the solver ran out of stack space!\n" + "Try simplifying your model or reducing the scope,\n" + "or increase stack under the Options menu.\n");
                        return;
                    }
                }
            }
            if (msg instanceof Err) {
                Err ex = (Err) msg;
                String text;
                if (ex instanceof ErrorSyntax)
                    text = "syntax";
                else if (ex instanceof ErrorType)
                    text = "type";
                else
                    text = "fatal";
                StringBuilder sb = new StringBuilder();
                if (ex.pos == Pos.UNKNOWN)
                    sb.append("A ").append(text).append(" error has occurred:  ");
                else {
                    sb.append("A ").append(text).append(" error has occurred at ").append(ex.pos.x).append(" ").append(ex.pos.y).append(" ").append(ex.pos.x2).append(" ").append(ex.pos.y2).append(" ").append(ex.pos.filename);
                }
                sb.append(ex.msg.trim());
                sb.append("(see the stacktrace)");
                sb.append(ex.dump());
                sb.append(")\n");
                logger.severe(sb.toString());
                return;
            }
            if (msg instanceof Throwable) {
                Throwable ex = (Throwable) msg;
                logger.severe(ex.toString().trim() + "\n");
                return;
            }
            if (!(msg instanceof Object[]))
                return;
            Object[] array = (Object[]) msg;
            if (repairing) {
                StringBuilder sb = new StringBuilder();
                if (array[0].equals("RepairTittle")) {
                    sb.append("#########\n");
                    sb.append(array[1]);
                    sb.append("#########\n");
                } else if (array[0].equals("RepairSubTittle")) {
                    sb.append("*********\n");
                    sb.append(array[1]);
                    sb.append("*********\n");
                } else if (array[0].equals("RepairExprOrig->Mut")) {
                    sb.append(array[1]).append("\n");
                    sb.append("|<\t");
                    sb.append(array[2]).append("\n");
                    sb.append("|>\t");
                    sb.append(array[3]).append("\n");
                } else if (array[0].equals("RepairResults")) {
                    sb.append("Evaluation result: ");
                    @SuppressWarnings("unchecked")
                    List<String> l = (List<String>) array[1];
                    int i=1;
                    for (String r:l){
                        switch (r){
                            case "E": sb.append("Invalid candidate");break;
                            case "V":
                            case "R": sb.append("Repair");break;
                            case "F":
                            case "X": sb.append("Not a repair");break;
                            case "PR": sb.append("Partial Repair");break;
                            default:break;
                        }
                        if (i<l.size()) sb.append(",");
                        i++;
                    }
                    sb.append("\n\n");
                }
                if (!sb.toString().trim().isEmpty())
                    logger.info(sb.toString());
                return;
            }
            if (!array[0].equals("warnings")) {
                logger.info(Arrays.toString(array));
                return;
            }
            if (array[0].equals("warnings")) {
                StringBuilder sb = new StringBuilder();
                if (warnings.size() > 1)
                    sb.append("Note: There were ").append(warnings.size()).append(" compilation warnings. Please scroll up to see them.\n\n");
                else
                    sb.append("Note: There was 1 compilation warning. Please scroll up to see them.\n\n");
                if (warnings.size() > 0 && Boolean.FALSE.equals(array[1])) {
                    warnings.forEach(e -> {
                        sb.append("POS: ").append(e.pos.x).append(" ").append(e.pos.y).append(" ").append(e.pos.x2).append(" ").append(e.pos.y2).append(" ").append(e.pos.filename);
                        sb.append("Warnings often indicate errors in the model.\n" + "Some warnings can affect the soundness of the analysis.\n" + "To proceed despite the warnings, go to the Options menu.\n");
                    });
                }
                warnings.clear();
                logger.warning(sb.toString());
                return;
            }
            if (array[0].equals("warning")) {
                ErrorWarning e = (ErrorWarning) (array[1]);
                if (!warnings.add(e))
                    return;
                Pos p = e.pos;
                String sb = "Warning #" + warnings.size() + " at " +
                        p.x + " " +
                        p.y + " " +
                        p.x2 + " " +
                        p.y2 + " " +
                        p.filename + "\n" +
                        e.msg.trim();
                logger.warning(sb);
                return;
            }
            if (array[0].equals("sat")) {
                boolean chk = Boolean.TRUE.equals(array[1]);
                int expects = (Integer) (array[2]);
                String formula = (String) (array[4]);
                StringBuilder sb = new StringBuilder();
                sb.append(chk ? "Counterexample" : "Instance").append(" found. ");
                sb.append(chk ? "Assertion" : "Predicate").append(" ").append(formula);
                sb.append(chk ? " is invalid" : " is consistent");
                if (expects == 0)
                    sb.append(", contrary to expectation");
                else if (expects == 1)
                    sb.append(", as expected");
                sb.append(". ").append(array[5]).append("ms.\n\n");
                logger.info(sb.toString());
                return;
            }
            if (array[0].equals("metamodel")) {
                logger.info("Metamodel successfully generated");
                return;
            }
            if (array[0].equals("minimizing")) {
                boolean chk = Boolean.TRUE.equals(array[1]);
                int expects = (Integer) (array[2]);
                StringBuilder sb = new StringBuilder();
                sb.append(chk ? "   No counterexample found." : "   No instance found.");
                if (chk)
                    sb.append(" Assertion may be valid");
                else
                    sb.append(" Predicate may be inconsistent");
                if (expects == 1)
                    sb.append(", contrary to expectation");
                else if (expects == 0)
                    sb.append(", as expected");
                sb.append(". ").append(array[4]).append("ms.\n");
                sb.append("   Minimizing the unsat core of ").append(array[3]).append(" entries...\n");
                logger.info(sb.toString());
                return;
            }
            if (array[0].equals("unsat")) {
                boolean chk = Boolean.TRUE.equals(array[1]);
                int expects = (Integer) (array[2]);
                String formula = (String) (array[4]);
                StringBuilder sb = new StringBuilder();
                sb.append(chk ? "   No counterexample found. " : "   No instance found.\n");
                sb.append(chk ? "Assertion" : "Predicate").append(formula).append("\n");
                sb.append(chk ? " may be valid" : " may be inconsistent\n");
                if (expects == 1)
                    sb.append(", contrary to expectation");
                else if (expects == 0)
                    sb.append(", as expected");
                if (array.length == 5) {
                    sb.append(". ").append(array[3]).append("ms.\n\n");
                    logger.info(sb.toString());
                    return;
                }
                String core = (String) (array[5]);
                int mbefore = (Integer) (array[6]), mafter = (Integer) (array[7]);
                sb.append(". ").append(array[3]).append("ms.\n");
                if (core.length() == 0) {
                    sb.append("   No unsat core is available in this case. ").append(array[8]).append("ms.\n\n");
                    logger.info(sb.toString());
                    return;
                }
                if (mbefore <= mafter)
                    sb.append(" contains ").append(mafter).append(" top-level formulas. ").append(array[8]).append("ms.\n\n");
                else
                    sb.append(" reduced from ").append(mbefore).append(" to ").append(mafter).append(" top-level formulas. ").append(array[8]).append("ms.\n\n");
                logger.info(sb.toString());
            }
        }


        @Override
        public void done() {
            logger.info("done");
        }

        @Override
        public void fail() {
            logger.severe("fail");
            WorkerEngine.stop();
        }
    }


}

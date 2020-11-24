/* Alloy Analyzer 4 -- Copyright (c) 2006-2009, Felix Chang
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files
 * (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify,
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF
 * OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package edu.mit.csail.sdg.alloy4whole;

import ar.edu.unrc.dc.mutation.MutationConfiguration;
import ar.edu.unrc.dc.mutation.MutationConfiguration.ConfigKey;
import ar.edu.unrc.dc.mutation.Ops;
import ar.edu.unrc.dc.mutation.mutantLab.*;
import ar.edu.unrc.dc.mutation.mutantLab.mutantGeneration.ExpressionsMarker;
import ar.edu.unrc.dc.mutation.mutantLab.testGeneration.BuggyPredsMarker;
import ar.edu.unrc.dc.mutation.mutantLab.testGeneration.TestGenerationRequest;
import ar.edu.unrc.dc.mutation.mutantLab.testGeneration.TestGenerationResult;
import ar.edu.unrc.dc.mutation.mutantLab.testGeneration.TestsGenerator;
import ar.edu.unrc.dc.mutation.util.*;
import ar.edu.unrc.dc.mutation.visitors.ExprToStringNicePrint;
import ar.edu.unrc.dc.mutation.visitors.NodeAliasingFixer;
import ar.edu.unrc.dc.mutation.visitors.ParentRelationshipFixer;
import edu.mit.csail.sdg.alloy4.*;
import edu.mit.csail.sdg.alloy4.WorkerEngine.WorkerCallback;
import edu.mit.csail.sdg.alloy4.WorkerEngine.WorkerTask;
import edu.mit.csail.sdg.alloy4viz.StaticInstanceReader;
import edu.mit.csail.sdg.alloy4viz.VizGUI;
import edu.mit.csail.sdg.ast.*;
import edu.mit.csail.sdg.parser.CompModule;
import edu.mit.csail.sdg.parser.CompUtil;
import edu.mit.csail.sdg.translator.*;
import org.alloytools.alloy.core.AlloyCore;

import java.io.*;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.Map.Entry;
import java.util.logging.FileHandler;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;
import java.util.stream.Collectors;


/** This helper method is used by SimpleGUI. */

final class SimpleReporter extends A4Reporter {

    public static final class SimpleCallback1 implements WorkerCallback {

        private final SimpleGUI         gui;
        private final VizGUI            viz;
        private final SwingLogPanel     span;
        private final Set<ErrorWarning> warnings = new HashSet<ErrorWarning>();
        private final List<String>      results  = new ArrayList<String>();
        private int                     len2     = 0, len3 = 0, verbosity = 0;
        private final String            latestName;
        private final int               latestVersion;
        //@Atryker - MuAlloy
        public boolean reparing =false;


        public SimpleCallback1(SimpleGUI gui, VizGUI viz, SwingLogPanel span, int verbosity, String latestName, int latestVersion) {
            this.gui = gui;
            this.viz = viz;
            this.span = span;
            this.verbosity = verbosity;
            this.latestName = latestName;
            this.latestVersion = latestVersion;
            len2 = len3 = span.getLength();
        }

        @Override
        public void done() {
            if (viz != null)
                span.setLength(len2);
            else
                span.logDivider();
            span.flush();
            gui.doStop(0);
        }

        @Override
        public void fail() {
            span.logBold("\nAn error has occurred!\n");
            span.logDivider();
            span.flush();
            gui.doStop(1);
        }

        @Override
        public void callback(Object msg) {
            if (msg == null) {
                span.logBold("Done\n");
                span.flush();
                return;
            }
            if (msg instanceof String) {
                span.logBold(((String) msg).trim() + "\n");
                span.flush();
                return;
            }
            if (msg instanceof Throwable) {
                for (Throwable ex = (Throwable) msg; ex != null; ex = ex.getCause()) {
                    if (ex instanceof OutOfMemoryError) {
                        span.logBold("\nFatal Error: the solver ran out of memory!\n" + "Try simplifying your model or reducing the scope,\n" + "or increase memory under the Options menu.\n");
                        return;
                    }
                    if (ex instanceof StackOverflowError) {
                        span.logBold("\nFatal Error: the solver ran out of stack space!\n" + "Try simplifying your model or reducing the scope,\n" + "or increase stack under the Options menu.\n");
                        StringWriter sw = new StringWriter();
                        ex.printStackTrace(new PrintWriter(sw));
                        String exceptionAsString = sw.toString();
                        span.logBold(exceptionAsString);
                        return;
                    }
                }
            }
            if (msg instanceof Err) {
                Err ex = (Err) msg;
                String text = "fatal";
                boolean fatal = false;
                if (ex instanceof ErrorSyntax)
                    text = "syntax";
                else if (ex instanceof ErrorType)
                    text = "type";
                else
                    fatal = true;
                if (ex.pos == Pos.UNKNOWN)
                    span.logBold("A " + text + " error has occurred:  ");
                else
                    span.logLink("A " + text + " error has occurred:  ", "POS: " + ex.pos.x + " " + ex.pos.y + " " + ex.pos.x2 + " " + ex.pos.y2 + " " + ex.pos.filename);
                if (verbosity > 2) {
                    span.log("(see the ");
                    span.logLink("stacktrace", "MSG: " + ex.dump());
                    span.log(")\n");
                } else {
                    span.log("\n");
                }
                span.logIndented(ex.msg.trim());
                span.log("\n");
                if (fatal && latestVersion > Version.buildNumber())
                    span.logBold("\nNote: You are running Alloy build#" + Version.buildNumber() + ",\nbut the most recent is Alloy build#" + latestVersion + ":\n( version " + latestName + " )\nPlease try to upgrade to the newest version," + "\nas the problem may have been fixed already.\n");
                span.flush();
                if (!fatal)
                    gui.doVisualize("POS: " + ex.pos.x + " " + ex.pos.y + " " + ex.pos.x2 + " " + ex.pos.y2 + " " + ex.pos.filename);
                return;
            }
            if (msg instanceof Throwable) {
                Throwable ex = (Throwable) msg;
                span.logBold(ex.toString().trim() + "\n");
                span.flush();
                return;
            }
            if (!(msg instanceof Object[]))
                return;

            Object[] array = (Object[]) msg;
            //@Atryker - mualloy - Avoid all callback message from the alloy process and only inform Repair messages
            if (reparing) {
                if (array[0].equals("TestGeneration")) {
                    len3 = len2 = span.getLength();
                    span.logAStrykerGreen("Command:\n");
                    span.logBold(array[1] + "\n");
                    span.logAStrykerGreen("Predicate:\n");
                    span.logBold(array[2]+"\n");
                }
                if (array[0].equals("RepairTittle")) {
                    len3 = len2 = span.getLength();
                    span.logBold("" + array[1]);
                }
                if (array[0].equals("RepairSubTittle")) {
                    len3 = len2 = span.getLength();
                    span.log("" + array[1]);
                }
                if (array[0].equals("RepairError")) {
                    len3 = len2 = span.getLength();
                    span.logAstrykerRed("" + array[1]);
                }
                if (array[0].equals("RepairExprOrig->Mut")) {
                    len3 = len2 = span.getLength();
                    span.log("" + array[1]+":\n");
                    span.log("|<     ");
                    span.logAStrykerGreen(array[2]+"\n");
                    span.log("|>     ");
                    span.logAstrykerBlue(array[3]+"");
                }
                if (array[0].equals("RepairResults")) {
                    len3 = len2 = span.getLength();
                    span.logBold("Results: [" );
                    List<String> l = (List<String>) array[1];
                    int i=1;
                    for (String r:l){
                        switch (r){
                            case "E": span.logAstrykerBlue("E");break;
                            case "V":
                            case "R": span.logAStrykerGreen("Repair");break;
                            case "F":
                            case "X": span.logAstrykerRed("Not a repair");break;
                            case "PR": span.logAstrykerRed("Partial Repair");break;
                            default:break;
                        }
                        if (i<l.size()) span.log(",");
                        i++;
                    }
                    span.logBold("]\n\n" );
                }
                return;
            }
            //-----------
            if (array[0].equals("pop")) {
                span.setLength(len2);
                String x = (String) (array[1]);
                if (viz != null && x.length() > 0)
                    OurDialog.alert(x);
            }
            if (array[0].equals("declare")) {
                gui.doSetLatest((String) (array[1]));
            }
            if (array[0].equals("S2")) {
                len3 = len2 = span.getLength();
                span.logBold("" + array[1]);
            }
            if (array[0].equals("R3")) {
                span.setLength(len3);
                span.log("" + array[1]);
            }

            if (array[0].equals("link")) {
                span.logLink((String) (array[1]), (String) (array[2]));
            }
            if (array[0].equals("bold")) {
                span.logBold("" + array[1]);
            }
            if (array[0].equals("")) {
                span.log("" + array[1]);
            }
            if (array[0].equals("scope") && verbosity > 0) {
                span.log("   " + array[1]);
            }
            if (array[0].equals("bound") && verbosity > 1) {
                span.log("   " + array[1]);
            }
            if (array[0].equals("resultCNF")) {
                results.add(null);
                span.setLength(len3);
                span.log("   File written to " + array[1] + "\n\n");
            }
            if (array[0].equals("debug") && verbosity > 2) {
                span.log("   " + array[1] + "\n");
                len2 = len3 = span.getLength();
            }
            if (array[0].equals("translate")) {
                span.log("   " + array[1]);
                len3 = span.getLength();
                span.logBold("   Generating CNF...\n");
            }
            if (array[0].equals("solve")) {
                span.setLength(len3);
                span.log("   " + array[1]);
                len3 = span.getLength();
                span.logBold("   Solving...\n");
            }
            if (array[0].equals("warnings")) {
                if (warnings.size() == 0)
                    span.setLength(len2);
                else if (warnings.size() > 1)
                    span.logBold("Note: There were " + warnings.size() + " compilation warnings. Please scroll up to see them.\n\n");
                else
                    span.logBold("Note: There was 1 compilation warning. Please scroll up to see them.\n\n");
                if (warnings.size() > 0 && Boolean.FALSE.equals(array[1])) {
                    Pos e = warnings.iterator().next().pos;
                    gui.doVisualize("POS: " + e.x + " " + e.y + " " + e.x2 + " " + e.y2 + " " + e.filename);
                    span.logBold("Warnings often indicate errors in the model.\n" + "Some warnings can affect the soundness of the analysis.\n" + "To proceed despite the warnings, go to the Options menu.\n");
                }
            }
            if (array[0].equals("warning")) {
                ErrorWarning e = (ErrorWarning) (array[1]);
                if (!warnings.add(e))
                    return;
                Pos p = e.pos;
                span.logLink("Warning #" + warnings.size(), "POS: " + p.x + " " + p.y + " " + p.x2 + " " + p.y2 + " " + p.filename);
                span.log("\n");
                span.logIndented(e.msg.trim());
                span.log("\n\n");
            }
            if (array[0].equals("sat")) {
                boolean chk = Boolean.TRUE.equals(array[1]);
                int expects = (Integer) (array[2]);
                String filename = (String) (array[3]), formula = (String) (array[4]);
                results.add(filename);
                (new File(filename)).deleteOnExit();
                gui.doSetLatest(filename);
                span.setLength(len3);
                span.log("   ");
                span.logLink(chk ? "Counterexample" : "Instance", "XML: " + filename);
                span.log(" found. ");
                span.logLink(chk ? "Assertion" : "Predicate", formula);
                span.log(chk ? " is invalid" : " is consistent");
                if (expects == 0)
                    span.log(", contrary to expectation");
                else if (expects == 1)
                    span.log(", as expected");
                span.log(". " + array[5] + "ms.\n\n");
            }
            if (array[0].equals("metamodel")) {
                String outf = (String) (array[1]);
                span.setLength(len2);
                (new File(outf)).deleteOnExit();
                gui.doSetLatest(outf);
                span.logLink("Metamodel", "XML: " + outf);
                span.log(" successfully generated.\n\n");
            }
            if (array[0].equals("minimizing")) {
                boolean chk = Boolean.TRUE.equals(array[1]);
                int expects = (Integer) (array[2]);
                span.setLength(len3);
                span.log(chk ? "   No counterexample found." : "   No instance found.");
                if (chk)
                    span.log(" Assertion may be valid");
                else
                    span.log(" Predicate may be inconsistent");
                if (expects == 1)
                    span.log(", contrary to expectation");
                else if (expects == 0)
                    span.log(", as expected");
                span.log(". " + array[4] + "ms.\n");
                span.logBold("   Minimizing the unsat core of " + array[3] + " entries...\n");
            }
            if (array[0].equals("unsat")) {
                boolean chk = Boolean.TRUE.equals(array[1]);
                int expects = (Integer) (array[2]);
                String formula = (String) (array[4]);
                span.setLength(len3);
                span.log(chk ? "   No counterexample found. " : "   No instance found. ");
                span.logLink(chk ? "Assertion" : "Predicate", formula);
                span.log(chk ? " may be valid" : " may be inconsistent");
                if (expects == 1)
                    span.log(", contrary to expectation");
                else if (expects == 0)
                    span.log(", as expected");
                if (array.length == 5) {
                    span.log(". " + array[3] + "ms.\n\n");
                    span.flush();
                    return;
                }
                String core = (String) (array[5]);
                int mbefore = (Integer) (array[6]), mafter = (Integer) (array[7]);
                span.log(". " + array[3] + "ms.\n");
                if (core.length() == 0) {
                    results.add("");
                    span.log("   No unsat core is available in this case. " + array[8] + "ms.\n\n");
                    span.flush();
                    return;
                }
                results.add(core);
                (new File(core)).deleteOnExit();
                span.log("   ");
                span.logLink("Core", core);
                if (mbefore <= mafter)
                    span.log(" contains " + mafter + " top-level formulas. " + array[8] + "ms.\n\n");
                else
                    span.log(" reduced from " + mbefore + " to " + mafter + " top-level formulas. " + array[8] + "ms.\n\n");
            }
            span.flush();
        }
    }

    private void cb(Serializable... objs) {
        cb.callback(objs);
    }

    /** {@inheritDoc} */
    @Override
    public void resultCNF(final String filename) {
        cb("resultCNF", filename);
    }

    /** {@inheritDoc} */
    @Override
    public void warning(final ErrorWarning ex) {
        warn++;
        cb("warning", ex);
    }

    /** {@inheritDoc} */
    @Override
    public void scope(final String msg) {
        cb("scope", msg);
    }

    /** {@inheritDoc} */
    @Override
    public void bound(final String msg) {
        cb("bound", msg);
    }

    /** {@inheritDoc} */
    @Override
    public void debug(final String msg) {
        cb("debug", msg.trim());
    }

    /** {@inheritDoc} */
    @Override
    public void translate(String solver, int bitwidth, int maxseq, int skolemDepth, int symmetry) {
        lastTime = System.currentTimeMillis();
        cb("translate", "Solver=" + solver + " Bitwidth=" + bitwidth + " MaxSeq=" + maxseq + (skolemDepth == 0 ? "" : " SkolemDepth=" + skolemDepth) + " Symmetry=" + (symmetry > 0 ? ("" + symmetry) : "OFF") + '\n');
    }

    /** {@inheritDoc} */
    @Override
    public void solve(final int primaryVars, final int totalVars, final int clauses) {
        minimized = 0;
        cb("solve", "" + totalVars + " vars. " + primaryVars + " primary vars. " + clauses + " clauses. " + (System.currentTimeMillis() - lastTime) + "ms.\n");
        lastTime = System.currentTimeMillis();
    }

    /** {@inheritDoc} */
    @Override
    public void resultSAT(Object command, long solvingTime, Object solution) {
        if (!(solution instanceof A4Solution) || !(command instanceof Command))
            return;
        A4Solution sol = (A4Solution) solution;
        Command cmd = (Command) command;
        String formula = recordKodkod ? sol.debugExtractKInput() : "";
        String filename = tempfile + ".xml";
        synchronized (SimpleReporter.class) {
            try {
                cb("R3", "   Writing the XML file...");
                if (latestModule != null)
                    writeXML(this, latestModule, filename, sol, latestKodkodSRC);
            } catch (Throwable ex) {
                cb("bold", "\n" + (ex.toString().trim()) + "\nStackTrace:\n" + (MailBug.dump(ex).trim()) + "\n");
                return;
            }
            latestKodkods.clear();
            latestKodkods.add(sol.toString());
            latestKodkod = sol;
            latestKodkodXML = filename;
        }
        String formulafilename = "";
        if (formula.length() > 0 && tempfile != null) {
            formulafilename = tempfile + ".java";
            try {
                Util.writeAll(formulafilename, formula);
                formulafilename = "CNF: " + formulafilename;
            } catch (Throwable ex) {
                formulafilename = "";
            }
        }
        cb("sat", cmd.check, cmd.expects, filename, formulafilename, System.currentTimeMillis() - lastTime);
    }

    /** {@inheritDoc} */
    @Override
    public void minimizing(Object command, int before) {
        if (!(command instanceof Command))
            return;
        Command cmd = (Command) command;
        minimized = System.currentTimeMillis();
        cb("minimizing", cmd.check, cmd.expects, before, minimized - lastTime);
    }

    /** {@inheritDoc} */
    @Override
    public void minimized(Object command, int before, int after) {
        minimizedBefore = before;
        minimizedAfter = after;
    }

    /** {@inheritDoc} */
    @Override
    public void resultUNSAT(Object command, long solvingTime, Object solution) {
        if (!(solution instanceof A4Solution) || !(command instanceof Command))
            return;
        A4Solution sol = (A4Solution) solution;
        Command cmd = (Command) command;
        String originalFormula = recordKodkod ? sol.debugExtractKInput() : "";
        String corefilename = "", formulafilename = "";
        if (originalFormula.length() > 0 && tempfile != null) {
            formulafilename = tempfile + ".java";
            try {
                Util.writeAll(formulafilename, originalFormula);
                formulafilename = "CNF: " + formulafilename;
            } catch (Throwable ex) {
                formulafilename = "";
            }
        }
        Pair<Set<Pos>,Set<Pos>> core = sol.highLevelCore();
        if ((core.a.size() > 0 || core.b.size() > 0) && tempfile != null) {
            corefilename = tempfile + ".core";
            OutputStream fs = null;
            ObjectOutputStream os = null;
            try {
                fs = new FileOutputStream(corefilename);
                os = new ObjectOutputStream(fs);
                os.writeObject(core);
                os.writeObject(sol.lowLevelCore());
                corefilename = "CORE: " + corefilename;
            } catch (Throwable ex) {
                corefilename = "";
            } finally {
                Util.close(os);
                Util.close(fs);
            }
        }
        if (minimized == 0)
            cb("unsat", cmd.check, cmd.expects, (System.currentTimeMillis() - lastTime), formulafilename);
        else
            cb("unsat", cmd.check, cmd.expects, minimized - lastTime, formulafilename, corefilename, minimizedBefore, minimizedAfter, (System.currentTimeMillis() - minimized));
    }

    private final WorkerCallback cb;

    // ========== These fields should be set each time we execute a set of
    // commands

    /** Whether we should record Kodkod input/output. */
    private final boolean recordKodkod;

    /**
     * The time that the last action began; we subtract it from
     * System.currentTimeMillis() to determine the elapsed time.
     */
    private long          lastTime  = 0;

    /**
     * If we performed unsat core minimization, then this is the start of the
     * minimization, else this is 0.
     */
    private long          minimized = 0;

    /** The unsat core size before minimization. */
    private int           minimizedBefore;

    /** The unsat core size after minimization. */
    private int           minimizedAfter;

    /**
     * The filename where we can write a temporary Java file or Core file.
     */
    private String        tempfile  = null;

    // ========== These fields may be altered as each successful command
    // generates a Kodkod or Metamodel instance

    /**
     * The set of Strings already enumerated for this current solution.
     */
    private static final Set<String>       latestKodkods      = new LinkedHashSet<String>();

    /**
     * The A4Solution corresponding to the latest solution generated by Kodkod; this
     * field must be synchronized.
     */
    private static A4Solution              latestKodkod       = null;

    /**
     * The root Module corresponding to this.latestKodkod; this field must be
     * synchronized.
     */
    private static Module                  latestModule       = null;

    /**
     * The source code corresponding to the latest solution generated by Kodkod;
     * this field must be synchronized.
     */
    private static ConstMap<String,String> latestKodkodSRC    = null;

    /**
     * The XML filename corresponding to the latest solution generated by Kodkod;
     * this field must be synchronized.
     */
    private static String                  latestKodkodXML    = null;

    /**
     * The XML filename corresponding to the latest metamodel generated by
     * TranslateAlloyToMetamodel; this field must be synchronized.
     */
    private static String                  latestMetamodelXML = null;

    /** Constructor is private. */
    SimpleReporter(WorkerCallback cb, boolean recordKodkod) {
        this.cb = cb;
        this.recordKodkod = recordKodkod;
    }

    /** Helper method to write out a full XML file. */
    private static void writeXML(A4Reporter rep, Module mod, String filename, A4Solution sol, Map<String,String> sources) throws Exception {
        sol.writeXML(rep, filename, mod.getAllFunc(), sources);
        if (AlloyCore.isDebug())
            validate(filename);
    }

    private int warn = 0;

    /** Task that performs solution enumeration. */
    static final class SimpleTask2 implements WorkerTask {

        private static final long       serialVersionUID = 0;
        public String                   filename         = "";
        public transient WorkerCallback out              = null;

        private void cb(Object... objs) throws Exception {
            out.callback(objs);
        }

        @Override
        public void run(WorkerCallback out) throws Exception {
            this.out = out;
            cb("S2", "Enumerating...\n");
            A4Solution sol;
            Module mod;
            synchronized (SimpleReporter.class) {
                if (latestMetamodelXML != null && latestMetamodelXML.equals(filename)) {
                    cb("pop", "You cannot enumerate a metamodel.\n");
                    return;
                }
                if (latestKodkodXML == null || !latestKodkodXML.equals(filename)) {
                    cb("pop", "You can only enumerate the solutions of the most-recently-solved command.");
                    return;
                }
                if (latestKodkod == null || latestModule == null || latestKodkodSRC == null) {
                    cb("pop", "Error: the SAT solver that generated the instance has exited,\nso we cannot enumerate unless you re-solve that command.\n");
                    return;
                }
                sol = latestKodkod;
                mod = latestModule;
            }
            if (!sol.satisfiable()) {
                cb("pop", "Error: This command is unsatisfiable,\nso there are no solutions to enumerate.");
                return;
            }
            if (!sol.isIncremental()) {
                cb("pop", "Error: This solution was not generated by an incremental SAT solver.\n" + "Currently only MiniSat and SAT4J are supported.");
                return;
            }
            int tries = 0;
            while (true) {
                sol = sol.next();
                if (!sol.satisfiable()) {
                    cb("pop", "There are no more satisfying instances.\n\n" + "Note: due to symmetry breaking and other optimizations,\n" + "some equivalent solutions may have been omitted.");
                    return;
                }
                String toString = sol.toString();
                synchronized (SimpleReporter.class) {
                    if (!latestKodkods.add(toString))
                        if (tries < 100) {
                            tries++;
                            continue;
                        }
                    // The counter is needed to avoid a Kodkod bug where
                    // sometimes we might repeat the same solution infinitely
                    // number of times; this at least allows the user to keep
                    // going
                    writeXML(null, mod, filename, sol, latestKodkodSRC);
                    latestKodkod = sol;
                }
                cb("declare", filename);
                return;
            }
        }
    }

    /**
     * Validate the given filename to see if it is a valid Alloy XML instance file.
     */
    private static void validate(String filename) throws Exception {
        A4SolutionReader.read(new ArrayList<Sig>(), new XMLNode(new File(filename))).toString();
        StaticInstanceReader.parseInstance(new File(filename));
    }

    /** Task that perform one command. */
    static final class SimpleTask1 implements WorkerTask {

        private static final long serialVersionUID = 0;
        public A4Options          options;
        public String             tempdir;
        public boolean            bundleWarningNonFatal;
        public int                bundleIndex;
        public int                resolutionMode;
        public Map<String,String> map;

        public SimpleTask1() {}

        public void cb(WorkerCallback out, Object... objs) throws IOException {
            out.callback(objs);
        }

        @Override
        public void run(WorkerCallback out) throws Exception {
            cb(out, "S2", "Starting the solver...\n\n");
            final SimpleReporter rep = new SimpleReporter(out, options.recordKodkod);
            final Module world = CompUtil.parseEverything_fromFile(rep, map, options.originalFilename, resolutionMode);
            final List<Sig> sigs = world.getAllReachableSigs();
            final ConstList<Command> cmds = world.getAllCommands();
            cb(out, "warnings", bundleWarningNonFatal);
            if (rep.warn > 0 && !bundleWarningNonFatal)
                return;
            List<String> result = new ArrayList<String>(cmds.size());
            if (bundleIndex == -2) {
                final String outf = tempdir + File.separatorChar + "m.xml";
                cb(out, "S2", "Generating the metamodel...\n");
                PrintWriter of = new PrintWriter(outf, "UTF-8");
                Util.encodeXMLs(of, "\n<alloy builddate=\"", Version.buildDate(), "\">\n\n");
                A4SolutionWriter.writeMetamodel(ConstList.make(sigs), options.originalFilename, of);
                Util.encodeXMLs(of, "\n</alloy>");
                Util.close(of);
                if (AlloyCore.isDebug())
                    validate(outf);
                cb(out, "metamodel", outf);
                synchronized (SimpleReporter.class) {
                    latestMetamodelXML = outf;
                }
            } else
                for (int i = 0; i < cmds.size(); i++)
                    if (bundleIndex < 0 || i == bundleIndex) {
                        synchronized (SimpleReporter.class) {
                            latestModule = world;
                            latestKodkodSRC = ConstMap.make(map);
                        }
                        final String tempXML = tempdir + File.separatorChar + i + ".cnf.xml";
                        final String tempCNF = tempdir + File.separatorChar + i + ".cnf";
                        final Command cmd = cmds.get(i);
                        rep.tempfile = tempCNF;
                        cb(out, "bold", "Executing \"" + cmd + "\"\n");
                        A4Solution ai = TranslateAlloyToKodkod.execute_commandFromBook(rep, world.getAllReachableSigs(), cmd, options);
                        if (ai == null)
                            result.add(null);
                        else if (ai.satisfiable())
                            result.add(tempXML);
                        else if (ai.highLevelCore().a.size() > 0)
                            result.add(tempCNF + ".core");
                        else
                            result.add("");
                    }
            (new File(tempdir)).delete(); // In case it was UNSAT, or
                                         // canceled...
            if (result.size() > 1) {
                rep.cb("bold", "" + result.size() + " commands were executed. The results are:\n");
                for (int i = 0; i < result.size(); i++) {
                    Command r = world.getAllCommands().get(i);
                    if (result.get(i) == null) {
                        rep.cb("", "   #" + (i + 1) + ": Unknown.\n");
                        continue;
                    }
                    if (result.get(i).endsWith(".xml")) {
                        rep.cb("", "   #" + (i + 1) + ": ");
                        rep.cb("link", r.check ? "Counterexample found. " : "Instance found. ", "XML: " + result.get(i));
                        rep.cb("", r.label + (r.check ? " is invalid" : " is consistent"));
                        if (r.expects == 0)
                            rep.cb("", ", contrary to expectation");
                        else if (r.expects == 1)
                            rep.cb("", ", as expected");
                    } else if (result.get(i).endsWith(".core")) {
                        rep.cb("", "   #" + (i + 1) + ": ");
                        rep.cb("link", r.check ? "No counterexample found. " : "No instance found. ", "CORE: " + result.get(i));
                        rep.cb("", r.label + (r.check ? " may be valid" : " may be inconsistent"));
                        if (r.expects == 1)
                            rep.cb("", ", contrary to expectation");
                        else if (r.expects == 0)
                            rep.cb("", ", as expected");
                    } else {
                        if (r.check)
                            rep.cb("", "   #" + (i + 1) + ": No counterexample found. " + r.label + " may be valid");
                        else
                            rep.cb("", "   #" + (i + 1) + ": No instance found. " + r.label + " may be inconsistent");
                        if (r.expects == 1)
                            rep.cb("", ", contrary to expectation");
                        else if (r.expects == 0)
                            rep.cb("", ", as expected");
                    }
                    rep.cb("", ".\n");
                }
                rep.cb("", "\n");
            }
            if (rep.warn > 1)
                rep.cb("bold", "Note: There were " + rep.warn + " compilation warnings. Please scroll up to see them.\n");
            if (rep.warn == 1)
                rep.cb("bold", "Note: There was 1 compilation warning. Please scroll up to see it.\n");
        }
    }

    /** Task that perform a repair by muting mutants expressions (marked with #m#) until all commands returns their expected value. */
    static final class SimpleTaskRepair1 implements WorkerTask {

        private static final Logger logger = Logger.getLogger(SimpleTaskRepair1.class.getName());

        static {
            try {
                // This block configure the logger with handler and formatter
                FileHandler fh = new FileHandler("SimpleTaskRepair1.log");
                logger.addHandler(fh);
                SimpleFormatter formatter = new SimpleFormatter();
                fh.setFormatter(formatter);
            } catch (SecurityException | IOException e) {
                e.printStackTrace();
            }
        }

        public enum ASTRYKER_MODE {REPAIR, TESTGENERATION, CHECK, MUTANTGENERATION}

        private static final long serialVersionUID = 0;
        public A4Options          options;
        public String             tempdir;
        public int                resolutionMode;
        public Map<String,String> map;
        public ASTRYKER_MODE mode = ASTRYKER_MODE.REPAIR;

        public SimpleTaskRepair1() {}

        public void cb(WorkerCallback out, Object... objs) throws IOException {
            out.callback(objs);
        }

        private void setupMutationConfiguration(WorkerCallback out, boolean repair) throws IOException {
            AStrykerConfigReader aStrykerConfig = AStrykerConfigReader.getInstance();
            aStrykerConfig.loadConfig();
            MutationConfiguration.getInstance().loadConfigFromAStrykerConfig();
            if (repair) {
                MutationConfiguration.getInstance().setConfig(ConfigKey.TEST_GENERATION_INSTANCES_TESTS_GENERATION, Boolean.FALSE);
                MutationConfiguration.getInstance().setConfig(ConfigKey.TEST_GENERATION_INSTANCES_TESTS_GENERATION_BUGGY_FUNCS_FILE, "");
                MutationConfiguration.getInstance().setConfig(ConfigKey.MUTATION_STRICT_TYPE_CHECKING, Boolean.FALSE);              //these lines should be later removed
                MutationConfiguration.getInstance().setConfig(ConfigKey.MUTATION_TOSTRING_FULL, Boolean.FALSE);                     //+
                MutationConfiguration.getInstance().setConfig(ConfigKey.MUTATION_BOUND_MUTATION_BY_ANY_OPERATOR, Boolean.TRUE);     //+
                MutationConfiguration.getInstance().setConfig(ConfigKey.REPAIR_GENERATOR_CANDIDATE_GETTER_TIMEOUT, 0L);       //++++++++++++++++++++++++++++++++++
                MutationConfiguration.getInstance().setConfig(ConfigKey.REPAIR_DEBUG_SKIP_VERIFICATION, Boolean.FALSE);             //ONLY FOR DEBUGGING MUTATION GENERATION
                MutationConfiguration.getInstance().setConfig(ConfigKey.REPAIR_VARIABILIZATION, aStrykerConfig.getBooleanArgument(AStrykerConfigReader.Config_key.VARIABILIZATION)); //update the variabilization Repair option
                MutationConfiguration.getInstance().setConfig(ConfigKey.REPAIR_VARIABILIZATION_TEST_GENERATION, aStrykerConfig.getBooleanArgument(AStrykerConfigReader.Config_key.VARIABILIZATION_TEST_GENERATION));
                MutationConfiguration.getInstance().setConfig(ConfigKey.REPAIR_VARIABILIZATION_USE_SAME_TYPES, aStrykerConfig.getBooleanArgument(AStrykerConfigReader.Config_key.VARIABILIZATION_SAME_TYPE));
                MutationConfiguration.getInstance().setConfig(ConfigKey.REPAIR_PARTIAL_REPAIR, aStrykerConfig.getBooleanArgument(AStrykerConfigReader.Config_key.PARTIAL_REPAIR));
                MutationConfiguration.getInstance().setConfig(ConfigKey.REPAIR_PARTIAL_REPAIR_FULL_CALLGRAPH_VALIDATION, aStrykerConfig.getBooleanArgument(AStrykerConfigReader.Config_key.PARTIAL_REPAIR_FULLCGRAPH_VALIDATION));
                MutationConfiguration.getInstance().setConfig(ConfigKey.REPAIR_PARTIAL_REPAIR_REQUIRE_TESTS_FOR_ALL, aStrykerConfig.getBooleanArgument(AStrykerConfigReader.Config_key.PARTIAL_REPAIR_INDEPENDENT_TESTS_FOR_ALL));
                MutationConfiguration.getInstance().setConfig(ConfigKey.REPAIR_PARTIAL_REPAIR_PRUNING, aStrykerConfig.getBooleanArgument(AStrykerConfigReader.Config_key.PARTIAL_REPAIR_PRUNING));
                MutationConfiguration.getInstance().setConfig(ConfigKey.REPAIR_MAX_DEPTH, aStrykerConfig.getIntArgument(AStrykerConfigReader.Config_key.MAX_DEPTH));
                MutationConfiguration.getInstance().setConfig(ConfigKey.REPAIR_TESTS_ONLY, aStrykerConfig.getBooleanArgument(AStrykerConfigReader.Config_key.USE_PO_TO_VALIDATE));
                boolean partialRepair = aStrykerConfig.getBooleanArgument(AStrykerConfigReader.Config_key.PARTIAL_REPAIR);
                boolean partialPruning = aStrykerConfig.getBooleanArgument(AStrykerConfigReader.Config_key.PARTIAL_REPAIR_PRUNING);
                if (partialRepair || partialPruning) {
                    MutationConfiguration.getInstance().setConfig(ConfigKey.REPAIR_DETAILED_TESTS_RESULTS, Boolean.TRUE);
                } else {
                    MutationConfiguration.getInstance().setConfig(ConfigKey.REPAIR_DETAILED_TESTS_RESULTS, Boolean.FALSE);
                }
                int timeoutInMinutes = aStrykerConfig.getIntArgument(AStrykerConfigReader.Config_key.TIMEOUT);
                long timeout = (timeoutInMinutes * 60) * 1000;
                MutationConfiguration.getInstance().setConfig(ConfigKey.REPAIR_TIMEOUT, timeout);
                MutationConfiguration.getInstance().setConfig(ConfigKey.TEST_GENERATION_AREPAIR_INTEGRATION, Boolean.FALSE);
                MutationConfiguration.getInstance().setConfig(ConfigKey.TEST_GENERATION_USE_MODEL_OVERRIDING, Boolean.FALSE);
            }
            MutationConfiguration.getInstance().setConfig(ConfigKey.TEST_GENERATION_TESTS_PER_STEP, aStrykerConfig.getIntArgument(AStrykerConfigReader.Config_key.TEST_GENERATION_TESTS_PER_STEP));
            MutationConfiguration.getInstance().setConfig(ConfigKey.TEST_GENERATION_MAX_TESTS_PER_COMMAND, aStrykerConfig.getIntArgument(AStrykerConfigReader.Config_key.TEST_GENERATION_MAX_TESTS_PER_COMMAND));
            if (repair) {
                logger.info(MutationConfiguration.getInstance().toString());
                cb(out, "RepairTittle", MutationConfiguration.getInstance().toString());
            } else {
                String testGenerationLogMessage = "Tests to generate per command: " + TestsGenerator.testsPerGeneration() + "\n";
                logger.info(testGenerationLogMessage);
                cb(out, "RepairTittle", testGenerationLogMessage);
            }
        }

        @Override
        public void run(WorkerCallback out) throws Exception {
            try {
                switch (mode) {
                    case REPAIR: {
                        runRepair(out);
                        break;
                    }
                    case TESTGENERATION: {
                        runTestGeneration(out);
                        break;
                    }
                    case CHECK: {
                        runCheck(out);
                        break;
                    }
                    case MUTANTGENERATION: {
                        runMutantGeneration(out);
                        break;
                    }
                }
            } catch (Exception e) {
                logger.info("Exception in run:\n" + Arrays.toString(e.getStackTrace()).replace( ',', '\n' ));
                throw e;
            } catch (Throwable e) {
                logger.info("FATAL Exception in run:\n" + Arrays.toString(e.getStackTrace()).replace( ',', '\n' ));
                throw e;
            }
        }

        private void runMutantGeneration(WorkerCallback out) throws Exception {
            cb(out, "RepairTittle", "Mutants generation...\n\n");
            logger.info("Starting mutants generation on model: " + options.originalFilename);
            setupMutationConfiguration(out, true);
            final SimpleReporter rep = new SimpleReporter(out, options.recordKodkod);
            final CompModule world = CompUtil.parseEverything_fromFile(rep, map, options.originalFilename, resolutionMode);
            ASTMutator.startInstance(world);
            //==========================================
            fixParentRelationship(world);
            NodeAliasingFixer nodeAliasingFixer = new NodeAliasingFixer();
            nodeAliasingFixer.fixSigNodes(world);
            if (world.markedEprsToMutate.isEmpty()) {
                ExpressionsMarker.markAllExpressions(world);
            }
            DependencyScanner.scanDependencies(world);
            // Generate and build the mutation manager
            cb(out, "RepairSubTittle", world.markedEprsToMutate.size()+  " mutations mark detected Executing \n");
            ContextExpressionExtractor.reInitialize(world);
            Pruning.initializeInstance(null, options);
            Ops[] availableOps = Ops.values();
            logger.info("***Mutation operators***\n" +
                    Arrays.stream(availableOps).filter(Ops::isImplemented).map(Enum::toString).collect(Collectors.joining(",")));
            RepairTimeOut.initialize(repairTimeout());
            MutantLab.initialize(world, maxDepthForRepair(), availableOps);
            MutantLab mutantLab = MutantLab.getInstance();
            RepairReport.getInstance().setCommands(DependencyGraph.getInstance().getAllCommands().size());
            RepairReport.getInstance().setVariabilizationRelatedCommands((int) DependencyGraph.getInstance().getAllCommands().stream().filter(Command::isVariabilizationTest).count());
            RepairReport.getInstance().setMarkedExpressions(MutantLab.getInstance().getMarkedExpressions());
            //disable variabilization and test generation
            MutationConfiguration.getInstance().setConfig(ConfigKey.REPAIR_VARIABILIZATION, Boolean.FALSE);
            MutationConfiguration.getInstance().setConfig(ConfigKey.REPAIR_VARIABILIZATION_TEST_GENERATION, Boolean.FALSE);
            //create mutants folder
            File outFolder = new File((String) MutationConfiguration.getInstance().getConfigValue(ConfigKey.MUTANT_GENERATION_OUTPUT_FOLDER).orElse(ConfigKey.MUTANT_GENERATION_OUTPUT_FOLDER.defaultValue()));
            if (outFolder.toString().isEmpty()) {
                Path modelFolder = Paths.get(options.originalFilename).getParent();
                if (modelFolder != null)
                    outFolder = modelFolder.toFile();
                else
                    outFolder = Paths.get(System.getProperty("user.dir")).toFile();
            }
            if (!outFolder.exists()) {
                logger.info("Mutants output folder doesn't exists ( " + outFolder.toString() + ") .. creating");
                if (!outFolder.isDirectory())
                    throw new IllegalStateException("Mutants output folder is not a folder ( " + outFolder.toString() + ")");
                if (!outFolder.canExecute() || !outFolder.canWrite())
                    throw new IllegalStateException("Insufficient access to output folder ( " + outFolder.toString() + ")");
                if (!outFolder.mkdirs()) {
                    throw new IllegalStateException("Couldn't create mutants output folder ( " + outFolder.toString() + ")");
                }
            }
            //======================== mutants test cycle ===========
            int count = 1;
            int mutationsLimit = (int) MutationConfiguration.getInstance().getConfigValueOrDefault(ConfigKey.MUTANT_GENERATION_LIMIT);
            int initialCommands = world.getAllCommands().size();
            cb(out, "RepairSubTittle", "Generating... ");
            RepairReport.getInstance().setAsMutantGenerationRun();
            RepairReport.getInstance().clockStart();
            RepairTimeOut.getInstance().start();
            FileUtils.setLogger(logger);
            Candidate current;
            while ((current = mutantLab.advance()) != null) {
                cb(out, "RepairSubTittle", "Validating mutant " + count + " for " + initialCommands + " commands...\n");
                count++;
                //report current mutation
                if (current.isFirst() || current.mutations() == 0) {
                    logger.info("Skipping original");
                } else {
                    if (current == Candidate.TIMEOUT) {
                        logger.info("Timeout reached");
                        cb(out, "RepairSubTittle", "Timeout reached");
                        break;
                    }
                    RepairReport.getInstance().incExaminedCandidates();
                    for (Triplet<String, String, String> em : current.getCurrentMutationsInfo()) {
                        cb(out, "RepairExprOrig->Mut", em.a, em.b, em.c + "  \n");
                    }
                    logger.info("Validating mutant " + count);
                    logger.info(current.toString());
                    // check all commands

                    boolean isValid = true;
                    if ((Boolean) MutationConfiguration.getInstance().getConfigValueOrDefault(ConfigKey.MUTANT_GENERATION_CHECK)) {
                        Browsable.freezeParents();
                        current.clearMutatedStatus();
                        try {
                            EvaluationResults results = evaluateCandidateNormalEvaluation(current, rep);
                            isValid = results != null && !results.isDiscarded();
                        } catch (Throwable e) {
                            isValid = false;
                            StringWriter sw = new StringWriter();
                            e.printStackTrace(new PrintWriter(sw));
                            String exceptionAsString = sw.toString();
                            String errorType = (e instanceof Exception) ? "Evaluation Error" : "Fatal Error";
                            cb(out, "RepairError", errorType + "\n" + exceptionAsString);
                            logger.info(errorType + "\n" + exceptionAsString);
                        }
                        Browsable.unfreezeParents();
                    }
                    current.clearMutatedStatus();
                    if (isValid) {
                        String mutantPostFix = "_mutant_" + count;
                        Path modelFileAsPath = Paths.get(options.originalFilename);
                        String mutantModelFile = modelFileAsPath.getFileName().toString().replace(".als", mutantPostFix + ".als");
                        File mutantFile = Paths.get(outFolder.getPath(), mutantModelFile).toFile();
                        File mutant = FileUtils.writeCandidateToFile(current, mutantFile.toString(), false, true);
                        if (mutant != null) {
                            logger.info("Mutant written to " + mutant.toString());
                            cb(out, "RepairSubTittle", "Mutant written to file : " + mutant.toString());
                        } else {
                            logger.info("Failed to write mutants\n" + current.toString());
                            cb(out, "RepairError", "Failed to write mutants\n" + current.toString());
                        }
                    } else {
                        current.markAsInvalid();
                        cb(out, "RepairError", "Invalid mutant\n" + current.toString());
                        logger.info("Invalid mutant\n" + current.toString());
                    }
                }
                if (mutationsLimit > 0 && current.mutations() < mutationsLimit)
                    MutantLab.getInstance().sendCandidateToInput(current, false);
            }
            RepairReport.getInstance().clockEnd();
            cb(out, "RepairSubTittle", "***REPORT***\n" + RepairReport.getInstance().toString() + "\n*********\n");
            logger.info(RepairReport.getInstance().toString());
            ASTMutator.destroyInstance();
            DependencyGraph.destroyInstance();
            Pruning.destroyInstance();
            MutantLab.destroyInstance();
            RepairReport.destroyInstance();
            (new File(tempdir)).delete(); // In case it was UNSAT, or
            // canceled...
        }

        private void runCheck(WorkerCallback out) throws Exception {
            cb(out, "RepairTittle", "Model verification started...\n\n");
            logger.info("Starting verification for model: " + options.originalFilename);
            final SimpleReporter rep = new SimpleReporter(out, options.recordKodkod);
            final CompModule world = CompUtil.parseEverything_fromFile(rep, map, options.originalFilename, resolutionMode);
            ASTMutator.startInstance(world);
            //==========================================
            fixParentRelationship(world);
            NodeAliasingFixer nodeAliasingFixer = new NodeAliasingFixer();
            nodeAliasingFixer.fixSigNodes(world);
            DependencyScanner.scanDependencies(world);
            ContextExpressionExtractor.reInitialize(world);
            Pruning.initializeInstance(null, options);
            MutantLab.initialize(world, maxDepthForRepair());
            //verify
            Candidate original = Candidate.original(world);
            EvaluationResults result = evaluateCandidateNormalEvaluation(original, rep);
            if (result.isRepaired()) {
                FileUtils.writeCheckReportToFile(options.originalFilename, "VALID");
            } else if (result.isDiscarded()) {
                StringWriter sw = new StringWriter();
                result.getException().printStackTrace(new PrintWriter(sw));
                String exceptionAsString = sw.toString();
                FileUtils.writeCheckReportToFile(options.originalFilename, "EXCEPTION\n"+exceptionAsString);
            } else {
                FileUtils.writeCheckReportToFile(options.originalFilename, "INVALID");
            }
            //--------------------------
            ASTMutator.destroyInstance();
            MutantLab.destroyInstance();
            Pruning.destroyInstance();
        }

        private void runTestGeneration(WorkerEngine.WorkerCallback out) throws Exception {
            cb(out, "RepairTittle", "Test generation started...\n\n");
            logger.info("Starting test generation for model: " + options.originalFilename);
            setupMutationConfiguration(out, false);
            File[] outputFiles = writeTestsToFile()?FileUtils.setUpTestGenerationFiles(options.originalFilename, TestsGenerator.generateInstanceTests()):null;
            final SimpleReporter rep = new SimpleReporter(out, options.recordKodkod);
            final CompModule world = CompUtil.parseEverything_fromFile(rep, map, options.originalFilename, resolutionMode);
            if (world.markedEprsToMutate.isEmpty()) {
                String buggyFuncsFileRaw = TestsGenerator.buggyFuncsFile();
                if (!buggyFuncsFileRaw.trim().isEmpty()) {
                    File buggyFuncsFile = new File(buggyFuncsFileRaw);
                    BuggyPredsMarker.markBuggyFunctions(world, buggyFuncsFile);
                }
            }
            ASTMutator.startInstance(world);
            //==========================================
            fixParentRelationship(world);
            NodeAliasingFixer nodeAliasingFixer = new NodeAliasingFixer();
            nodeAliasingFixer.fixSigNodes(world);
            DependencyScanner.scanDependencies(world);
            ContextExpressionExtractor.reInitialize(world);
            Pruning.initializeInstance(null, options);
            MutantLab.initialize(world, maxDepthForRepair());
            TestGenerationResult lastCETestGenerationRes = generateVariabilizationTests(world, rep, true);
            TestGenerationResult lastInstanceTestsGenerationRes = TestGenerationResult.UNDEFINED;
            if (TestsGenerator.generateInstanceTests()) {
                lastInstanceTestsGenerationRes = generateInstanceTests(world, rep);
            }
            for (Command c : world.getAllCommands()) {
                if (c.isGenerated()) {
                    String command = c.toString();
                    Optional<Func> testFunc = DependencyScanner.getFuncByName(((ExprHasName)c.nameExpr).label, world.getAllFunc());
                    if (!testFunc.isPresent())
                        throw new Error("Something went wrong, test command " + command + " has no associated predicate");
                    ExprToStringNicePrint toString = new ExprToStringNicePrint(null);
                    toString.visitPredicate(testFunc.get());
                    String predicate = toString.getStringRepresentation();
                    cb(out, "TestGeneration", command, predicate);
                    String msg = "";
                    msg += "Origin: " + (c.isInstanceTest()?"from instance":"from counterexample") + "\n";
                    if (c.isInstanceTest()) {
                        msg += "From " + (c.fromTrusted()?"trusted":"untrusted") + " functions/predicates" + "\n";
                        msg += "Generated as " + (c.isPositiveInstanceTest()?"positive":"negative") + " test" + "\n";
                    }
                    logger.info("Test Generation, new test generated\n" +
                                    "Command: " + command + "\n" +
                                    msg +
                                    "Test predicate:\n" +
                                    predicate + "\n"
                            );
                }
            }
            if (TestsGenerator.getInstance().getTestAmountPerProperty() == null || TestsGenerator.getInstance().getTestAmountPerProperty().isEmpty()) {
                cb(out, "RepairSubTittle", "No tests generated");
                logger.info("No tests generated");
            } else {
                StringBuilder sb = new StringBuilder("Generated tests per command\n");
                for (Entry<String, Integer> propertyTestAmount : TestsGenerator.getInstance().getTestAmountPerProperty().entrySet()) {
                    sb.append(propertyTestAmount.getKey()).append(" : ").append(propertyTestAmount.getValue()).append("\n");
                }
                cb(out, "RepairSubTittle", sb.toString());
                logger.info(sb.toString());
            }
            if (writeTestsToFile() && outputFiles != null) {
                FileUtils.writeTests(outputFiles[0], world, FileUtils.TestType.CE);
                if (TestsGenerator.generateInstanceTests() && lastInstanceTestsGenerationRes.equals(TestGenerationResult.GENERATED)) {
                    FileUtils.writeTests(outputFiles[2], world, FileUtils.TestType.INS_POS_TRUSTED);
                    FileUtils.writeTests(outputFiles[3], world, FileUtils.TestType.INS_POS_UNTRUSTED);
                    FileUtils.writeTests(outputFiles[4], world, FileUtils.TestType.INS_NEG);
                }
            }
            if (writeTestsToFile() && outputFiles != null)
                FileUtils.writeReport(outputFiles[1], lastCETestGenerationRes);
            ASTMutator.destroyInstance();
            MutantLab.destroyInstance();
            Pruning.destroyInstance();
        }



        public void runRepair(WorkerCallback out) throws Exception {
            cb(out, "RepairTittle", "Reparing process...\n\n");
            logger.info("Starting repair on model: " + options.originalFilename);
            setupMutationConfiguration(out, true);
            final SimpleReporter rep = new SimpleReporter(out, options.recordKodkod);
            final CompModule world = CompUtil.parseEverything_fromFile(rep, map, options.originalFilename, resolutionMode);
            ASTMutator.startInstance(world);
            //==========================================
            fixParentRelationship(world);
            NodeAliasingFixer nodeAliasingFixer = new NodeAliasingFixer();
            nodeAliasingFixer.fixSigNodes(world);
            DependencyScanner.scanDependencies(world);
            // Generate and build the mutation manager
            cb(out, "RepairSubTittle", world.markedEprsToMutate.size()+  " mutations mark detected Executing \n");

            ContextExpressionExtractor.reInitialize(world);
            Pruning.initializeInstance(null, options);
            Ops[] availableOps = Ops.values();
            logger.info("***Mutation operators***\n" +
                    Arrays.stream(availableOps).filter(Ops::isImplemented).map(Enum::toString).collect(Collectors.joining(",")));
            RepairTimeOut.initialize(repairTimeout());
            MutantLab.initialize(world, maxDepthForRepair(), availableOps);
            MutantLab mutantLab = MutantLab.getInstance();
            if (detailedTestResults()) {
                logger.info("Detailed tests results is ENABLED " + (MutantLab.getInstance().isPartialRepairSupported()?"AND SUPPORTED":"BUT NOT SUPPORTED"));
                if (MutantLab.getInstance().isPartialRepairSupported()) {
                    MutationConfiguration.getInstance().setConfig(ConfigKey.REPAIR_GENERATOR_TRIGGER_THRESHOLD, 0);
                    logger.info("Changing " + ConfigKey.REPAIR_GENERATOR_TRIGGER_THRESHOLD.toString() + " to 0");
                    if (!MutantLab.partialRepairAllFPANeedTests()) {
                        StringBuilder sb = new StringBuilder("Using detailed tests results for some bugged predicates/functions/assertions");
                        sb.append("\n");
                        for (Entry<Browsable, List<Command>> indCommandsPerFPA : MutantLab.getInstance().originalCommandsPerFPA.entrySet()) {
                            sb.append(indCommandsPerFPA.getKey().toString());
                            if (indCommandsPerFPA.getValue().isEmpty()) {
                                sb.append(" : has no independent tests");
                            } else {
                                sb.append(" : ").append(indCommandsPerFPA.getValue().stream().map(Command::toString).collect(Collectors.joining(", ")));
                            }
                            sb.append("\n");
                        }
                        logger.info(sb.toString());
                        cb(out, "RepairSubTittle", sb.toString());
                    }
                }
            }
            if (Pruning.getInstance().useVariabilization() && !MutantLab.getInstance().isVariabilizationSupported()) {
                logger.info("Variabilization is ENABLED BUT NOT SUPPORTED (disabling variabilization and test generation)");
                MutationConfiguration.getInstance().setConfig(ConfigKey.REPAIR_VARIABILIZATION, Boolean.FALSE);
                MutationConfiguration.getInstance().setConfig(ConfigKey.REPAIR_VARIABILIZATION_TEST_GENERATION, Boolean.FALSE);
            }
            RepairReport.getInstance().setCommands(DependencyGraph.getInstance().getAllCommands().size());
            RepairReport.getInstance().setVariabilizationRelatedCommands((int) DependencyGraph.getInstance().getAllCommands().stream().filter(Command::isVariabilizationTest).count());
            RepairReport.getInstance().setMarkedExpressions(MutantLab.getInstance().getMarkedExpressions());
            //======================== mutants test cycle ===========
            int count = 0;
            boolean repairFound = false;
            Candidate repair = null;
            int initialCommands = world.getAllCommands().size();
            if (variabilizationTestGeneration()) {
                generateVariabilizationTests(world, rep, false);
            }
            cb(out, "RepairSubTittle", "Repairing... ");
            RepairReport.getInstance().clockStart();
            RepairTimeOut.getInstance().start();
            boolean timeoutReached = false;
            Candidate current;
            while ((current = mutantLab.advance()) != null) {
                count++;
                cb(out, "RepairSubTittle", "Validating mutant " + count + " for " + initialCommands + " commands...\n");
                //report current mutation
                if (current == Candidate.TIMEOUT) {
                    logger.info("Time out reached");
                    timeoutReached = true;
                    break;
                }
                RepairReport.getInstance().incExaminedCandidates();
                if (current.getCurrentMarkedExpression() > 0)
                    RepairReport.getInstance().updateMaxReachedDepth(current.getCurrentMarkedExpression(), current.mutationsForCurrentIndex());
                for (Triplet<String, String, String> em : current.getCurrentMutationsInfo()) {
                    cb(out, "RepairExprOrig->Mut", em.a, em.b, em.c + "  \n");
                }
                logger.info("Validating mutant " + count);
                logger.info(current.toString());
                // check all commands
                Browsable.freezeParents();
                EvaluationResults results = null;
                current.clearMutatedStatus();
                try {
                    if (Pruning.getInstance().partialRepair() || Pruning.getInstance().partialPruning()) {
                        results = evaluateCandidatePartialRepairEvaluation(current, rep);
                    } else {
                        results = evaluateCandidateNormalEvaluation(current, rep);
                    }
                } catch (Throwable e) {
                    StringWriter sw = new StringWriter();
                    e.printStackTrace(new PrintWriter(sw));
                    String exceptionAsString = sw.toString();
                    String errorType = (e instanceof Exception)?"Evaluation Error":"Fatal Error";
                    cb(out, "RepairError", errorType + "\n"+exceptionAsString);
                    logger.info(errorType + "\n" + exceptionAsString);
                }
                current.clearMutatedStatus();
                if (results != null) {
                    if (Pruning.getInstance().partialRepair() || Pruning.getInstance().partialPruning())
                        current.setCommandsResults(results.getCommandResults());
                    if (results.isDiscarded()) {
                        cb(out, "RepairResults", Collections.singletonList("E"));
                        logger.info("ERROR, mutant discarded\ncurrent mutant is:\n" + current.toString());
                        StringWriter sw = new StringWriter();
                        results.getException().printStackTrace(new PrintWriter(sw));
                        String exceptionAsString = sw.toString();
                        logger.info(exceptionAsString);
                        RepairReport.getInstance().incInvalidCandidates();
                        current.markAsInvalid();
                        logger.info("Current combination " + current.toString() + " reported as invalid by repair task");
                    } else if (results.isRepaired()) {
                        repairFound = true;
                        repair = current;
                        cb(out, "RepairResults", Collections.singletonList("R"));
                        RepairReport.getInstance().setRepair(current);
                        logger.info("Current mutant repair the model, all commands results as expected");
                        logger.info("MODEL REPAIRED!");
                        break;
                    } else if (results.isPartialRepair()) {
                        cb(out, "RepairResults", Collections.singletonList("PR"));
                        logger.info("Current mutant is a partial repair");
                    } else {
                        rep.cb("bold", "Current mutant does not repair the model, some commands do not results as expected \n");
                        logger.info("Current mutant does not repair the model, some commands do not results as expected");
                        cb(out, "RepairResults", Collections.singletonList("F"));
                    }
                }
                Browsable.unfreezeParents();
                if (results == null)
                    break;
                MutantLab.getInstance().sendCandidateToInput(current, Pruning.getInstance().partialRepair());
            }
            if (!repairFound) {
                cb(out, "RepairTittle", "Model couldn't be repaired\n");
                if (timeoutReached)
                    cb(out, "RepairSubTittle", "Timeout reached\n");
            } else {
                cb(out, "RepairTittle", "Model repaired\n");
                if (MutantLab.useTestsOnly()) {
                    List<Command> perfectOracleTests = MutantLab.getInstance().getPerfectOracleCommands();
                    if (perfectOracleTests.isEmpty()) {
                        logger.info("No perfect oracle tests to validate repair");
                        cb(out, "RepairSubTittle", "No perfect oracle tests to validate repair\n");
                    } else {
                        repair.clearMutatedStatus();
                        EvaluationResults spuriousCheck = evaluateCandidate(repair, perfectOracleTests, rep, false);
                        if (spuriousCheck.isRepaired()) {
                            logger.info("Perfect oracle tests passed!");
                            cb(out, "RepairTittle", "Perfect oracle test passed!\n");
                        } else {
                            logger.info("Perfect oracle tests not passed, repair is spurious!");
                            cb(out, "RepairTittle", "Perfect oracle test not passed, repair is spurious!\n");
                        }
                    }
                }
            }
            RepairReport.getInstance().clockEnd();
            cb(out, "RepairSubTittle", "***REPORT***\n" + RepairReport.getInstance().toString() + "\n*********\n");
            cb(out, "RepairSubTittle", "Repair time: " + RepairReport.getInstance().getTime() + "ms\n");
            if (repairFound) {
                File repairFile = FileUtils.writeCandidateToFile(repair, options.originalFilename, true, true);
                if (repairFile != null) {
                    logger.info("Repair written to " + repairFile.toString());
                    cb(out, "RepairSubTittle", "Repair written to file : " + repairFile.toString());
                }
            }
            logger.info(RepairReport.getInstance().toString());
            ASTMutator.destroyInstance();
            DependencyGraph.destroyInstance();
            Pruning.destroyInstance();
            MutantLab.destroyInstance();
            RepairReport.destroyInstance();
            (new File(tempdir)).delete(); // In case it was UNSAT, or
            // canceled...
        }

        //TestGenerationResult lastTestGenerationRes = TestGenerationResult.UNDEFINED;
        private TestGenerationResult generateVariabilizationTests(CompModule world, SimpleReporter rep, boolean onlyTestGeneration) {
            //check if there are at least one variabilization test
            TestGenerationResult lastTestGenerationRes = TestGenerationResult.NO_TESTS_TO_RUN;
            if (world.getAllCommands().stream().noneMatch(Command::isVariabilizationTest) || onlyTestGeneration) {
                //we should run the original candidate to try to generate at least one variabilization test
                boolean atLeastOneTestGenerated = false;
                boolean atLeastOneCheckTest = false;
                Candidate original = Candidate.original(world);
                for (Command cmd : world.getAllCommands()) {
                    if (cmd.check || cmd.expects == 0) {
                        atLeastOneCheckTest = true;
                        try {
                            A4Solution result = evaluateCandidateWithCommand(original, cmd, rep);
                            if (result != null && result.satisfiable()) {
                                try {
                                    List<Command> variabilizationTests = TestsGenerator.getInstance().generateCEBasedTestsFor(result, world, cmd);
                                    if (!atLeastOneTestGenerated && !variabilizationTests.isEmpty()) {
                                        atLeastOneTestGenerated = true;
                                        lastTestGenerationRes = TestGenerationResult.GENERATED;
                                    }
                                    for (Command varTest : variabilizationTests) {
                                        DependencyGraph.getInstance().addLooseCommand(varTest);
                                    }
                                } catch (Exception e) {
                                    StringWriter sw = new StringWriter();
                                    e.printStackTrace(new PrintWriter(sw));
                                    String exceptionAsString = sw.toString();
                                    logger.info("Error while generating variabilization tests\n" +
                                            "Current candidate (should be original): " + original.toString() + "\n" +
                                            "Current command (should be either a check or a run with expect 0): " + cmd.toString() + "\n" +
                                            "Exception:\n" +  exceptionAsString
                                    );
                                }
                            }
                        } catch (Exception e) {
                            StringWriter sw = new StringWriter();
                            e.printStackTrace(new PrintWriter(sw));
                            String exceptionAsString = sw.toString();
                            logger.info("Error while running tests\n" +
                                    "Exception:\n" +  exceptionAsString
                            );
                        }
                    }
                }
                if (!atLeastOneTestGenerated) {
                    logger.info("Couldn't generate any variabilization test");
                }
                if (!world.getAllCommands().isEmpty() && !atLeastOneCheckTest)
                    lastTestGenerationRes = TestGenerationResult.NO_CHECK_TESTS_TO_RUN;
                if (atLeastOneCheckTest && !atLeastOneTestGenerated)
                    lastTestGenerationRes = TestGenerationResult.NO_FAILING_TEST;
            }
            return lastTestGenerationRes;
        }

        private TestGenerationResult generateInstanceTests(CompModule world, SimpleReporter rep) {
            TestGenerationResult testGenerationResult = TestGenerationResult.NO_TESTS_TO_RUN;
            Candidate original = Candidate.original(world);
            boolean atLeastOneTestGenerated = false;
            boolean atLeastOneRunCommand = false;
            for (Command c : world.getAllCommands()) {
                if (c.check || c.expects == 0)
                    continue;
                if (c.isGenerated())
                    continue;
                atLeastOneRunCommand = true;
                try {
                    A4Solution result = evaluateCandidateWithCommand(original, c, rep);
                    if (result != null && result.satisfiable()) {
                        try {
                            TestGenerationRequest request;
                            if (DependencyGraph.getInstance().trustedCommand(c, world)) {
                                request = TestGenerationRequest.createInstancePositiveTestRequestFromTrustedCommand(result, world, c);
                            } else {
                                request = TestGenerationRequest.createInstancePositiveAndNegativeTests(result, world, c);
                            }
                            List<Command> instanceTests = TestsGenerator.getInstance().generateTestsFor(request);
                            if (!atLeastOneTestGenerated && !instanceTests.isEmpty()) {
                                atLeastOneTestGenerated = true;
                                testGenerationResult = TestGenerationResult.GENERATED;
                            }
                            for (Command varTest : instanceTests) {
                                DependencyGraph.getInstance().addLooseCommand(varTest);
                            }
                        } catch (Exception e) {
                            StringWriter sw = new StringWriter();
                            e.printStackTrace(new PrintWriter(sw));
                            String exceptionAsString = sw.toString();
                            logger.info("Error while generating instance tests\n" +
                                    "Current candidate (should be original): " + original.toString() + "\n" +
                                    "Current command (should be a run expect >0): " + c.toString() + "\n" +
                                    "Exception:\n" +  exceptionAsString
                            );
                        }
                    }
                } catch (Exception e) {
                    StringWriter sw = new StringWriter();
                    e.printStackTrace(new PrintWriter(sw));
                    String exceptionAsString = sw.toString();
                    logger.info("Error while running tests\n" +
                            "Exception:\n" +  exceptionAsString
                    );
                }
            }
            if (!world.getAllCommands().isEmpty() && !atLeastOneRunCommand)
                testGenerationResult = TestGenerationResult.NO_RUN_TESTS_TO_RUN;
            if (atLeastOneRunCommand && !atLeastOneTestGenerated)
                testGenerationResult = TestGenerationResult.NO_INSTANCES_GENERATED;
            return testGenerationResult;
        }

        private EvaluationResults evaluateCandidateNormalEvaluation(Candidate candidate, SimpleReporter rep) {
            List<Command> cmds = MutantLab.getInstance().getCommandsToRunFor(candidate);
            return evaluateCandidate(candidate, cmds, rep, false);
        }

        private EvaluationResults evaluateCandidatePartialRepairEvaluation(Candidate candidate, SimpleReporter rep) {
            boolean atLeastOnePartialRepair = false;
            boolean repaired = true;
            Exception exception = null;
            Map<Command, Boolean> results = new HashMap<>();
            Map<Browsable, List<Command>> commandsForPartialRepair = MutantLab.getInstance().getCommandsToRunUsingPartialRepairFor(candidate);
            for (Browsable relatedPFA : candidate.getRelatedAssertionsAndFunctions()) {
                if (commandsForPartialRepair.containsKey(relatedPFA)) {
                    List<Command> independentCommands = commandsForPartialRepair.get(relatedPFA);
                    EvaluationResults partialResults = evaluateCandidate(candidate, independentCommands, rep, true);
                    if (partialResults.isDiscarded()) {
                        atLeastOnePartialRepair = false;
                        exception = partialResults.getException();
                        results.clear();
                        break;
                    }
                    if (repaired && !partialResults.isRepaired())
                        repaired = false;
                    for (Map.Entry<Command, Boolean> entry : partialResults.getCommandResults().entrySet()) {
                        Command command = entry.getKey();
                        Boolean result = entry.getValue();
                        if (!results.containsKey(command))
                            results.put(command, result);
                        else {
                            throw new IllegalStateException("Independent tests are not independent, command " + command.toString() + " was already executed");
                        }
                    }
                    if (partialResults.isRepaired() && independentCommands.size() > 0)
                        atLeastOnePartialRepair = true;
                }
            }
            if (repaired && exception == null) { //run the rest of the commands (priority)
                List<Command> restOfPriorityCommands = commandsForPartialRepair.get(MutantLab.PARTIAL_REPAIR_PRIORITY);
                EvaluationResults restOfPriorityCommandsResults = evaluateCandidate(candidate, restOfPriorityCommands, rep, false);
                if (restOfPriorityCommandsResults.isDiscarded()) {
                    repaired = false;
                    exception = restOfPriorityCommandsResults.getException();
                    results.clear();
                    atLeastOnePartialRepair = false;
                } else if (!restOfPriorityCommandsResults.isRepaired())
                    repaired = false;
            }
            if (repaired && exception == null) { //run the rest of the commands (non priority)
                List<Command> restOfNonPriorityCommands = commandsForPartialRepair.get(MutantLab.PARTIAL_REPAIR_NON_PRIORITY);
                EvaluationResults restOfNonPriorityCommandsResults = evaluateCandidate(candidate, restOfNonPriorityCommands, rep, false);
                if (restOfNonPriorityCommandsResults.isDiscarded()) {
                    repaired = false;
                    exception = restOfNonPriorityCommandsResults.getException();
                    results.clear();
                    atLeastOnePartialRepair = false;
                } else if (!restOfNonPriorityCommandsResults.isRepaired())
                    repaired = false;
            }
            return EvaluationResults.partialRepairEvaluationResults(repaired, atLeastOnePartialRepair, exception, results);
        }

        private EvaluationResults evaluateCandidate(Candidate current, List<Command> cmds, SimpleReporter rep, boolean partialRepair) {
            boolean repaired = true;
            Exception exception = null;
            Map<Command, Boolean> testsResults = partialRepair? new HashMap<>():null;
            CompModule world = current.getContext();
            for (int i = 0; i < cmds.size(); i++) {
                logger.info("Running cmd " + cmds.get(i).toString() + " with complexity " + DependencyGraph.getInstance().getCommandComplexity(cmds.get(i)));
                current.clearMutatedStatus();
                final Command cmd = cmds.get(i);
                if (cmd.isGenerated())
                    continue;
                try {
                    synchronized (SimpleReporter.class) {
                        latestModule = world;
                        latestKodkodSRC = ConstMap.make(map);
                    }
                    rep.tempfile = tempdir + File.separatorChar + i + ".cnf";
                    //cb(out, "bold", "Executing \"" + cmd + "\"\n");
                    //@mutation ==>> pass the mutation lab to the translator for mutation
                    A4Solution ai = evaluateCandidateWithCommand(current, cmd, rep);
                    //if the solution is as expected then continue, else break this mutation checks and continue with other mutation
                    if (ai != null) {
                        if (ai.satisfiable()) {
                            if (cmd.expects == 0 || (cmd.expects == -1 && cmd.check)) {
                                repaired = false;
                                if (variabilizationTestGeneration()) {
                                    try {
                                        List<Command> varTests = TestsGenerator.getInstance().generateCEBasedTestsFor(ai, world, cmd);
                                        for (Command varTest : varTests) {
                                            DependencyGraph.getInstance().addLooseCommand(varTest);
                                        }
                                    } catch (Exception e) {
                                        StringWriter sw = new StringWriter();
                                        e.printStackTrace(new PrintWriter(sw));
                                        String exceptionAsString = sw.toString();
                                        logger.info("Error while generating variabilization tests\n" +
                                                "Current candidate: " + current.toString() + "\n" +
                                                "Current command (should be either a check or a run with expect 0): " + cmd.toString() + "\n" +
                                                "Exception:\n" +  exceptionAsString
                                        );
                                    }
                                }
                            }
                        } else {
                            if (cmd.expects == 1 || (cmd.expects == -1 && !cmd.check)) {
                                repaired = false;
                            }
                        }
                    } else {
                        repaired = false;
                    }
                } catch (Exception e) {
                    repaired = false;
                    exception = e;
                }
                if (partialRepair)
                    testsResults.put(cmd, repaired);
                if (!repaired)
                    break;
            }
            if (partialRepair)
                return EvaluationResults.partialRepairEvaluationResults(repaired, repaired, exception, testsResults);
            return EvaluationResults.normalEvaluationResults(repaired, exception);
        }

        private A4Solution evaluateCandidateWithCommand(Candidate candidate, Command cmd, A4Reporter rep) throws Err {
            if (skipVerification())
                return null;
            A4Solution sol;
            RepairReport.getInstance().validationClockStart();
            try {
                sol = TranslateAlloyToKodkod.execute_commandFromBookWithMutation(rep, candidate.getContext().getAllReachableSigs(), cmd, options, candidate);
            } catch (Exception e) {
                RepairReport.getInstance().validationClockEnd();
                throw e;
            }
            RepairReport.getInstance().validationClockEnd();
            return sol;
        }

        private void fixParentRelationship(CompModule module) {
            for (Func f : module.getAllFunc()) {
                ParentRelationshipFixer parentRelationshipFixer = new ParentRelationshipFixer(f);
                parentRelationshipFixer.fixParentRelation();
            }
            for (Pair<String, Expr> namedAssertion : module.getAllAssertions()) {
                ParentRelationshipFixer parentRelationshipFixer = new ParentRelationshipFixer(namedAssertion.b, module);
                parentRelationshipFixer.fixParentRelation();
            }
            for (Pair<String, Expr> namedFact : module.getAllFacts()) {
                ParentRelationshipFixer parentRelationshipFixer = new ParentRelationshipFixer(namedFact.b, module);
                parentRelationshipFixer.fixParentRelation();
            }
        }

        private boolean skipVerification() {
            Optional<Object> configValue = MutationConfiguration.getInstance().getConfigValue(ConfigKey.REPAIR_DEBUG_SKIP_VERIFICATION);
            return configValue.map(o -> (Boolean) o).orElse((Boolean) ConfigKey.REPAIR_DEBUG_SKIP_VERIFICATION.defaultValue());
        }

        private int maxDepthForRepair() {
            Optional<Object> configValue = MutationConfiguration.getInstance().getConfigValue(ConfigKey.REPAIR_MAX_DEPTH);
            return configValue.map(o -> (Integer) o).orElse((Integer) ConfigKey.REPAIR_MAX_DEPTH.defaultValue());
        }

        private boolean detailedTestResults() {
            Optional<Object> configValue = MutationConfiguration.getInstance().getConfigValue(ConfigKey.REPAIR_DETAILED_TESTS_RESULTS);
            return configValue.map(o -> (Boolean) o).orElse((Boolean) ConfigKey.REPAIR_DETAILED_TESTS_RESULTS.defaultValue());
        }

        private long repairTimeout() {
            Optional<Object> timeoutConfigValue = MutationConfiguration.getInstance().getConfigValue(ConfigKey.REPAIR_TIMEOUT);
            return timeoutConfigValue.map(o -> (Long) o).orElse((Long) ConfigKey.REPAIR_TIMEOUT.defaultValue());
        }

        private boolean variabilizationTestGeneration() {
            Optional<Object> variabilizationConfigValue = MutationConfiguration.getInstance().getConfigValue(ConfigKey.REPAIR_VARIABILIZATION);
            boolean useVariabilization = variabilizationConfigValue.map(o -> (Boolean) o).orElse((Boolean) ConfigKey.REPAIR_VARIABILIZATION.defaultValue());
            if (useVariabilization) {
                Optional<Object> testGenerationConfigValue = MutationConfiguration.getInstance().getConfigValue(ConfigKey.REPAIR_VARIABILIZATION_TEST_GENERATION);
                return testGenerationConfigValue.map(o -> (Boolean) o).orElse((Boolean) ConfigKey.REPAIR_VARIABILIZATION_TEST_GENERATION.defaultValue());
            }
            return false;
        }

        private boolean writeTestsToFile() {
            return (boolean) MutationConfiguration.getInstance().getConfigValue(ConfigKey.TEST_GENERATION_OUTPUT_TO_FILES).orElse(false);
        }

    }

}

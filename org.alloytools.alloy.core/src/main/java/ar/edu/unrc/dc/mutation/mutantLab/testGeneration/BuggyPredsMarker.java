package ar.edu.unrc.dc.mutation.mutantLab.testGeneration;

import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.Func;
import edu.mit.csail.sdg.ast.Sig;
import edu.mit.csail.sdg.parser.CompModule;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

/**
 * Marks functions (and predicates) and facts as marked for mutation using a file, each line of the file references a function or fact to mark.
 * <ul>
 *     <li>To mark a function there must be a line with {@value FUNC_PREFIX} followed by the function's name, e.g.: <i>{@value FUNC_PREFIX}contains</i></li>
 *     <li>We support signature facts and named facts:</li>
 *     <ul>
 *         <li>For named facts, there must be a line with {@value FACT_PREFIX} followed by the fact's name, e.g.: <i>{@value FACT_PREFIX}acyclic</i></li>
 *         <li>For signature facts, there must be a line with {@value FACT_PREFIX}{@value SIG_PREFIX} followed by the signature's name, e.g.: <i>{@value FACT_PREFIX}{@value SIG_PREFIX}list</i></li>
 *     </ul>
 * </ul>
 */
public final class BuggyPredsMarker {

    private static final String FUNC_PREFIX = "func:";
    private static final String FACT_PREFIX = "fact:";
    private static final String SIG_PREFIX = "sig:";

    private static boolean marking = false;

    public static void markAllAsBuggy(CompModule module) {
        if (module == null)
            throw new IllegalArgumentException("null module");
        if (!module.markedEprsToMutate.isEmpty())
            throw new IllegalStateException("Can't mark buggy functions/predicates/facts when there are marked expressions in the model");
        module.getAllFunc().forEach(f -> {
            f.getBody().mutGenLimit(1);
            module.addMarkedExprToMutate(f.getBody());
        });
        module.getAllFacts().forEach(f -> {
            f.b.mutGenLimit(1);
            module.addMarkedExprToMutate(f.b);
        });
    }

    public static void markBuggyFunctions(CompModule module, Collection<String> funcs) {
        if (module == null)
            throw new IllegalArgumentException("null module");
        if (funcs == null)
            throw new IllegalArgumentException("null funcs");
        if (!module.markedEprsToMutate.isEmpty() && !marking)
            throw new IllegalStateException("Can't mark buggy predicate when there are marked expressions in the model");
        for (String func : funcs) {
            markFunction(module, func);
        }
    }

    public static void markBuggyNamedFacts(CompModule module, Collection<String> facts) {
        if (module == null)
            throw new IllegalArgumentException("null module");
        if (facts == null)
            throw new IllegalArgumentException("null facts");
        if (!module.markedEprsToMutate.isEmpty() && !marking)
            throw new IllegalStateException("Can't mark buggy named facts when there are marked expressions in the model");
        for (String fact : facts) {
            markFact(module, fact);
        }
    }

    public static void markBuggySignatureFacts(CompModule module, Collection<String> signatures) {
        if (module == null)
            throw new IllegalArgumentException("null module");
        if (signatures == null)
            throw new IllegalArgumentException("null signatures");
        if (!module.markedEprsToMutate.isEmpty() && !marking)
            throw new IllegalStateException("Can't mark signature facts when there are marked expressions in the model");
        for (String sig : signatures) {
            markSignatureFact(module, sig);
        }
    }

    public static void markBuggyFunctionsAndFacts(CompModule module, File funcsFile) throws IOException {
        if (funcsFile == null)
            throw new IllegalArgumentException("null funcs file");
        if (!funcsFile.exists())
            throw new IllegalArgumentException("funcs file " + funcsFile.toString() + " doesn't exist");
        if (!funcsFile.isFile())
            throw new IllegalArgumentException("funcs file " + funcsFile.toString() + " is not a file");
        if (!funcsFile.canRead())
            throw new IllegalArgumentException("funcs file " + funcsFile.toString() + " can't be read (no read permission)");
        List<String> funcs = new LinkedList<>();
        List<String> facts = new LinkedList<>();
        List<String> signatures = new LinkedList<>();
        for (String l : Files.readAllLines(funcsFile.toPath())) {
            String line = l.trim();
            if (line.isEmpty())
                continue;
            if (line.startsWith(FUNC_PREFIX)) {
                String func = line.replace(FUNC_PREFIX, "");
                funcs.add(func);
            } else if (line.startsWith(FACT_PREFIX)) {
                String factLine = line.replace(FACT_PREFIX, "");
                if (factLine.startsWith(SIG_PREFIX)) {
                    signatures.add(factLine.replace(SIG_PREFIX, ""));
                } else {
                    facts.add(factLine);
                }
            }
        }
        marking = true;
        markBuggyFunctions(module, funcs);
        markBuggyNamedFacts(module, facts);
        markBuggySignatureFacts(module, signatures);
        marking = false;
    }

    private static void markFunction(CompModule module, String func) {
        for (Func f : module.getAllFunc()) {
            if (removeAlias(f.label).compareTo(func) == 0) {
                f.getBody().mutGenLimit(1);
                module.addMarkedExprToMutate(f.getBody());
            }
        }
    }

    private static void markFact(CompModule module, String fact) {
        for (Pair<String, Expr> namedFact : module.getAllFacts()) {
            if (namedFact.a.equals("sig$fact"))
                continue;
            if (removeAlias(namedFact.a).compareTo(fact) == 0) {
                namedFact.b.mutGenLimit(1);
                module.addMarkedExprToMutate(namedFact.b);
            }
        }
    }

    private static void markSignatureFact(CompModule module, String sig) {
        for (Sig s : module.getAllSigs()) {
            if (removeAlias(s.label).compareTo(sig) == 0) {
                for (Expr sfact : s.getFacts()) {
                    sfact.mutGenLimit(1);
                    module.addMarkedExprToMutate(sfact);
                }
            }
        }
    }

    private static String removeAlias(String key) {
        int lastSlash = key.lastIndexOf('/');
        if (lastSlash == -1)
            return key;
        return key.substring(lastSlash + 1);
    }

}

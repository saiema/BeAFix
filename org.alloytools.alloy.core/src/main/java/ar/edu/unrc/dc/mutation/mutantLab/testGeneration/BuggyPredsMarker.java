package ar.edu.unrc.dc.mutation.mutantLab.testGeneration;

import edu.mit.csail.sdg.ast.Func;
import edu.mit.csail.sdg.parser.CompModule;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

public final class BuggyPredsMarker {

    public static void markBuggyFunctions(CompModule module, Collection<String> funcs) {
        if (module == null)
            throw new IllegalArgumentException("null module");
        if (funcs == null)
            throw new IllegalArgumentException("null funcs");
        if (!module.markedEprsToMutate.isEmpty())
            throw new IllegalStateException("Can't mark buggy predicate when there are marked expressions in the model");
        for (String func : funcs) {
            markFunction(module, func);
        }
    }

    public static void markBuggyFunctions(CompModule module, File funcsFile) throws IOException {
        if (funcsFile == null)
            throw new IllegalArgumentException("null funcs file");
        if (!funcsFile.exists())
            throw new IllegalArgumentException("funcs file " + funcsFile.toString() + " doesn't exist");
        if (!funcsFile.isFile())
            throw new IllegalArgumentException("funcs file " + funcsFile.toString() + " is not a file");
        if (!funcsFile.canRead())
            throw new IllegalArgumentException("funcs file " + funcsFile.toString() + " can't be read (no read permission)");
        List<String> funcs = new LinkedList<>();
        for (String l : Files.readAllLines(funcsFile.toPath())) {
            if (l.trim().isEmpty())
                continue;
            funcs.add(l);
        }
        markBuggyFunctions(module, funcs);
    }

    public static void markBuggyFunctions(CompModule module, Path funcsFilePath) throws IOException {
        if (funcsFilePath == null)
            throw new IllegalArgumentException("null funcs file path");
        markBuggyFunctions(module, funcsFilePath.toFile());
    }

    private static void markFunction(CompModule module, String func) {
        for (Func f : module.getAllFunc()) {
            if (removeAlias(f.label).compareTo(func) == 0) {
                f.getBody().mutGenLimit(1);
                module.addMarkedExprToMutate(f.getBody());
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

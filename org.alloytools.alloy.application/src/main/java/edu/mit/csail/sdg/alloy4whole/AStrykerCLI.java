package edu.mit.csail.sdg.alloy4whole;

import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.ast.Command;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.parser.CompUtil;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.stream.Stream;

public class AStrykerCLI {


    public static void main(String[] args) throws IOException {
        if (args.length != 1) throw new IllegalArgumentException("Only one argument (the module to repair) is required");
        String sourcefile = args[0];
        String source = toText(sourcefile);
        Pair<ConstList<Command>,ConstList<Expr>> moduleInfo = CompUtil.parseOneModuleToRepair_fromFile(sourcefile);
        if (moduleInfo.a == null || moduleInfo.a.isEmpty() || moduleInfo.b == null || moduleInfo.b.isEmpty()) {
            System.out.println("There are no marked expressions to repair or no commands for repair validation.");
            return;
        }
        AStryker astryker = new AStryker(moduleInfo.a, sourcefile, source);
        astryker.init();
        astryker.doRepair();
    }




    //==========================UTILITY METHODS AND FIELDS==========================

    private static String toText(String filePath) throws IOException {
        StringBuilder contentBuilder = new StringBuilder();
        Stream<String> stream = Files.lines( Paths.get(filePath), StandardCharsets.UTF_8);
        stream.forEach(s -> contentBuilder.append(s).append("\n"));
        return contentBuilder.toString();
    }

}

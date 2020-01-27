package edu.mit.csail.sdg.alloy4whole;

import edu.mit.csail.sdg.ast.Command;
import edu.mit.csail.sdg.parser.CompUtil;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Stream;

public class AStrykerCLI {


    public static void main(String[] args) throws IOException {
        if (args.length != 1) throw new IllegalArgumentException("Only one argument (the module to repair) is required");
        String sourcefile = args[0];
        String source = toText(sourcefile);
        List<Command> commands = CompUtil.parseOneModule_fromFile(source);
        if (commands == null || commands.isEmpty()) {
            System.out.println("There are no commands for repair validation.");
            return;
        }
        AStryker astryker = new AStryker(commands, sourcefile, source);
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

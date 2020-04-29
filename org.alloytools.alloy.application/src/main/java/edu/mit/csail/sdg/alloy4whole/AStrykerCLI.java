package edu.mit.csail.sdg.alloy4whole;

import edu.mit.csail.sdg.alloy4.A4Preferences;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.ast.Command;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.parser.CompUtil;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Optional;
import java.util.stream.Stream;

public class AStrykerCLI {


    public static void main(String[] args) throws IOException {
        if (args.length == 0) throw new IllegalArgumentException("At least one argument (the module to repair) is required");
        String sourcefile = args[0];
        if (args.length > 1)
            parseCommandLine(Arrays.copyOfRange(args, 1, args.length));
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

    private static void parseCommandLine(String[] args) {
        if (args.length == 0)
            return;
        boolean configKeyRead = false;
        String configKey = null;
        for (String arg : args) {
            if (arg.trim().isEmpty()) {
                continue;
            }
            if (arg.trim().startsWith("--")) {
                if (configKeyRead)
                    throw new IllegalArgumentException("Expecting value for " + configKey + " got " + arg.trim() + " instead");
                configKeyRead = true;
                configKey = arg.trim().substring(2);
            } else {
                if (!configKeyRead)
                    throw new IllegalArgumentException("Expecting config key but got a value instead " + arg.trim());
                setConfig(configKey, arg.trim());
                configKeyRead = false;
                configKey = null;
            }
        }
    }

    private static final String VARIABILIZATION = "variabilization";
    private static final String PARTIALREPAIR = "partialrepair";
    private static final String TIMEOUT = "timeout";
    private static final String MAXDEPTH = "maxdepth";
    private static void setConfig(String key, String value) {
        switch (key.toLowerCase()) {
            case VARIABILIZATION :
            case PARTIALREPAIR : {
                Optional<Boolean> varValue = parseBooleanValue(value);
                if (varValue.isPresent()) {
                    if (value.equals(VARIABILIZATION))
                        varValue.ifPresent(A4Preferences.AStrykerVariabilization::set);
                    else
                        varValue.ifPresent(A4Preferences.AStrykerPartialRepair::set);
                } else
                    throw new IllegalArgumentException("Invalid value for " + key + " expecting (true/false) but got " + value + " instead");
                break;
            }
            case TIMEOUT :
            case MAXDEPTH : {
                Optional<Integer> intVal = parseIntegerValue(value);
                intVal.ifPresent(intValue -> {
                    if (intValue < 0)
                        throw new IllegalArgumentException("Negative value for " + key + " (" + intValue + ")");
                    if (key.equals(TIMEOUT))
                        A4Preferences.AStrykerRepairTimeout.set(intValue);
                    else
                        A4Preferences.AStrykerRepairDepth.set(intValue);
                });
                if (!intVal.isPresent())
                    throw new IllegalArgumentException("Invalid value for " + key + " expecting integer but got " + value + " instead");
                break;
            }
            default : throw new IllegalArgumentException("Invalid configuration key " + key);
        }
    }

    private static Optional<Boolean> parseBooleanValue(String v) {
        if (v.isEmpty())
            return Optional.empty();
        switch (v.toLowerCase()) {
            case "true" : return Optional.of(Boolean.TRUE);
            case "false" : return Optional.of(Boolean.FALSE);
            default : return Optional.empty();
        }
    }

    private static Optional<Integer> parseIntegerValue(String v) {
        try {
            int intVal = Integer.parseInt(v);
            return Optional.of(intVal);
        } catch (NumberFormatException e) {
            return Optional.empty();
        }
    }

}

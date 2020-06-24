package edu.mit.csail.sdg.alloy4whole;

import ar.edu.unrc.dc.mutation.util.AStrykerConfigReader;
import edu.mit.csail.sdg.alloy4.A4Preferences;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.ast.Command;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.parser.CompUtil;
import org.alloytools.alloy.core.AlloyCore;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Optional;
import java.util.stream.Stream;

import static ar.edu.unrc.dc.mutation.util.AStrykerConfigReader.Config_key.*;

public class AStrykerCLI {


    public static void main(String[] args) throws IOException {
        AlloyCore.debug = false;
        if (args.length == 0) throw new IllegalArgumentException("At least one argument (the module to repair) is required");
        String sourcefile = args[0];
        if (args.length > 1) {
            parseCommandLine(Arrays.copyOfRange(args, 1, args.length));
            AStrykerConfigReader.getInstance().saveConfig();
        }
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

    private static final String VARIABILIZATION_KEY = "variabilization";
    private static final String VARIABILIZATION_TEST_GENERATION_KEY = "testgeneration";
    private static final String VARIABILIZATION_SAME_TYPE_KEY = "sametypes";
    private static final String PARTIALREPAIR_KEY = "partialrepair";
    private static final String USEPOTOVALIDATE_KEY = "validatewithpo";
    private static final String TIMEOUT_KEY = "timeout";
    private static final String MAXDEPTH_KEY = "maxdepth";
    private static void setConfig(String key, String value) {
        switch (key.toLowerCase()) {
            case VARIABILIZATION_KEY:
            case VARIABILIZATION_TEST_GENERATION_KEY:
            case VARIABILIZATION_SAME_TYPE_KEY:
            case USEPOTOVALIDATE_KEY:
            case PARTIALREPAIR_KEY: {
                Optional<Boolean> varValue = parseBooleanValue(value);
                if (varValue.isPresent()) {
                    switch (key) {
                        case VARIABILIZATION_KEY: {
                            AStrykerConfigReader.getInstance().setBooleanArgument(VARIABILIZATION, varValue.get());
                            break;
                        }
                        case VARIABILIZATION_TEST_GENERATION_KEY: {
                            AStrykerConfigReader.getInstance().setBooleanArgument(VARIABILIZATION_TEST_GENERATION, varValue.get());
                            break;
                        }
                        case PARTIALREPAIR_KEY: {
                            Boolean partialRepair = varValue.get();
                            AStrykerConfigReader.getInstance().setBooleanArgument(PARTIAL_REPAIR, partialRepair);
                            if (partialRepair) {
                                AStrykerConfigReader.getInstance().setBooleanArgument(PARTIAL_REPAIR_INDEPENDENT_TESTS_FOR_ALL, Boolean.FALSE);
                                AStrykerConfigReader.getInstance().setBooleanArgument(PARTIAL_REPAIR_FULLCGRAPH_VALIDATION, Boolean.TRUE);
                            }
                            break;
                        }
                        case USEPOTOVALIDATE_KEY: {
                            AStrykerConfigReader.getInstance().setBooleanArgument(USE_PO_TO_VALIDATE, varValue.get());
                            break;
                        }
                        case VARIABILIZATION_SAME_TYPE_KEY: {
                            AStrykerConfigReader.getInstance().setBooleanArgument(VARIABILIZATION_SAME_TYPE, varValue.get());
                            varValue.ifPresent(A4Preferences.AStrykerVariabilizationUseSameType::set);
                            break;
                        }
                    }
                } else
                    throw new IllegalArgumentException("Invalid value for " + key + " expecting (true/false) but got " + value + " instead");
                break;
            }
            case TIMEOUT_KEY:
            case MAXDEPTH_KEY: {
                Optional<Integer> intVal = parseIntegerValue(value);
                intVal.ifPresent(intValue -> {
                    if (intValue < 0)
                        throw new IllegalArgumentException("Negative value for " + key + " (" + intValue + ")");
                    if (key.equals(TIMEOUT_KEY))
                        AStrykerConfigReader.getInstance().setIntArgument(TIMEOUT, intVal.get());
                    else
                        AStrykerConfigReader.getInstance().setIntArgument(MAX_DEPTH, intVal.get());
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

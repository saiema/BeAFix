package edu.mit.csail.sdg.alloy4whole;

import ar.edu.unrc.dc.mutation.util.AStrykerConfigReader;
import edu.mit.csail.sdg.alloy4.A4Preferences;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.ast.Command;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.parser.CompUtil;
import org.alloytools.alloy.core.AlloyCore;
import edu.mit.csail.sdg.alloy4whole.SimpleReporter.SimpleTaskRepair1.ASTRYKER_MODE;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import static ar.edu.unrc.dc.mutation.util.AStrykerConfigReader.Config_key.*;

public class AStrykerCLI {

    private static ASTRYKER_MODE astryker_mode = ASTRYKER_MODE.REPAIR;

    private static final String REPAIR = "REPAIR";
    private static final String TESTGEN = "TESTS";
    private static final String CHECK = "CHECK";
    private static final String MUTANTGEN = "MUTANTS";
    public static void main(String[] args) throws IOException {
        AlloyCore.debug = false;
        if (args.length == 0) throw new IllegalArgumentException("At least one argument (the module to repair) is required");
        String sourcefile = args[0];
        if (args.length > 1) {
            String mode = args[1];
            if (mode.compareToIgnoreCase(REPAIR) == 0) {
                astryker_mode = ASTRYKER_MODE.REPAIR;
            } else if (mode.compareToIgnoreCase(TESTGEN) == 0) {
                astryker_mode = ASTRYKER_MODE.TESTGENERATION;
                AStrykerConfigReader.getInstance().setBooleanArgument(TEST_GENERATION_OUTPUT_TO_FILES, true);
            } else if (mode.compareToIgnoreCase(CHECK) == 0) {
                astryker_mode = ASTRYKER_MODE.CHECK;
            } else if (mode.compareToIgnoreCase(MUTANTGEN) == 0) {
                astryker_mode = ASTRYKER_MODE.MUTANTGENERATION;
            } else {
                throw new IllegalArgumentException("The second argument must either be REPAIR, TESTS, or CHECK (got " + mode + " instead)");
            }
            if (!astryker_mode.equals(ASTRYKER_MODE.CHECK)) {
                parseCommandLine(Arrays.copyOfRange(args, 2, args.length));
                AStrykerConfigReader.getInstance().saveConfig();
            }
        }
        String source = toText(sourcefile);
        List<Command> commands;

        if (astryker_mode.equals(ASTRYKER_MODE.REPAIR)) {
            Pair<ConstList<Command>, ConstList<Expr>> moduleInfo = CompUtil.parseOneModuleToRepair_fromFile(sourcefile);
            if (moduleInfo.a == null || moduleInfo.a.isEmpty() || moduleInfo.b == null || moduleInfo.b.isEmpty()) {
                System.out.println("There are no marked expressions to repair or no commands for repair validation.");
                return;
            }
            commands = moduleInfo.a;
        } else {
            commands = CompUtil.parseOneModule_fromFile(sourcefile);
        }
        AStryker astryker = new AStryker(commands, sourcefile, source);
        astryker.init();
        astryker.doAStryker(astryker_mode);
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
                if (astryker_mode.equals(ASTRYKER_MODE.REPAIR))
                    setConfig_repair(configKey, arg.trim());
                else if (astryker_mode.equals(ASTRYKER_MODE.MUTANTGENERATION))
                    setConfig_mutantgen(configKey, arg.trim());
                else if (astryker_mode.equals(ASTRYKER_MODE.TESTGENERATION))
                    setConfig_tests(configKey, arg.trim());
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
    private static final String TEST_GENERATION_MAX_TEST_PER_COMMAND_KEY = "maxtestspercommand";
    private static final String TEST_GENERATION_TESTS_PER_STEP_KEY = "testspergeneration";
    private static void setConfig_repair(String key, String value) {
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
            case TIMEOUT_KEY: {
                int toValue = getIntegerValue(TIMEOUT_KEY, value);
                AStrykerConfigReader.getInstance().setIntArgument(TIMEOUT, toValue);
                break;
            }
            case TEST_GENERATION_MAX_TEST_PER_COMMAND_KEY: {
                int maxTestsPerCommand = getIntegerValue(TEST_GENERATION_MAX_TEST_PER_COMMAND_KEY, value);
                AStrykerConfigReader.getInstance().setIntArgument(TEST_GENERATION_MAX_TESTS_PER_COMMAND, maxTestsPerCommand);
                break;
            }
            case TEST_GENERATION_TESTS_PER_STEP_KEY: {
                int testsPerGeneration = getIntegerValue(TEST_GENERATION_TESTS_PER_STEP_KEY, value);
                AStrykerConfigReader.getInstance().setIntArgument(TEST_GENERATION_TESTS_PER_STEP, testsPerGeneration);
                break;
            }
            case MAXDEPTH_KEY: {
                int maxDepth = getIntegerValue(MAXDEPTH_KEY, value);
                AStrykerConfigReader.getInstance().setIntArgument(MAX_DEPTH, maxDepth);
                break;
            }
            default : throw new IllegalArgumentException("Invalid configuration key for automatic repair " + key);
        }
    }

    private static final String TESTS_TO_GENERATE_KEY = "generate";
    private static final String OUTPUT_FOLDER_KEY = "out";
    private static void setConfig_tests(String key, String value) {
        switch (key.toLowerCase()) {
            case TESTS_TO_GENERATE_KEY: {
                int tests = getIntegerValue(TESTS_TO_GENERATE_KEY, value);
                if (tests < 1)
                    throw new IllegalArgumentException("generate value must be 1 or more (got " + tests + " )");
                AStrykerConfigReader.getInstance().setIntArgument(TEST_GENERATION_MAX_TESTS_PER_COMMAND, tests);
                AStrykerConfigReader.getInstance().setIntArgument(TEST_GENERATION_TESTS_PER_STEP, tests);
                break;
            }
            case OUTPUT_FOLDER_KEY: {
                File outFolder = new File(value);
                if (!outFolder.exists())
                    throw new IllegalArgumentException("tests output folder doesn't exists ( " + value + ")");
                if (!outFolder.isDirectory())
                    throw new IllegalArgumentException("tests output folder is not a folder ( " + value + ")");
                if (!outFolder.canExecute() || !outFolder.canWrite())
                    throw new IllegalArgumentException("Insufficient access to output folder ( " + value + ")");
                AStrykerConfigReader.getInstance().setStringArgument(TEST_GENERATION_OUTPUT_FOLDER, value);
                break;
            }
            default : throw new IllegalArgumentException("Invalid configuration key for test generation " + key);
        }
    }

    private static final String MUTANT_GENERATION_CHECK_KEY = "check";
    private static final String MUTANT_GENERATION_LIMIT_KEY = "limit";
    private static void setConfig_mutantgen(String key, String value) {
        switch (key.toLowerCase()) {
            case TIMEOUT_KEY: {
                int toValue = getIntegerValue(TIMEOUT_KEY, value);
                AStrykerConfigReader.getInstance().setIntArgument(TIMEOUT, toValue);
                break;
            }
            case MAXDEPTH_KEY: {
                int maxDepth = getIntegerValue(MAXDEPTH_KEY, value);
                AStrykerConfigReader.getInstance().setIntArgument(MAX_DEPTH, maxDepth);
                break;
            }
            case OUTPUT_FOLDER_KEY: {
                File outFolder = new File(value);
                if (!outFolder.exists())
                    throw new IllegalArgumentException("mutants output folder doesn't exists ( " + value + ")");
                if (!outFolder.isDirectory())
                    throw new IllegalArgumentException("mutants output folder is not a folder ( " + value + ")");
                if (!outFolder.canExecute() || !outFolder.canWrite())
                    throw new IllegalArgumentException("Insufficient access to mutants output folder ( " + value + ")");
                AStrykerConfigReader.getInstance().setStringArgument(MUTANTS_GENERATION_OUTPUT_FOLDER, value);
                break;
            }
            case MUTANT_GENERATION_CHECK_KEY: {
                boolean booleanValue = getBooleanValue(key, value);
                AStrykerConfigReader.getInstance().setBooleanArgument(MUTANTS_GENERATION_CHECK, booleanValue);
                break;
            }
            case MUTANT_GENERATION_LIMIT_KEY: {
                int limit = getIntegerValue(MUTANT_GENERATION_LIMIT_KEY, value);
                AStrykerConfigReader.getInstance().setIntArgument(MUTANTS_GENERATION_LIMIT, limit);
                break;
            }
            default : throw new IllegalArgumentException("Invalid configuration key for mutants generation " + key);
        }
    }

    private static Boolean getBooleanValue(String key, String originalValue) {
        Optional<Boolean> boolVal = parseBooleanValue(originalValue);
        if (!boolVal.isPresent())
            throw new IllegalArgumentException("Invalid value for " + key + " expecting boolean but got " + originalValue + " instead");
        return boolVal.get();
    }

    private static Integer getIntegerValue(String key, String originalValue) {
        Optional<Integer> intVal = parseIntegerValue(originalValue);
        intVal.ifPresent(intValue -> {
            if (intValue < 0)
                throw new IllegalArgumentException("Negative value for " + key + " (" + intValue + ")");
        });
        if (!intVal.isPresent())
            throw new IllegalArgumentException("Invalid value for " + key + " expecting integer but got " + originalValue + " instead");
        return intVal.get();
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

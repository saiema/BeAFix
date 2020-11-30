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
    private static final String HELP = "--help";
    public static void main(String[] args) throws IOException {
        AlloyCore.debug = false;
        if (args.length == 0) throw new IllegalArgumentException("At least one argument (the module to repair) is required");
        if (args[0].compareToIgnoreCase(HELP) == 0) {
            if (args.length > 1)
                printHelp(args[1].toUpperCase());
            else
                printHelp();
            return;
        }
        String sourcefile = args[0];;
        if (args.length > 1) {
            String mode = args[1];
            if (mode.compareToIgnoreCase(REPAIR) == 0) {
                astryker_mode = ASTRYKER_MODE.REPAIR;
                AStrykerConfigReader.getInstance().setBooleanArgument(TEST_GENERATION_AREPAIR_INTEGRATION, false);
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
            AStrykerConfigReader.getInstance().setStringArgument(HACKS_CANDIDATE_HASHES, "");
            if (mode.compareToIgnoreCase(TESTGEN) != 0) {
                AStrykerConfigReader.getInstance().setBooleanArgument(TEST_GENERATION_INSTANCES_TESTS_GENERATION, false);
                AStrykerConfigReader.getInstance().setStringArgument(TEST_GENERATION_INSTANCES_TESTS_GENERATION_BUGGY_FUNCS_FILE, "");
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
    private static final String PARTIALREPAIR_PRUNING_KEY = "prpruning";
    private static final String PARTIALREPAIR_PRIORITIZATION_KEY = "prprioritization";
    private static final String USEPOTOVALIDATE_KEY = "validatewithpo";
    private static final String TIMEOUT_KEY = "timeout";
    private static final String MAXDEPTH_KEY = "maxdepth";
    private static final String TEST_GENERATION_MAX_TEST_PER_COMMAND_KEY = "maxtestspercommand";
    private static final String TEST_GENERATION_TESTS_PER_STEP_KEY = "testspergeneration";
    private static final String HACKS_CANDIDATE_HASHES_KEY = "hackschashes";
    private static void setConfig_repair(String key, String value) {
        switch (key.toLowerCase()) {
            case VARIABILIZATION_KEY:
            case VARIABILIZATION_TEST_GENERATION_KEY:
            case VARIABILIZATION_SAME_TYPE_KEY:
            case USEPOTOVALIDATE_KEY:
            case PARTIALREPAIR_PRUNING_KEY:
            case PARTIALREPAIR_PRIORITIZATION_KEY:
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
                            boolean partialRepair = varValue.get();
                            AStrykerConfigReader.getInstance().setBooleanArgument(PARTIAL_REPAIR, partialRepair);
                            AStrykerConfigReader.getInstance().setBooleanArgument(PARTIAL_REPAIR_PRUNING, partialRepair);
                            if (partialRepair) {
                                AStrykerConfigReader.getInstance().setBooleanArgument(PARTIAL_REPAIR_INDEPENDENT_TESTS_FOR_ALL, Boolean.FALSE);
                                AStrykerConfigReader.getInstance().setBooleanArgument(PARTIAL_REPAIR_FULLCGRAPH_VALIDATION, Boolean.TRUE);
                            }
                            break;
                        }
                        case PARTIALREPAIR_PRUNING_KEY: {
                            boolean partialRepairPruning = varValue.get();
                            AStrykerConfigReader.getInstance().setBooleanArgument(PARTIAL_REPAIR_PRUNING, partialRepairPruning);
                            if (partialRepairPruning) {
                                AStrykerConfigReader.getInstance().setBooleanArgument(PARTIAL_REPAIR_INDEPENDENT_TESTS_FOR_ALL, Boolean.FALSE);
                                AStrykerConfigReader.getInstance().setBooleanArgument(PARTIAL_REPAIR_FULLCGRAPH_VALIDATION, Boolean.TRUE);
                            }
                            break;
                        }
                        case PARTIALREPAIR_PRIORITIZATION_KEY: {
                            boolean partialRepairPrioritization = varValue.get();
                            AStrykerConfigReader.getInstance().setBooleanArgument(PARTIAL_REPAIR, partialRepairPrioritization);
                            if (partialRepairPrioritization) {
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
            case HACKS_CANDIDATE_HASHES_KEY: {
                if (value.trim().isEmpty()) {
                    AStrykerConfigReader.getInstance().setStringArgument(HACKS_CANDIDATE_HASHES, value);
                    break;
                }
                File chashes = new File(value);
                if (chashes.exists())
                    throw new IllegalArgumentException("candidate hashes file already exists ( " + value + ")");
                AStrykerConfigReader.getInstance().setStringArgument(HACKS_CANDIDATE_HASHES, value);
                break;
            }
            default : throw new IllegalArgumentException("Invalid configuration key for automatic repair " + key);
        }
    }

    private static final String TESTS_TO_GENERATE_KEY = "generate";
    private static final String OUTPUT_FOLDER_KEY = "out";
    private static final String TESTS_AREPAIR_INTEGRATION_KEY = "arepair";
    private static final String TESTS_NAME_KEY = "tname";
    private static final String TESTS_NAME_STARTING_INDEX_KEY = "tindex";
    private static final String MODEL_OVERRIDING_KEY = "modeloverriding";
    private static final String MODEL_OVERRIDING_FOLDER_KEY = "mofolder";
    private static final String INSTANCES_TESTS_GENERATION_KEY = "itests";
    private static final String BUGGY_FUNCS_FILE_KEY = "buggyfuncs";
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
            case TESTS_AREPAIR_INTEGRATION_KEY: {
                boolean arepairIntegration = getBooleanValue(TESTS_AREPAIR_INTEGRATION_KEY, value);
                AStrykerConfigReader.getInstance().setBooleanArgument(TEST_GENERATION_AREPAIR_INTEGRATION, arepairIntegration);
                break;
            }
            case TESTS_NAME_KEY: {
                if (value.trim().isEmpty())
                    throw new IllegalArgumentException("test name (tname value) can't be empty");
                AStrykerConfigReader.getInstance().setStringArgument(TEST_GENERATION_NAME, value.trim());
                break;
            }
            case TESTS_NAME_STARTING_INDEX_KEY: {
                int intValue = getIntegerValue(TESTS_NAME_STARTING_INDEX_KEY, value);
                if (intValue < 0)
                    throw new IllegalArgumentException("test name index (tindex value) must be positive (" + intValue + ")");
                AStrykerConfigReader.getInstance().setIntArgument(TEST_GENERATION_NAME_STARTING_INDEX, intValue);
                break;
            }
            case MODEL_OVERRIDING_KEY: {
                boolean boolValue = getBooleanValue(MODEL_OVERRIDING_KEY, value);
                AStrykerConfigReader.getInstance().setBooleanArgument(TEST_GENERATION_USE_MODEL_OVERRIDING, boolValue);
                break;
            }
            case MODEL_OVERRIDING_FOLDER_KEY: {
                File overridesFolder = new File(value);
                if (!overridesFolder.exists())
                    throw new IllegalArgumentException("model overriding folder doesn't exists ( " + value + ")");
                if (!overridesFolder.isDirectory())
                    throw new IllegalArgumentException("model overriding folder is not a folder ( " + value + ")");
                if (!overridesFolder.canExecute() || !overridesFolder.canRead())
                    throw new IllegalArgumentException("Insufficient access to model overriding folder ( " + value + ")");
                AStrykerConfigReader.getInstance().setStringArgument(TEST_GENERATION_MODEL_OVERRIDING_FOLDER, value);
                break;
            }
            case INSTANCES_TESTS_GENERATION_KEY: {
                boolean boolValue = getBooleanValue(INSTANCES_TESTS_GENERATION_KEY, value);
                AStrykerConfigReader.getInstance().setBooleanArgument(TEST_GENERATION_INSTANCES_TESTS_GENERATION, boolValue);
                break;
            }
            case BUGGY_FUNCS_FILE_KEY: {
                File buggyFuncs = new File(value);
                if (!buggyFuncs.exists())
                    throw new IllegalArgumentException("buggy functions file doesn't exists ( " + value + ")");
                if (!buggyFuncs.isFile())
                    throw new IllegalArgumentException("buggy functions file is not a file ( " + value + ")");
                if (!buggyFuncs.canRead())
                    throw new IllegalArgumentException("Insufficient access to buggy functions file ( " + value + ")");
                AStrykerConfigReader.getInstance().setStringArgument(TEST_GENERATION_INSTANCES_TESTS_GENERATION_BUGGY_FUNCS_FILE, value);
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

    private static void printHelp() {
        String sb = "BeAFix" + "\n" +
                "--help                               :   To print this help." + "\n" +
                "--help (mode)                        :   To print mode specific help" + "\n" +
                "<path to model> REPAIR [options]     :   To repair a model." + "\n" +
                "<path to model> TESTS [options]      :   To generate tests from counterexamples." + "\n" +
                "<path to model> MUTANTS [options]    :   To generate mutants from a model." + "\n" +
                "<path to model> CHECK                :   To validate a model (similar to running execute all in GUI mode)." + "\n" +
                "\n" +
                "note: all options will be saved in a astryker.properties file, so using all required options each time is suggested.";
        System.out.println(sb);
    }

    private static void printHelp(String mode) {
        switch (mode) {
            case REPAIR: {printRepairHelp(); break;}
            case TESTGEN: {printTestGenerationHelp(); break;}
            case MUTANTGEN: {printMutantGenerationHelp(); break;}
            case CHECK: {printValidationHelp(); break;}
            default: printHelp();
        }
    }

    private static void printRepairHelp() {
        String sb = "Model repair mode, this will take a model with marked expressions and try to repair it." + "\n" +
                "A marked expression has the form {#m#([vars]) expression} where vars is a comma separated list of variables" + "\n" +
                "of which the expression depends. These are used by the variabilization pruning technique." + "\n" +
                "\n" +
                "Example without variables: {#m#() some x : T | P[x]}." + "\n" +
                "Example with variables: some x : T | {#m#(x) P[x]}." + "\n" +
                "\n" +
                "Usage:" + "\n" +
                "<path to model> REPAIR [options]" + "\n" +
                "\n" +
                "Options:" + "\n" +
                "--maxdepth <int>             :     How many mutations are allowed per marked expressions (default is 2)." + "\n" +
                "--timeout <int>              :     Time budget (in minutes) for the repair process (default is 0, unlimited)." + "\n" +
                "--variabilization <boolean>  :     Enables/disables variabilization pruning technique (default is false)." + "\n" +
                "--sametypes <boolean>        :     If variabilization will use expressions exact types for constructed relation or more general ones (default and suggested is false)." + "\n" +
                "--testgeneration <boolean>   :     When variabilization is enabled, specific commands can be set to be used only by the variabilization technique" + "\n" +
                "                                   these commands must be prefixed by #t#. Any signature that is used only by this commands can also be marked by #t#" + "\n" +
                "                                   which will make the mutation process ignore these. Variabilization commands must be run <pred> expect 1." + "\n" +
                "                                   When test generation is enabled, commands and predicates will be automatically constructed from counterexamples." + "\n" +
                "                                   Default is false, but the suggested is using true." + "\n" +
                "--maxtestspercommand <int>   :     How many tests can be constructed from counterexamples from a particular command (default is 4)." + "\n" +
                "--testspergeneration <int>   :     How many tests can be constructed each time a counterexample is found. This is an upper bound when there are several counterexamples." + "\n" +
                "--partialrepair <boolean>    :     Enables/disables the partial repair pruning and partial repair prioritization." + "\n" +
                "                                   This option only works when there is more than one bugged predicate, function, or assertion" + "\n" +
                "                                   and no fact is bugged. And requires that at least one of bugged predicate, function or assertions has" + "\n" +
                "                                   a related command which doesn't call (directly or indirectly) another of the bugged ones." + "\n" +
                "--prpruning <boolean>        :     Enables/disables partial repair pruning." + "\n" +
                "                                   This option only works when there is more than one bugged predicate, function, or assertion" + "\n" +
                "                                   and no fact is bugged. And requires that at least one of bugged predicate, function or assertions has" + "\n" +
                "                                   a related command which doesn't call (directly or indirectly) another of the bugged ones." + "\n" +
                "--prprioritization <boolean> :     Enables/disables partial repair prioritization." + "\n" +
                "                                   This option only works when there is more than one bugged predicate, function, or assertion" + "\n" +
                "                                   and no fact is bugged. And requires that at least one of bugged predicate, function or assertions has" + "\n" +
                "                                   a related command which doesn't call (directly or indirectly) another of the bugged ones." + "\n" +
                "--validatewithpo <boolean>   :     Enables/disables repair validation against commands prefixed by #po#, these commands will not be used" + "\n" +
                "                                   during the repair process but only used after a repair is found to validate it." + "\n";
        System.out.println(sb);
    }

    private static void printTestGenerationHelp() {
        String sb = "Test generation mode, this will take a model with at least one run <pred> expect 0, or check <assertion> command" + "\n" +
                "and will generate tests from obtained counterexamples." + "\n" +
                "\n" +
                "Usage:" + "\n" +
                "<path to model> TESTS [options]" + "\n" +
                "\n" +
                "Options:" + "\n" +
                "--generate <int>                     :     How many tests to generate (default is 4)." + "\n" +
                "--out <path to existing folder>      :     Where to store tests (default is the model's folder)." + "\n" +
                "--arepair <boolean>                  :     Enables/disables arepair integration (default is false)." + "\n" +
                "--tname <name>                       :     Base name for generated tests, all tests will start with name and be followed by an index." + "\n" +
                "                                           if name is empty (or a string with all blank space) the base name will be that of the command" + "\n" +
                "                                           from which the counterexample came, in this case no index will be used." + "\n" +
                "--tindex <int>                       :     A positive number to be used with a non empty tname that defines the starting index for generated tests" + "\n" +
                "--modeloverriding <boolean>          :     Enables/disables model overriding (can ignore signatures and use functions instead of fields). This requires" + "\n" +
                "                                           a folder with files <model name>.overrides with the following lines" + "\n" +
                "                                              * signatures.<signature name>=IGNORE : to ignore a signature in the generated test." + "\n" +
                "                                              * field.<field name>=IGNORE : to ignore a field in the generated test." + "\n" +
                "                                              * field.<signature name>=function.<no arguments function> : to use a function instead of a field in the generated test" + "\n" +
                "                                           An example can be found in modelOverrides/ordering.overrides." + "\n" +
                "                                           This feature is disabled by default." + "\n" +
                "--mofolder <path to existing folder> :     From which directory to load the .overrides files, default is modelOverrides." + "\n" +
                "--itests <boolean>                   :     Enables/disables instance based test generation. This will generate three types of tests:" + "\n" +
                "                                              * positive trusted tests : the instances from which these tests come from are positive and" + "\n" +
                "                                                does not they don't involve calling a bugged function or predicate." + "\n" +
                "                                              * positive untrusted tests : the instances from which these tests come from are positive but" + "\n" +
                "                                                involve calling at least one buggy function or predicate." + "\n" +
                "                                              * negative tests : tests are made such that the originating property is negated." + "\n" +
                "                                           This tests are created from run expect > 0 commands, buggy facts are not supported." + "\n" +
                "                                           This feature is disabled by default." + "\n" +
                "--buggyfuncs <path to existing file> :     When there are no expressions marked, lines in this file will be used to define which functions/predicates" + "\n" +
                "                                           are to be considered as buggy. This is used in conjunction with the <itests> feature." + "\n" +
                "                                           The default value is empty." + "\n";
        System.out.println(sb);
    }

    private static void printValidationHelp() {
        String sb = "Model validation mode, this will take a model and run all declared commands." + "\n" +
                "\n" +
                "Usage:" + "\n" +
                "<path to model> CHECK" + "\n";
        System.out.println(sb);
    }

    private static void printMutantGenerationHelp() {
        String sb = "Mutant generation mode, this will take a model an generate mutants." + "\n" +
                "If the model have marked expressions then mutants will be generated for these, otherwise it will generate mutants" + "\n" +
                "for every supported expression." + "\n" +
                "\n" +
                "Usage:" + "\n" +
                "<path to model> MUTANTS [options]" + "\n" +
                "\n" +
                "Options:" + "\n" +
                "--out <path to existing folder>     :     Where to store the mutated models (default is the model's directory)." + "\n" +
                "--timeout <int>                     :     Time budget (in minutes) for the mutation process (default is 0, unlimited)." + "\n" +
                "--maxdepth <int>                    :     How many mutations are allowed per marked expressions (default is 2)." + "\n" +
                "--limit <int>                       :     How many mutations in total will can a mutant have (default is 1)" + "\n" +
                "--check <boolean>                   :     Enables/disables mutant validation, commands are run to check that no error is thrown." + "\n";
        System.out.println(sb);
    }

}

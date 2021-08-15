package ar.edu.unrc.dc.mutation.mutantLab.testGeneration;

import ar.edu.unrc.dc.mutation.CheatingIsBadMkay;
import ar.edu.unrc.dc.mutation.Cheats;
import ar.edu.unrc.dc.mutation.MutationConfiguration;
import ar.edu.unrc.dc.mutation.mutantLab.MutantLab;
import ar.edu.unrc.dc.mutation.util.ExpressionEvaluator;
import ar.edu.unrc.dc.mutation.util.RepairReport;
import ar.edu.unrc.dc.mutation.visitors.FunctionsCollector;
import ar.edu.unrc.dc.mutation.visitors.SearchCall;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.*;
import edu.mit.csail.sdg.ast.Sig.Field;
import edu.mit.csail.sdg.parser.CompModule;
import edu.mit.csail.sdg.translator.A4Solution;
import edu.mit.csail.sdg.translator.A4TupleSet;
import kodkod.ast.Relation;
import kodkod.engine.Evaluator;
import kodkod.instance.Tuple;
import kodkod.instance.TupleSet;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.*;
import java.util.Map.Entry;
import java.util.logging.FileHandler;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;
import java.util.stream.Collectors;

import static ar.edu.unrc.dc.mutation.MutationConfiguration.ConfigKey.*;
import static ar.edu.unrc.dc.mutation.mutantLab.testGeneration.TestGeneratorHelper.*;

public class TestsGenerator {

    private static final Logger logger = Logger.getLogger(TestsGenerator.class.getName());

    static {
        try {
            // This block configures the logger with handler and formatter
            FileHandler fh = new FileHandler("TestsGenerator.log");
            logger.addHandler(fh);
            SimpleFormatter formatter = new SimpleFormatter();
            fh.setFormatter(formatter);
        } catch (SecurityException | IOException e) {
            e.printStackTrace();
        }
    }

    private static final String FACTS_PROPERTY = "facts";
    private final Map<String, Integer> testsPerProperty;
    private final String testName;
    private int testIndex;
    private int subTestIndex;
    private String testType = EMPTY_TYPE;
    private static final String EMPTY_TYPE = "";
    private static final String FROM_COUNTEREXAMPLE_TYPE = "CE";
    private static final String FROM_PREDICATE_TYPE = "PRED";
    private String testNamePostfix = EMPTY_POSTFIX;
    private static final String EMPTY_POSTFIX = "";
    private static final String POS_POSTFIX = "POS";
    private static final String NEG_POSTFIX = "NEG";

    private static TestsGenerator instance;

    public static TestsGenerator getInstance() {
        if (instance == null) {
            instance = new TestsGenerator();
        }
        return instance;
    }

    public static void destroyInstance() {
        if (instance == null)
            throw new IllegalStateException("No TestsGenerator instance to destroy");
        instance = null;
    }

    private TestsGenerator() {
        testsPerProperty = new TreeMap<>();
        String testBaseName = testBaseName();
        if (!testBaseName.isEmpty()) {
            this.testName = testBaseName;
            this.testIndex = testBaseNameStartingIndex();
        } else {
            this.testName = null;
        }
        this.subTestIndex = 0;
        if (useModelOverriding()) {
            String overridingFolder = modelOverridingFolder();
            if (!overridingFolder.trim().isEmpty()) {
                try {
                    CESigAndFieldOverriding.getInstance().changeOverridesFolder(overridingFolder);
                } catch (IOException e) {
                    throw new IllegalStateException("An error occurred related to the model overriding folder", e);
                }
            }
            try {
                CESigAndFieldOverriding.getInstance().loadProperties();
            } catch (IOException e) {
                throw new IllegalStateException("An error occurred while loading model overrides", e);
            }
        } else {
            CESigAndFieldOverriding.getInstance().noOverrides();
        }
    }

    public static int testsToGeneratePerCommand() {
        return (Integer) MutationConfiguration.getInstance().getConfigValue(TEST_GENERATION_MAX_TESTS_PER_COMMAND).orElse(TEST_GENERATION_MAX_TESTS_PER_COMMAND.defaultValue());
    }

    public static int testsPerGeneration() {
        return (Integer) MutationConfiguration.getInstance().getConfigValue(TEST_GENERATION_TESTS_PER_STEP).orElse(TEST_GENERATION_TESTS_PER_STEP.defaultValue());
    }

    public static boolean arepairIntegration() {
        return (Boolean) MutationConfiguration.getInstance().getConfigValue(TEST_GENERATION_AREPAIR_INTEGRATION).orElse(TEST_GENERATION_AREPAIR_INTEGRATION.defaultValue());
    }

    public static boolean arepairRelaxed() {
        return (Boolean) MutationConfiguration.getInstance().getConfigValue(TEST_GENERATION_AREPAIR_INTEGRATION_RELAXED_MODE).orElse(TEST_GENERATION_AREPAIR_INTEGRATION_RELAXED_MODE.defaultValue());
    }

    public static boolean arepairRelaxedFacts() {
        return (Boolean) MutationConfiguration.getInstance().getConfigValue(TEST_GENERATION_AREPAIR_INTEGRATION_RELAXED_FACTS).orElse(TEST_GENERATION_AREPAIR_INTEGRATION_RELAXED_FACTS.defaultValue());
    }

    public static boolean arepairForceAssertionTests() {
        return (Boolean) MutationConfiguration.getInstance().getConfigValue(TEST_GENERATION_AREPAIR_INTEGRATION_FORCE_ASSERTION_TESTS).orElse(TEST_GENERATION_AREPAIR_INTEGRATION_FORCE_ASSERTION_TESTS.defaultValue());
    }

    public static boolean arepairNoExpectInstanceForNegativeTestWhenNoFacts() {
        return (Boolean) MutationConfiguration.getInstance().getConfigValue(TEST_GENERATION_NO_EXPECT_INSTANCE_FOR_NEGATION_TEST_WHEN_NO_FACTS).orElse(TEST_GENERATION_NO_EXPECT_INSTANCE_FOR_NEGATION_TEST_WHEN_NO_FACTS.defaultValue());
    }

    public static String testBaseName() {
        return (String) MutationConfiguration.getInstance().getConfigValue(TEST_GENERATION_NAME).orElse(TEST_GENERATION_NAME.defaultValue());
    }

    public static int testBaseNameStartingIndex() {
        return (int) MutationConfiguration.getInstance().getConfigValue(TEST_GENERATION_NAME_STARTING_INDEX).orElse(TEST_GENERATION_NAME_STARTING_INDEX.defaultValue());
    }

    public static boolean useModelOverriding() {
        return (Boolean) MutationConfiguration.getInstance().getConfigValue(TEST_GENERATION_USE_MODEL_OVERRIDING).orElse(TEST_GENERATION_USE_MODEL_OVERRIDING.defaultValue());
    }

    public static String modelOverridingFolder() {
        return (String) MutationConfiguration.getInstance().getConfigValue(TEST_GENERATION_MODEL_OVERRIDING_FOLDER).orElse(TEST_GENERATION_MODEL_OVERRIDING_FOLDER.defaultValue());
    }

    public static boolean generateInstanceTests() {
        return (Boolean) MutationConfiguration.getInstance().getConfigValue(TEST_GENERATION_INSTANCES_TESTS_GENERATION).orElse(TEST_GENERATION_INSTANCES_TESTS_GENERATION.defaultValue());
    }

    public static String buggyFunctionsFile() {
        return (String) MutationConfiguration.getInstance().getConfigValue(TEST_GENERATION_INSTANCES_TESTS_GENERATION_BUGGY_FUNCS_FILE).orElse(TEST_GENERATION_INSTANCES_TESTS_GENERATION_BUGGY_FUNCS_FILE.defaultValue());
    }

    private static final String INSTANCE_TESTS_BOTH_BRANCHES = "BOTH";
    private static final String INSTANCE_TESTS_POS_BRANCHES = "POS";
    private static final String INSTANCE_TESTS_NEG_BRANCHES = "NEG";
    private boolean instanceTestsArepairIntegrationBothBranches() {
        return ((String) MutationConfiguration.getInstance().getConfigValueOrDefault(MutationConfiguration.ConfigKey.TEST_GENERATION_AREPAIR_INSTANCE_TESTS_BRANCHES)).compareTo(INSTANCE_TESTS_BOTH_BRANCHES) == 0;
    }

    private boolean instanceTestsArepairIntegrationPosBranches() {
        return ((String) MutationConfiguration.getInstance().getConfigValueOrDefault(MutationConfiguration.ConfigKey.TEST_GENERATION_AREPAIR_INSTANCE_TESTS_BRANCHES)).compareTo(INSTANCE_TESTS_POS_BRANCHES) == 0;
    }

    private boolean instanceTestsArepairIntegrationNegBranches() {
        return ((String) MutationConfiguration.getInstance().getConfigValueOrDefault(MutationConfiguration.ConfigKey.TEST_GENERATION_AREPAIR_INSTANCE_TESTS_BRANCHES)).compareTo(INSTANCE_TESTS_NEG_BRANCHES) == 0;
    }

    public List<Command> generateCEBasedTestsFor(A4Solution solution, CompModule context, Command command) throws Err {
        if (solution.getOriginalCommand().compareTo(command.toString()) != 0)
            throw new IllegalArgumentException("Command argument doesn't match the one use for the obtained solution");
        if (!command.check && command.expects != 0)
            throw new IllegalArgumentException("Command is not a check or run with expect 0");
        TestGenerationRequest request = TestGenerationRequest.createCETestRequest(solution, context, command);
        return generateTestsFor(request);
    }

    private final Set<String> observedInstances = new TreeSet<>();

    public List<Command> generateTestsFor(TestGenerationRequest request) throws Err {
        observedInstances.clear();
        Command command = request.command();
        logger.info("generate tests for\n" + command.toString());
        List<Command> generatedTests = new LinkedList<>();
        String property;
        if (command.nameExpr instanceof ExprVar)
            property = ((ExprVar) command.nameExpr).label;
        else
            property = FACTS_PROPERTY;
        int currentTests = testsPerProperty.getOrDefault(property, 0);
        int testsToGenerate = testsToGeneratePerCommand() - currentTests;
        int testsGenerated = 0;
        boolean positiveAndNegativeTestGeneration = false;
        for (int i = 0; i < testsToGenerate && i < testsPerGeneration(); i++) {
            subTestIndex = 0;
            testNamePostfix = EMPTY_POSTFIX;
            testType = request.command().check?FROM_COUNTEREXAMPLE_TYPE:FROM_PREDICATE_TYPE;
            String msg = "";
            if (!request.isInstanceTestRequest()) {
                msg = "Generating CE based test for instance\n";
            } else if (request.isInstancePositiveTestRequest()) {
                msg = "Generating positive test from " + (request.fromTrustedCommand()?"trusted":"untrusted") + " command for instance\n";
            } else if (request.isInstanceNegativeTestRequest()) {
                msg = "Generating negative test for instance\n";
            } else if (request.isInstancePositiveAndNegativeTestRequest()) {
                msg = "Generating positive and negative test from untrusted command for instance\n";
                positiveAndNegativeTestGeneration = true;
            }
            logger.info(msg + request.solution().getEvaluator().instance().relationTuples().entrySet());
            if (positiveAndNegativeTestGeneration) {
                int testIndexBackup = testIndex;
                Set<Command> positiveTests = null;
                Set<Command> negativeTests = null;
                List<TestGenerationRequest> positiveAndNegativeRequests = request.splitABothRequestIntoPositiveAndNegative();
                if (instanceTestsArepairIntegrationBothBranches() || instanceTestsArepairIntegrationPosBranches()) {
                    testNamePostfix = POS_POSTFIX;
                    positiveTests = generateNewTest(positiveAndNegativeRequests.get(0));
                    generatedTests.addAll(positiveTests);
                }
                if (instanceTestsArepairIntegrationBothBranches() || instanceTestsArepairIntegrationNegBranches()) {
                    testNamePostfix = NEG_POSTFIX;
                    testIndex = testIndexBackup;
                    subTestIndex = 0;
                    negativeTests = generateNewTest(positiveAndNegativeRequests.get(1));
                    generatedTests.addAll(negativeTests);
                }
                testsGenerated += (positiveTests == null?0:positiveTests.size()) + (negativeTests == null?0:negativeTests.size());
            } else {
                Set<Command> newTests = generateNewTest(request);
                generatedTests.addAll(newTests);
                testsGenerated += newTests.size();
            }
            testIndex++;
            observedInstances.add(request.solution().toString());
            Optional<A4Solution> next = advanceToNextInstance(request.solution());
            if (next.isPresent()) {
                request.updateSolution(next.get());
            } else {
                break;
            }
        }
        testsPerProperty.put(property, currentTests + testsGenerated);
        return generatedTests;
    }

    private Optional<A4Solution> advanceToNextInstance(A4Solution current) {
        if (current.getEvaluator() == null)
            return Optional.empty();
        A4Solution next = current.next();
        if (observedInstances.add(next.toString()))
            return Optional.of(next);
        return advanceToNextInstance(next);
    }

    public Map<String, Integer> getTestAmountPerProperty() {
        return testsPerProperty;
    }

    private Set<Command> generateNewTest(TestGenerationRequest request) {
        Set<Command> cmd;
        RepairReport.getInstance().testGenerationClockStart();
        try {
            cmd = generateNewTest_impl(request);
        } catch (Exception e) {
            RepairReport.getInstance().testGenerationClockEnd();
            throw e;
        }
        RepairReport.getInstance().testGenerationClockEnd();
        return cmd;
    }

    private Set<Command> generateNewTest_impl(TestGenerationRequest request) {
        clearVarsCache();
        A4Solution solution = request.solution();
        Command command = request.command();
        CompModule context = request.context();
        Map<Sig, List<ExprVar>> signatureValues = getSignaturesAtoms(solution, context);
        Map<Sig, List<ExprVar>> declSignatureValues = mergeExtendingSignaturesValues(signatureValues);
        Map<Field, List<Expr>> fieldValues = getFieldsValues(solution, context, signatureValues);
        Map<ExprVar, List<Expr>> variablesValues = getVariablesValues(solution, signatureValues);
        List<ExprVar> skolemVariables = new LinkedList<>(variablesValues.keySet());
        Browsable predicateOrAssertionCalled = getPredicateOrAssertionCalled(command, context);
        if (predicateOrAssertionCalled == null)
            throw new IllegalStateException("Couldn't get predicate or assertion for " + command);
        PropertyExtractor propertyExtractor = new PropertyExtractor();
        ExtractedProperty extractedProperty;
        if (predicateOrAssertionCalled instanceof Func) {
            extractedProperty = propertyExtractor.extractFromPredicate((Func) predicateOrAssertionCalled);
        } else {
            extractedProperty = propertyExtractor.extractFromAssertion((Expr) predicateOrAssertionCalled);
        }
        List<ExprVar> originalVariables = extractedProperty.getVariables();
        VariableMapping variableMapping = new VariableMapping(originalVariables, skolemVariables, command);
        List<ExprVar> solutionsSkolemVariables = new LinkedList<>();
        solution.getAllSkolems().forEach(solutionsSkolemVariables::add);
        VariableMapping internalVariableMapping = new VariableMapping(originalVariables, solutionsSkolemVariables, command);
        PropertyCleaner propertyCleaner = new PropertyCleaner();
        Expr cleanedFormula = propertyCleaner.removeSuperficialNoop(extractedProperty.getProperty());//propertyCleaner.cleanExpression(extractedProperty.getProperty());
        if (cleanedFormula == null)
            throw new IllegalStateException("Cleaned formula is null");
        if (arepairIntegration()) { //We will check that at most one predicate/function is present, unless in relaxed mode
            FunctionsCollector functionsCollector = FunctionsCollector.buggedFunctionsCollector();
            Set<Func> buggyFunctions = functionsCollector.visitThis(cleanedFormula);
            if (buggyFunctions.size() > 1 && !arepairRelaxed()) {
                String message = "ARepair integration mode can only support at most one predicate without using relaxed mode, we found " + buggyFunctions.size() + " for current expression " + cleanedFormula;
                logger.severe(message);
                throw new IllegalStateException(message);
            }
            if (buggyFunctions.stream().filter(f -> !f.isPred).count() > 1) {
                String message = "ARepair integration mode can only support predicates, we found a function (showing first) " + buggyFunctions.stream().filter(f -> f.isPred).findFirst().map(Func::toString).orElse("N/A") + " in current expression " + cleanedFormula;
                logger.severe(message);
                throw new IllegalStateException(message);
            }
        }
        VariableExchanger variableExchanger = new VariableExchanger(variableMapping);
        Expr testFormula = variableExchanger.replaceVariables((Expr)cleanedFormula.clone());
        testFormula.setCommentBefore("testFormula");
        if (!arepairIntegration()) {
            if (!request.isInstanceTestRequest()) {
                Expr facts = getFacts(context);
                if (!(facts instanceof ExprConstant)) {
                    Expr negateFacts = ExprUnary.Op.NOT.make(null, getFacts(context));
                    testFormula = ExprBinary.Op.OR.make(null, null, negateFacts, testFormula);
                    testFormula.setCommentPreviousLine("testFormulaWithFacts");
                }
            } else if (request.isInstancePositiveTestRequest()) {
                Expr facts = getFacts(context);
                if (!(facts instanceof ExprConstant)) {
                    testFormula = ExprBinary.Op.AND.make(null, null, facts, testFormula);
                    testFormula.setCommentPreviousLine("testFormulaWithFacts");
                }
            } else if (request.isInstanceNegativeTestRequest()) {
                testFormula = ExprUnary.Op.NOT.make(null, testFormula);
                Expr facts = getFacts(context);
                if (!(facts instanceof ExprConstant)) {
                    testFormula = ExprBinary.Op.AND.make(null, null, facts, testFormula);
                    testFormula.setCommentPreviousLine("testFormulaWithFacts");
                }
            }
        } else if (request.forcedExpect() == TestGenerationRequest.NEGATIVE_FORCE) {
            cleanedFormula = ExprUnary.Op.NOT.make(null, cleanedFormula);
        }
        Map<ExprVar, List<Expr>> usedVariablesValues = getUsedVariableValues(variablesValues, variableMapping);
        List<ExprVar> usedVariables = new LinkedList<>(usedVariablesValues.keySet());
        List<Expr> fieldOverrides = getFieldOverrides(solution, context, signatureValues);
        Expr initialization;
        if (arepairIntegration()) {
            FunctionsCollector functionsCollector = FunctionsCollector.buggedFunctionsCollector();
            Set<Func> buggyFunctions = functionsCollector.visitThis(cleanedFormula);
            Set<Command> testCommands = new HashSet<>();
            boolean allUntrusted = buggyFunctions.size() > 1;
            boolean fromCounterexample = !request.isInstanceTestRequest() || request.isInstanceNegativeTestRequest();
            List<TestBody> testBodies = new LinkedList<>();
            for (Func buggyFunc : buggyFunctions) {
                testBodies.addAll(fromCounterexample || (request.forcedExpect() == TestGenerationRequest.NEGATIVE_FORCE) ?
                        getBodyForARepairIntegrationCounterExample(cleanedFormula, context, solution, internalVariableMapping, buggyFunc) :
                        getBodyForARepairIntegrationInstance(cleanedFormula, context, solution, internalVariableMapping, buggyFunc)
                );
            }
            if (buggyFunctions.isEmpty()) {
                testBodies.addAll(fromCounterexample || (request.forcedExpect() == TestGenerationRequest.NEGATIVE_FORCE)?
                        getBodyForARepairIntegrationCounterExample(cleanedFormula, context, solution, internalVariableMapping, null) :
                        getBodyForARepairIntegrationInstance(cleanedFormula, context, solution, internalVariableMapping, null)
                );
            }
            for (TestBody testBody : testBodies) {
                Set<ExprVar> testUsedVariables = new TreeSet<>(Comparator.comparing(o -> o.label));
                Map<ExprVar, List<Expr>> testUsedVariablesValues = null;
                testUsedVariables.addAll(usedVariables);
                if (request.hasForcedExpect())
                    testBody.trusted = false;
                if (arepairRelaxedFacts())
                    testBody.trusted = false;
                if (!testBody.trusted || request.isInstanceTestRequest() || !testBody.expect)
                    testBody.fromInstance(true); //is a bad fix, but we don't want untrusted or expect 0 counterexample tests while in arepair integration mode
                if (request.isInstanceTestRequest() && !request.fromTrustedCommand())
                    testBody.trusted = false;
                if (testBody.body != null) {
                    if (testBody.hasVariableMapping()) {
                        VariableExchanger refinedVariableExchanger = new VariableExchanger(testBody.getRelatedVariableMapping().cleanMappingToAlloyNames());
                        testBody.body = refinedVariableExchanger.replaceVariables((Expr) testBody.body.clone());
                        testUsedVariablesValues = getUsedVariableValues(variablesValues, testBody.getRelatedVariableMapping().cleanMappingToAlloyNames());
                        testUsedVariablesValues.forEach((exprVar, exprs) -> testUsedVariables.add(exprVar));
                    } else {
                        testBody.body = variableExchanger.replaceVariables((Expr) testBody.body.clone());
                    }
                }
                if (testUsedVariablesValues == null) {
                    testUsedVariablesValues = getUsedVariableValues(variablesValues, variableMapping);
                }
                if (allUntrusted)
                    testBody.trusted = false;
                Expr disjoints = generateDisjointExpression(new LinkedList<>(testUsedVariables), signatureValues.values(), solution);
                initialization = generateInitialization(signatureValues, fieldValues, testUsedVariablesValues, fieldOverrides);
                if (disjoints != null && initialization == null) {
                    initialization = disjoints;
                } else if (disjoints != null) {
                    initialization = ExprList.makeAND(null, null, disjoints, initialization);
                }
                Command testCommand = generateTest(initialization, testBody, new LinkedList<>(testUsedVariables), declSignatureValues, command, context);
                testCommands.add(testCommand);
            }
            return testCommands;
        } else {
            Expr disjoints = generateDisjointExpression(usedVariables, signatureValues.values(), solution);
            initialization = generateInitialization(signatureValues, fieldValues, usedVariablesValues, fieldOverrides);
            if (disjoints != null && initialization == null) {
                initialization = disjoints;
            } else if (disjoints != null) {
                initialization = ExprList.makeAND(null, null, disjoints, initialization);
            }
            TestBody testBody;
            if (!request.isInstanceTestRequest()) {
                testBody = TestBody.trustedSatisfiablePredicate(testFormula, null);
                testBody.fromInstance(false);
            } else if (request.isInstancePositiveTestRequest()) {
                testBody = request.fromTrustedCommand()?TestBody.trustedSatisfiablePredicate(testFormula, null):TestBody.untrustedSatisfiablePredicate(testFormula, null);
            } else if (request.isInstanceNegativeTestRequest()) {
                testBody = request.fromTrustedCommand()?TestBody.trustedUnsatisfiablePredicate(testFormula, null):TestBody.untrustedUnsatisfiablePredicate(testFormula, null);
            } else {
                throw new IllegalStateException("Unexpected request: " + request);
            }
            Command testCommand = generateTest(initialization, testBody, usedVariables, declSignatureValues, command, context);
            return Collections.singleton(testCommand);
        }
    }


    private Expr generateDisjointExpression(List<ExprVar> skolemVars, Collection<List<ExprVar>> signatureVars, A4Solution solution) {
        Expr disjoint = null;
        List<Expr> skolemDisjoints = generateDisjointExpressions(skolemVars, solution, true);
        if (!skolemDisjoints.isEmpty())
            disjoint = ExprList.make(null, null, ExprList.Op.AND, skolemDisjoints);
        Expr signatureVarsDisjointExpressionAll = null;
        for (List<ExprVar> sigVars : signatureVars) {
            List<Expr> signatureAtomsDisjoints = generateDisjointExpressions(sigVars, solution, false);
            if (!signatureAtomsDisjoints.isEmpty()) {
                Expr signatureVarsDisjointExpression = ExprList.make(null, null, ExprList.Op.AND, signatureAtomsDisjoints);
                if (signatureVarsDisjointExpressionAll == null)
                    signatureVarsDisjointExpressionAll = signatureVarsDisjointExpression;
                else
                    signatureVarsDisjointExpressionAll = ExprBinary.Op.AND.make(null, null, signatureVarsDisjointExpressionAll, signatureVarsDisjointExpression);
            }
        }
        if (signatureVarsDisjointExpressionAll != null && disjoint == null)
            disjoint = signatureVarsDisjointExpressionAll;
        else if (signatureVarsDisjointExpressionAll != null)
            disjoint = ExprBinary.Op.AND.make(null, null, disjoint, signatureVarsDisjointExpressionAll);
        return disjoint;
    }

    private Command generateTest(Expr initialization, TestBody testBody, List<ExprVar> usedVariables, Map<Sig, List<ExprVar>> declSignatureValues, Command command, CompModule context) {
        Func testPredicate = generateTestPredicate(initialization, testBody.body(), usedVariables, declSignatureValues, command, (testBody.isRelated()?testBody.relatedTo():null));
        testPredicate.setCommentPreviousLine("TEST START");
        testPredicate.setCommentNextLine("TEST FINISH");
        Command testCommand = generateTestCommand(testPredicate, !testBody.fromInstance(), testBody.isExpected(), testBody.isTrusted());
        try {
            Cheats.copyScopes(command, testCommand);
        } catch (CheatingIsBadMkay e) {
            throw new Error("An error occurred while copying original command scopes to test command", e);
        }
        logger.info("Test generated\n" +
                testPredicate + "\n" +
                testPredicate.getBody().toString()
        );
        try {
            Cheats.addFunctionToModule(context, testPredicate);
        } catch (CheatingIsBadMkay e) {
            throw new Error("An error occurred while adding counter example predicate to ast", e);
        }
        try {
            Cheats.addCommand(testCommand, context);
        } catch (CheatingIsBadMkay e) {
            throw new Error("An error occurred while adding counter example command to ast", e);
        }
        return testCommand;
    }

    private Command generateTestCommand(Func testPredicate, boolean counterexample, boolean positiveTest, boolean trusted) {
        ExprVar predName = ExprVar.make(null, testPredicate.label);
        predName.setReferenced(testPredicate);
        Expr formula = testPredicate.getBody();
        Command testCommand = new Command(null, predName, testPredicate.label, false, -1, -1, -1, positiveTest?1:0, null, null, formula, null);
        testCommand.setAsVariabilizationTest();
        testCommand.setAsGenerated();
        if (counterexample)
            testCommand.testType(Command.TestType.CE);
        else if (trusted)
            testCommand.testType(Command.TestType.TRUSTED);
        else
            testCommand.testType(Command.TestType.UNTRUSTED);
        return testCommand;
    }

    private Func generateTestPredicate(Expr initialization, Expr testFormula, List<ExprVar> skolemVariables, Map<Sig, List<ExprVar>> signatureValues, Command cmd, String relatedTo) {
        Expr sub;// = testFormula!=null?ExprBinary.Op.AND.make(null, null, initialization, testFormula):initialization;
        if (testFormula != null) {
            List<Expr> expressions = new LinkedList<>();
            expressions.add(initialization);
            expressions.add(testFormula);
            sub = ExprList.make(null, null, ExprList.Op.AND, ConstList.make(expressions));
        } else {
            sub = initialization;
        }
        List<Decl> signatureDecls = getVariablesDecls(signatureValues);
        List<Decl> skolemDecls = getVariablesDecls(skolemVariables);
        Expr body;
        if (!skolemDecls.isEmpty()) {
            body = generateSome(skolemDecls, sub);
            body = generateSome(signatureDecls, body);
        } else {
            body = generateSome(signatureDecls, sub);
        }
        String from = cmd.nameExpr instanceof ExprVar?((ExprVar) cmd.nameExpr).label:"NO_NAME";
        String name = getTestName(from);
        if (relatedTo != null)
             name += "_relTo-" + relatedTo;
        Func testPredicate = new Func(null, name, null, null, body);
        testPredicate.setGenerated();
        return testPredicate;
    }

    private Map<ExprVar, List<Expr>> getUsedVariableValues(Map<ExprVar, List<Expr>> variablesValues, VariableMapping variableMapping) {
        Map<ExprVar, List<Expr>>  usedVariablesValues = new HashMap<>();
        for (Entry<ExprVar, List<Expr>> vValues : variablesValues.entrySet()) {
            if (variableMapping.isSkolemUsed(vValues.getKey())) {
                usedVariablesValues.put(vValues.getKey(), vValues.getValue());
            }
        }
        return usedVariablesValues;
    }

    private static class TestBody {
        private Expr body;
        private final boolean expect;
        private boolean trusted;
        private String relatedTo;
        private final VariableMapping relatedVariableMapping;

        private boolean fromInstance = false;

        public static TestBody trustedUnexpectedInstance() {
            return new TestBody(null, false, true, null);
        }

        public static TestBody untrustedUnexpectedInstance() {
            return new TestBody(null, false, false, null);
        }

        public static TestBody trustedExpectedInstance() { return new TestBody(null, true, true, null); }

        public static TestBody untrustedExpectedInstance() { return new TestBody(null, true, false, null); }

        public static TestBody trustedSatisfiablePredicate(Expr pred, VariableMapping relatedVariableMapping) {
            return new TestBody(pred, true, true, relatedVariableMapping);
        }

        public static TestBody untrustedSatisfiablePredicate(Expr pred, VariableMapping relatedVariableMapping) {
            return new TestBody(pred, true, false, relatedVariableMapping);
        }

        public static TestBody trustedUnsatisfiablePredicate(Expr pred, VariableMapping relatedVariableMapping) {
            return new TestBody(pred, false, true, relatedVariableMapping);
        }

        public static TestBody untrustedUnsatisfiablePredicate(Expr pred, VariableMapping relatedVariableMapping) {
            return new TestBody(pred, false, false, relatedVariableMapping);
        }

        private TestBody(Expr body, boolean expect, boolean trusted, VariableMapping relatedVariableMapping) {
            this.body = body;
            this.expect = expect;
            this.trusted = trusted;
            if (relatedVariableMapping != null && relatedVariableMapping.isExtended())
                this.relatedVariableMapping = relatedVariableMapping;
            else
                this.relatedVariableMapping = null;
        }

        public void fromInstance(boolean fromInstance) {
            this.fromInstance = fromInstance;
        }

        public boolean fromInstance() {
            return fromInstance;
        }

        public Expr body() {
            return body;
        }

        public boolean isExpected() {
            return expect;
        }

        public boolean isTrusted() {
            return trusted;
        }

        public void relatedTo(String relatedTo) {
            this.relatedTo = relatedTo;
        }

        public String relatedTo() {
            return relatedTo;
        }

        public boolean isRelated() {
            return relatedTo != null && !relatedTo.isEmpty();
        }

        public boolean hasVariableMapping() { return relatedVariableMapping != null; }

        public VariableMapping getRelatedVariableMapping() { return relatedVariableMapping; }

    }

    private Optional<Expr> andOrListToBinaryExpression(ExprList list) {
        if (list.op.equals(ExprList.Op.AND) || list.op.equals(ExprList.Op.OR)) {
            if (list.args.size() == 2) {
                ExprBinary.Op op = list.op.equals(ExprList.Op.AND)?ExprBinary.Op.AND:ExprBinary.Op.OR;
                return Optional.of(op.make(list.pos, null, (Expr) list.args.get(0).clone(), (Expr) list.args.get(1).clone()));
            }
        }
        return Optional.empty();
    }

    private List<TestBody> getBodyForARepairIntegrationCounterExample(Expr originalBody, CompModule context, A4Solution instance, VariableMapping internalVariableMapping, Func buggyFunc) {
        if (originalBody == null)
            throw new IllegalArgumentException("initial expression is null");
        if (context == null)
            throw new IllegalArgumentException("context is null");
        if (instance == null)
            throw new IllegalArgumentException("instance is null");
        if (internalVariableMapping == null)
            throw new IllegalArgumentException("variable mapping is null");
        VariableMapping variableMapping = internalVariableMapping;
        boolean hasFacts = hasFacts(context);
        if (originalBody instanceof ExprUnary && ((ExprUnary)originalBody).op.equals(ExprUnary.Op.NOOP))
            return getBodyForARepairIntegrationCounterExample(((ExprUnary)originalBody).sub, context, instance, variableMapping, buggyFunc);
        if (buggyFunc == null)
            return Collections.singletonList(TestBody.trustedUnexpectedInstance());
        Boolean expected = null;
        boolean invert;
        boolean lor; //false: left, true: right
        Expr current = originalBody;
        if (current instanceof ExprList) {
            current = andOrListToBinaryExpression((ExprList) current).orElse(current);
        }
        SearchCall searchExpr = new SearchCall(null);
        boolean untrustedFacts = (hasFacts && MutantLab.getInstance().isAnyFactAffected());
        while (true) {
            invert = false;
            if (current instanceof ExprBinary) {
                ExprBinary currentAsBinaryExpr = (ExprBinary) current;
                Expr formulaToEvaluate;
                searchExpr.setTarget(buggyFunc);
                if (searchExpr.visitThis(currentAsBinaryExpr.left)) {
                    formulaToEvaluate = currentAsBinaryExpr.right;
                    lor = false;
                } else if (searchExpr.visitThis(currentAsBinaryExpr.right)) {
                    formulaToEvaluate = currentAsBinaryExpr.left;
                    lor = true;
                } else {
                    logger.warning("Predicate exists but can't be located in the expression (pred: " + buggyFunc + ") (expr: " + current + ")");
                    return Collections.emptyList();
                }
                Expr predsFormula = lor?currentAsBinaryExpr.right:currentAsBinaryExpr.left;
                if (formulaToEvaluate.type().is_int() || formulaToEvaluate.type().is_small_int()) {
                    if (needToIterate(predsFormula)) {
                        logger.warning("Can't support complex expressions for preds related formula when dealing with int expressions");
                        return Collections.emptyList();
                    }
                    if (isArithmeticComparisonBinaryOp(currentAsBinaryExpr.op)) {
                        Optional<ExprConstant> constantValue = ExpressionEvaluator.evaluateIntFormula(instance, formulaToEvaluate, variableMapping);
                        if (constantValue.isPresent()) {
                            Expr expressionToTest = currentAsBinaryExpr.op.make(null, null, (Expr) predsFormula.clone(), constantValue.get());
                            if (expressionToTest.errors != null && !expressionToTest.errors.isEmpty()) {
                                logger.warning("An error occurred while trying to generate the arithmetic expression to test:\n" + expressionToTest.errors.stream().map(Err::toString).collect(Collectors.joining("\n")));
                                return Collections.emptyList();
                            }
                            List<TestBody> testBodies = new LinkedList<>();
                            if (expected == null || expected) {
                                testBodies.add(untrustedFacts?TestBody.untrustedSatisfiablePredicate(expressionToTest, variableMapping):TestBody.trustedSatisfiablePredicate(expressionToTest, variableMapping));
                            } else  {
                                testBodies.addAll(generateUnsatPredicateTestBodies(expressionToTest, !untrustedFacts, hasFacts, variableMapping));
                            }
                            if (untrustedFacts) {
                                testBodies.add(TestBody.untrustedUnexpectedInstance());
                            }
                            return testBodies;
                        } else {
                            logger.warning("Evaluation of " + formulaToEvaluate + " didn't yield any integer constant value");
                            return Collections.emptyList();
                        }
                    } else {
                        logger.warning("Can't support other arithmetic operators, only equality ones; current operator is (" + currentAsBinaryExpr.op + ") in " + currentAsBinaryExpr);
                        return Collections.emptyList();
                    }
                }
                Optional<Boolean> formulaEvaluation = ExpressionEvaluator.evaluateFormula(instance, formulaToEvaluate, variableMapping);
                if (!formulaEvaluation.isPresent()) {
                    logger.warning("Failed to evaluate " + formulaToEvaluate + " to a boolean value");
                    return Collections.emptyList();
                }
                switch (currentAsBinaryExpr.op) {
                    case NOT_EQUALS: invert = true;
                    case IFF:
                    case EQUALS: {
                        if (expected == null)
                            expected = Boolean.TRUE;
                        if (invert)
                            expected = !expected;
                        //expected is the same as saying if the operands must be equals or not
                        if (needToIterate(predsFormula)) {
                            current = predsFormula;
                        } else {
                            if (expected && !formulaEvaluation.get()) { //must be equals, and formula is false, pred should be false or instance must not exist
                                List<TestBody> testBodies = generateUnsatPredicateTestBodies(predsFormula, !untrustedFacts, hasFacts, variableMapping);
                                if (untrustedFacts)
                                    testBodies.add(TestBody.untrustedUnexpectedInstance());
                                return testBodies;
                            } else if (expected) { //must be equals, and formula is true, pred should be true, or instance must not exist
                                if (untrustedFacts) {
                                    return Arrays.asList(
                                            TestBody.untrustedSatisfiablePredicate(predsFormula, variableMapping),
                                            TestBody.untrustedUnexpectedInstance()
                                    );
                                } else {
                                    return Collections.singletonList(
                                            TestBody.trustedSatisfiablePredicate(predsFormula, variableMapping)
                                    );
                                }
                            } else if (!formulaEvaluation.get()) { //must be different, and formula is false, pred should be true or instance must not exist
                                if (untrustedFacts) {
                                    return Arrays.asList(
                                            TestBody.untrustedSatisfiablePredicate(predsFormula, variableMapping),
                                            TestBody.untrustedUnexpectedInstance()
                                    );
                                } else {
                                    return Collections.singletonList(
                                            TestBody.trustedSatisfiablePredicate(predsFormula, variableMapping)
                                    );
                                }
                            } else { //must be different, and formula is true, pred should be false or instance must not exist
                                List<TestBody> testBodies = generateUnsatPredicateTestBodies(predsFormula, !untrustedFacts, hasFacts, variableMapping);
                                if (untrustedFacts)
                                    testBodies.add(TestBody.untrustedUnexpectedInstance());
                                return testBodies;
                            }
                        }
                        break;
                    }
                    case IMPLIES: {
                        if (expected == null)
                            expected = Boolean.TRUE;
                        boolean impliesLor = !lor;
                        if ((expected && !impliesLor) || (!expected && impliesLor && !formulaEvaluation.get())) {
                            if (needToIterate(predsFormula)) {
                                expected = true;
                                current = predsFormula;
                                continue;
                            }
                            if (untrustedFacts) {
                                return Arrays.asList(TestBody.untrustedSatisfiablePredicate(predsFormula, variableMapping), TestBody.untrustedUnexpectedInstance());
                            } else {
                                return Collections.singletonList(TestBody.trustedSatisfiablePredicate(predsFormula, variableMapping));
                            }
                        } else if (expected || !impliesLor && formulaEvaluation.get()) {
                            if (needToIterate(predsFormula)) {
                                expected = false;
                                current = predsFormula;
                                continue;
                            }
                            return generateUnsatPredicateTestBodies(predsFormula, true, hasFacts, variableMapping);
                        } else
                          return Collections.singletonList(TestBody.trustedUnexpectedInstance());
                    }
                    case AND: {
                        if (expected == null)
                            expected = Boolean.TRUE;
                        if (needToIterate(predsFormula)) {
                            if (!formulaEvaluation.get() && expected)
                                return Collections.singletonList(TestBody.trustedUnexpectedInstance());
                            else
                                expected = formulaEvaluation.get() && expected;
                            current = predsFormula;
                            continue;
                        }
                        if (expected) {
                            if (untrustedFacts) {
                                return Arrays.asList(TestBody.untrustedSatisfiablePredicate(predsFormula, variableMapping), TestBody.untrustedUnexpectedInstance());
                            } else {
                                return Collections.singletonList(TestBody.trustedSatisfiablePredicate(predsFormula, variableMapping));
                            }
                        } else {
                            if (!formulaEvaluation.get())
                                return Collections.singletonList(TestBody.trustedUnexpectedInstance());
                            else {
                                return generateUnsatPredicateTestBodies(predsFormula, true, hasFacts, variableMapping);
                            }
                        }
                    }
                    case OR: {
                        if (expected == null)
                            expected = Boolean.TRUE;
                        if (needToIterate(predsFormula)) { //expected value doesn't change
                            current = predsFormula;
                            continue;
                        }
                        if (expected) {
                            return untrustedFacts?
                                    Arrays.asList(
                                            TestBody.untrustedSatisfiablePredicate(predsFormula, variableMapping),
                                            TestBody.untrustedUnexpectedInstance()
                                    ):
                                    Collections.singletonList(TestBody.trustedSatisfiablePredicate(predsFormula, variableMapping));
                        } else if (formulaEvaluation.get())
                            return Collections.singletonList(TestBody.trustedUnexpectedInstance());
                        else {
                            return generateUnsatPredicateTestBodies(predsFormula, true, hasFacts, variableMapping);
                        }
                    }
                    default: {
                        logger.warning("Unsupported binary operator (" + currentAsBinaryExpr.op + ") in expression: " + currentAsBinaryExpr);
                        return Collections.emptyList();
                    }
                }
            } else if (current instanceof ExprUnary) {
                ExprUnary currentAsExprUnary = (ExprUnary) current;
                if (currentAsExprUnary.op.equals(ExprUnary.Op.NOOP)) {
                    current = currentAsExprUnary.sub;
                    continue;
                }
                if (currentAsExprUnary.op.equals(ExprUnary.Op.NOT)) {
                    if (expected == null)
                        expected = Boolean.TRUE;
                    if (needToIterate(currentAsExprUnary.sub)) {
                        expected = !expected;
                        current = currentAsExprUnary.sub;
                        continue;
                    }
                    if (expected) {
                        return generateUnsatPredicateTestBodies(currentAsExprUnary.sub, !untrustedFacts, hasFacts, variableMapping);
                    } else {
                        return untrustedFacts?
                                Arrays.asList(
                                        TestBody.untrustedSatisfiablePredicate(currentAsExprUnary.sub, variableMapping),
                                        TestBody.untrustedUnexpectedInstance()
                                ):
                                Collections.singletonList(TestBody.trustedSatisfiablePredicate(currentAsExprUnary.sub, variableMapping));
                    }
                } else {
                    logger.warning("Unsupported unary operator (" + currentAsExprUnary.op + ") in expression: " + currentAsExprUnary);
                    return Collections.emptyList();
                }
            } else if (current instanceof ExprCall) {
                if (expected == null)
                    expected = Boolean.TRUE;
                if (expected && hasFacts) {
                    return Arrays.asList(TestBody.untrustedSatisfiablePredicate(current, variableMapping), TestBody.untrustedUnexpectedInstance());
                }
                if (expected) {
                    return Collections.singletonList(TestBody.trustedUnexpectedInstance());
                }
                if (untrustedFacts) {
                    List<TestBody> testBodies = generateUnsatPredicateTestBodies(current, false, true, variableMapping);
                    testBodies.add(TestBody.untrustedUnexpectedInstance());
                    return testBodies;
                }
                return Collections.singletonList(TestBody.trustedUnexpectedInstance());
            } else if (current instanceof ExprConstant) {
                return Collections.singletonList(TestBody.trustedUnexpectedInstance());
            } else if (current instanceof ExprList) {
                //NOT YET
                logger.warning("ExprList not yet supported (" + current + ")");
                return Collections.emptyList();
            } else if (current instanceof ExprQt) {
                PropertyExtractor qtExtractor = new PropertyExtractor();
                ExtractedProperty qtExtracted;
                try {
                    qtExtracted = qtExtractor.extractFromQt((ExprQt) current);
                    VariableMapping extendedMapping = VariableMapping.extendPreviousMapping(variableMapping, qtExtracted.getVariables());
                    current = qtExtracted.getProperty();
                    variableMapping = extendedMapping;
                    //continue; only add if anything is added to the following expression types
                } catch (Exception e) {
                    StringWriter sw = new StringWriter();
                    e.printStackTrace(new PrintWriter(sw));
                    String exceptionAsString = sw.toString();
                    logger.warning("Couldn't reduce quantifier expression\n" + exceptionAsString);
                    return Collections.emptyList();
                }
            } else if (current instanceof ExprLet) {
                //NOT YET
                logger.warning("ExprLet not yet supported (" + current + ")");
                return Collections.emptyList();
            } else {
                //??? SOMETHING
                logger.warning("Expr not yet supported (" + current.toString() + ")");
                return Collections.emptyList();
            }
        }
    }

    private List<TestBody> getBodyForARepairIntegrationInstance(Expr originalBody, CompModule context, A4Solution instance, VariableMapping internalVariableMapping, Func buggyFunc) {
        if (originalBody == null)
            throw new IllegalArgumentException("initial expression is null");
        if (context == null)
            throw new IllegalArgumentException("context is null");
        if (instance == null)
            throw new IllegalArgumentException("instance is null");
        if (internalVariableMapping == null)
            throw new IllegalArgumentException("variable mapping is null");
        VariableMapping variableMapping = internalVariableMapping;
        boolean hasFacts = hasFacts(context);
        if (originalBody instanceof ExprUnary && ((ExprUnary)originalBody).op.equals(ExprUnary.Op.NOOP))
            return getBodyForARepairIntegrationInstance(((ExprUnary)originalBody).sub, context, instance, variableMapping, buggyFunc);
        if (buggyFunc == null)
            return Collections.singletonList(TestBody.trustedSatisfiablePredicate(originalBody, variableMapping));//TestBody.trustedExpectedInstance());
        Boolean expected = null;
        boolean lor; //false: left, true: right
        Expr current = originalBody;
        if (current instanceof ExprList) {
            current = andOrListToBinaryExpression((ExprList) current).orElse(current);
        }
        SearchCall searchExpr = new SearchCall(null);
        boolean untrustedFacts = (hasFacts && MutantLab.getInstance().isAnyFactAffected());
        while (true) {
            if (current instanceof ExprBinary) {
                ExprBinary currentAsBinaryExpr = (ExprBinary) current;
                Expr formulaToEvaluate;
                searchExpr.setTarget(buggyFunc);
                if (searchExpr.visitThis(currentAsBinaryExpr.left)) {
                    formulaToEvaluate = currentAsBinaryExpr.right;
                    lor = false;
                } else if (searchExpr.visitThis(currentAsBinaryExpr.right)) {
                    formulaToEvaluate = currentAsBinaryExpr.left;
                    lor = true;
                } else {
                    logger.warning("Predicate exists but can't be located in the expression (pred: " + buggyFunc + ") (expr: " + current + ")");
                    return Collections.emptyList();
                }
                Expr predsFormula = lor?currentAsBinaryExpr.right:currentAsBinaryExpr.left;
                if (formulaToEvaluate.type().is_int() || formulaToEvaluate.type().is_small_int()) {
                    if (needToIterate(predsFormula)) {
                        logger.warning("Can't support complex expressions for preds related formula when dealing with int expressions");
                        return Collections.emptyList();
                    }
                    if (isArithmeticComparisonBinaryOp(currentAsBinaryExpr.op)) {
                        Optional<ExprConstant> constantValue = ExpressionEvaluator.evaluateIntFormula(instance, formulaToEvaluate, variableMapping);
                        if (constantValue.isPresent()) {
                            Expr expressionToTest = currentAsBinaryExpr.op.make(null, null, (Expr) predsFormula.clone(), constantValue.get());
                            if (expressionToTest.errors != null && !expressionToTest.errors.isEmpty()) {
                                logger.warning("An error occurred while trying to generate the arithmetic expression to test:\n" + expressionToTest.errors.stream().map(Err::toString).collect(Collectors.joining("\n")));
                                return Collections.emptyList();
                            }
                            List<TestBody> testBodies = new LinkedList<>();
                            if (expected == null || expected) {
                                testBodies.add(untrustedFacts?TestBody.untrustedSatisfiablePredicate(expressionToTest, variableMapping):TestBody.trustedSatisfiablePredicate(expressionToTest, variableMapping));
                            } else  {
                                testBodies.addAll(generateUnsatPredicateTestBodies(expressionToTest, !untrustedFacts, hasFacts, variableMapping));
                            }
                            return testBodies;
                        } else {
                            logger.warning("Evaluation of " + formulaToEvaluate + " didn't yield any integer constant value");
                            return Collections.emptyList();
                        }
                    } else {
                        logger.warning("Can't support other arithmetic operators, only equality ones; current operator is (" + currentAsBinaryExpr.op + ") in " + currentAsBinaryExpr);
                        return Collections.emptyList();
                    }
                }
                Optional<Boolean> formulaEvaluation = ExpressionEvaluator.evaluateFormula(instance, formulaToEvaluate, variableMapping);
                if (!formulaEvaluation.isPresent()) {
                    logger.warning("Failed to evaluate " + formulaToEvaluate + " to a boolean value");
                    return Collections.emptyList();
                }
                switch (currentAsBinaryExpr.op) {
                    case NOT_EQUALS: {
                        expected = !formulaEvaluation.get();
                    }
                    case IFF:
                    case EQUALS: {
                        if (expected == null)
                            expected = formulaEvaluation.get();
                        if (needToIterate(predsFormula)) {
                            current = predsFormula;
                            continue;
                        } else {
                            List<TestBody> testBodies = new LinkedList<>();
                            if (untrustedFacts) {
                                if (expected)
                                    testBodies.add(TestBody.untrustedSatisfiablePredicate(predsFormula, variableMapping));
                                else {
                                    testBodies.addAll(generateUnsatPredicateTestBodies(predsFormula, false, true, variableMapping));
                                }
                            } else {
                                if (expected) {
                                    testBodies.add(TestBody.trustedSatisfiablePredicate(predsFormula, variableMapping));
                                } else {
                                    testBodies.addAll(generateUnsatPredicateTestBodies(predsFormula, true, hasFacts, variableMapping));
                                }
                            }
                            return testBodies;
                        }
                    }
                    case IMPLIES: {
                        if (expected == null)
                            expected = Boolean.TRUE;
                        boolean precedentIsTrue;
                        if (lor) {
                            Optional<Boolean> predEvaluation = ExpressionEvaluator.evaluateFormula(instance, predsFormula, variableMapping);
                            if (!predEvaluation.isPresent()) {
                                logger.warning("Failed to evaluate " + predsFormula + " to a boolean value");
                                return Collections.emptyList();
                            }
                            precedentIsTrue = predEvaluation.get();
                        } else {
                            precedentIsTrue = formulaEvaluation.get();
                        }
                        if (expected && !lor && !formulaEvaluation.get()) {
                            return Collections.singletonList(TestBody.trustedExpectedInstance());
                        }
                        if (needToIterate(predsFormula)) {
                            if (expected && lor) {
                                expected = precedentIsTrue;
                            } else if (!expected && !lor) {
                                expected = false;
                            }
                            current = predsFormula;
                            continue;
                        }
                        if (expected) {
                            if (precedentIsTrue) {
                                if (untrustedFacts) {
                                    return Collections.singletonList(TestBody.untrustedSatisfiablePredicate(predsFormula, variableMapping));
                                } else {
                                    return Collections.singletonList(TestBody.trustedSatisfiablePredicate(predsFormula, variableMapping));
                                }
                            } else {
                                if (untrustedFacts) {
                                    return generateUnsatPredicateTestBodies(predsFormula, false, true, variableMapping);
                                } else {
                                    return Collections.singletonList(TestBody.trustedSatisfiablePredicate(predsFormula, variableMapping));
                                }
                            }
                        } else {
                            List<TestBody> testBodies = new LinkedList<>();
                            if (untrustedFacts) {
                                if (lor)
                                    testBodies.add(TestBody.untrustedSatisfiablePredicate(predsFormula, variableMapping));
                                else {
                                    testBodies.addAll(generateUnsatPredicateTestBodies(predsFormula, false, true, variableMapping));
                                }
                                testBodies.add(TestBody.untrustedUnexpectedInstance());
                            } else {
                                if (lor)
                                    return Collections.singletonList(TestBody.trustedSatisfiablePredicate(predsFormula, variableMapping));
                                else {
                                    return generateUnsatPredicateTestBodies(predsFormula, true, hasFacts, variableMapping);
                                }
                            }
                            return testBodies;
                        }
                    }
                    case AND: {
                        if (expected == null)
                            expected = Boolean.TRUE;
                        if (!expected && !formulaEvaluation.get())
                            return Collections.singletonList(TestBody.trustedExpectedInstance());
                        if (needToIterate(predsFormula)) {
                            current = predsFormula;
                            continue;
                        }
                        if (expected) {
                            if (untrustedFacts) {
                                return Collections.singletonList(TestBody.untrustedSatisfiablePredicate(predsFormula, variableMapping));
                            } else {
                                return Collections.singletonList(TestBody.trustedSatisfiablePredicate(predsFormula, variableMapping));
                            }
                        } else if (untrustedFacts) {
                            return generateUnsatPredicateTestBodies(predsFormula, false, true, variableMapping);
                        } else {
                            return generateUnsatPredicateTestBodies(predsFormula, true, hasFacts, variableMapping);
                        }
                    }
                    case OR: {
                        if (expected == null)
                            expected = Boolean.TRUE;
                        if (expected && formulaEvaluation.get()) {
                            return Collections.singletonList(TestBody.trustedExpectedInstance());
                        }
                        if (needToIterate(predsFormula)) { //expected value doesn't change
                            current = predsFormula;
                            continue;
                        }
                        List<TestBody> testBodies = new LinkedList<>();
                        if (untrustedFacts) {
                            if (expected)
                                testBodies.add(TestBody.untrustedSatisfiablePredicate(predsFormula, variableMapping));
                            else {
                                testBodies.addAll(generateUnsatPredicateTestBodies(predsFormula, false, true, variableMapping));
                            }
                            testBodies.add(TestBody.untrustedUnexpectedInstance());
                        } else {
                            if (expected)
                                testBodies.add(TestBody.trustedSatisfiablePredicate(predsFormula, variableMapping));
                            else {
                                testBodies.addAll(generateUnsatPredicateTestBodies(predsFormula, true, hasFacts, variableMapping));
                            }
                        }
                        return testBodies;
                    }
                    default: {
                        logger.warning("Unsupported binary operator (" + currentAsBinaryExpr.op + ") in expression: " + currentAsBinaryExpr);
                        return Collections.emptyList();
                    }
                }
            } else if (current instanceof ExprUnary) {
                ExprUnary currentAsExprUnary = (ExprUnary) current;
                if (currentAsExprUnary.op.equals(ExprUnary.Op.NOOP)) {
                    current = currentAsExprUnary.sub;
                    continue;
                }
                if (currentAsExprUnary.op.equals(ExprUnary.Op.NOT)) {
                    if (expected == null)
                        expected = Boolean.TRUE;
                    if (needToIterate(currentAsExprUnary.sub)) {
                        expected = !expected;
                        current = currentAsExprUnary.sub;
                        continue;
                    }
                    if (expected) {
                        return generateUnsatPredicateTestBodies(currentAsExprUnary.sub, !untrustedFacts, hasFacts, variableMapping);
                    } else {
                        return untrustedFacts?
                                Collections.singletonList(TestBody.untrustedSatisfiablePredicate(currentAsExprUnary.sub, variableMapping)):
                                Collections.singletonList(TestBody.trustedSatisfiablePredicate(currentAsExprUnary.sub, variableMapping));
                    }
                } else {
                    logger.warning("Unsupported unary operator (" + currentAsExprUnary.op + ") in expression: " + currentAsExprUnary);
                    return Collections.emptyList();
                }
            } else if (current instanceof ExprCall) {
                if (expected == null)
                    expected = Boolean.TRUE;
                if (expected) {
                    return untrustedFacts?
                            Arrays.asList(TestBody.untrustedSatisfiablePredicate(current, variableMapping), TestBody.untrustedUnexpectedInstance()):
                            Collections.singletonList(TestBody.trustedSatisfiablePredicate(current, variableMapping));
                } else {
                    return generateUnsatPredicateTestBodies(current, !untrustedFacts, hasFacts, variableMapping);
                }
            } else if (current instanceof ExprConstant) {
                return Collections.singletonList(TestBody.trustedExpectedInstance());
            } else if (current instanceof ExprList) {
                //NOT YET
                logger.warning("ExprList not yet supported (" + current + ")");
                return Collections.emptyList();
            } else if (current instanceof ExprQt) {
                PropertyExtractor qtExtractor = new PropertyExtractor();
                ExtractedProperty qtExtracted;
                try {
                    qtExtracted = qtExtractor.extractFromQt((ExprQt) current);
                    VariableMapping extendedMapping = VariableMapping.extendPreviousMapping(variableMapping, qtExtracted.getVariables());
                    current = qtExtracted.getProperty();
                    variableMapping = extendedMapping;
                    //continue; only add if anything is added to the following expression types
                } catch (Exception e) {
                    StringWriter sw = new StringWriter();
                    e.printStackTrace(new PrintWriter(sw));
                    String exceptionAsString = sw.toString();
                    logger.warning("Couldn't reduce quantifier expression\n" + exceptionAsString);
                    return Collections.emptyList();
                }
            } else if (current instanceof ExprLet) {
                //NOT YET
                logger.warning("ExprLet not yet supported (" + current + ")");
                return Collections.emptyList();
            } else {
                //??? SOMETHING
                logger.warning("Expr not yet supported (" + current.toString() + ")");
                return Collections.emptyList();
            }
        }
    }

    private List<TestBody> generateUnsatPredicateTestBodies(Expr predsFormula, boolean trusted, boolean hasFacts, VariableMapping variableMapping) {
        List<TestBody> testBodies = new LinkedList<>();
        String relationName = generateRandomName();
        TestBody unsatisfiablePredicate = trusted?TestBody.trustedUnsatisfiablePredicate(predsFormula, variableMapping):TestBody.untrustedUnsatisfiablePredicate(predsFormula, variableMapping);
        if (hasFacts || !arepairNoExpectInstanceForNegativeTestWhenNoFacts()) {
            TestBody expectedInstance = trusted?TestBody.trustedExpectedInstance():TestBody.untrustedExpectedInstance();
            unsatisfiablePredicate.relatedTo(relationName);
            expectedInstance.relatedTo(relationName);
            testBodies.add(expectedInstance);
        }
        testBodies.add(unsatisfiablePredicate);
        return testBodies;
    }

    private boolean isArithmeticComparisonBinaryOp(ExprBinary.Op op) {
        switch (op) {
            case EQUALS:
            case NOT_EQUALS:
            case LT:
            case LTE:
            case GT:
            case GTE:
            case NOT_LT:
            case NOT_LTE:
            case NOT_GT:
            case NOT_GTE: return true;
            default: return false;
        }
    }

    private boolean needToIterate(Expr x) {
        return !(x instanceof ExprCall);
    }

    private String getTestName(String from) {
        if (this.testName == null) {
            return "CE_" + from + "_" + generateRandomName();
        }
        return this.testName + (testType.isEmpty()?"":("_" + testType + "_")) + (this.testIndex) + "_" + (this.subTestIndex++) + (testNamePostfix.isEmpty()?"":("_" + testNamePostfix));
    }

    private List<Decl> getVariablesDecls(Map<Sig, List<ExprVar>> varsPerSignature) {
        List<Decl> decls = new LinkedList<>();
        for (Entry<Sig, List<ExprVar>> sigVars : varsPerSignature.entrySet()) {
            Type t = sigVars.getKey().type();
            List<ExprHasName> variables = new LinkedList<>(sigVars.getValue());
            if (variables.isEmpty())
                continue;
            Expr bound = t.toExpr();
            Decl d = new Decl(null, null, null, variables, bound);
            decls.add(d);
        }
        return decls;
    }

    private List<Decl> getVariablesDecls(List<ExprVar> varsToDeclare) {
        Map<String, List<ExprHasName>> variablesPerType = new TreeMap<>();
        Map<String, Type> typeMap = new HashMap<>();
        List<Decl> decls = new LinkedList<>();
        for (ExprVar v : varsToDeclare) {
            Type t = v.type();
            String tString = t.toString();
            List<ExprHasName> vars;
            if (variablesPerType.containsKey(tString))
                vars = variablesPerType.get(tString);
            else {
                vars = new LinkedList<>();
                variablesPerType.put(tString, vars);
            }
            vars.add(v);
            typeMap.put(tString, t);
        }
        for (Entry<String, List<ExprHasName>> varsOfType : variablesPerType.entrySet()) {
            Type t = typeMap.get(varsOfType.getKey());
            List<ExprHasName> variables = varsOfType.getValue();
            Expr bound = t.toExpr();
            Decl d = new Decl(null, null, null, variables, bound);
            decls.add(d);
        }
        return decls;
    }

    private ExprVar getSkolemVarFromInstance(A4Solution instance, String name) {
        for (ExprVar skolem : instance.getAllSkolems()) {
            if (skolem.label.compareToIgnoreCase(name) == 0|| skolem.label.compareToIgnoreCase(TestGeneratorHelper.alloyVarNameToInternalAtomNotation(name, true)) == 0)
                return skolem;
        }
        return null;
    }

    private ExprVar getSignatureVarFromInstance(A4Solution instance, String name) {
        boolean retry = true;
        while (retry) {
            retry = false;
            for (ExprVar atom : instance.getAllAtoms()) {
                if (atom.label.compareToIgnoreCase(name) == 0 || atom.label.compareToIgnoreCase(TestGeneratorHelper.alloyVarNameToInternalAtomNotation(name, false)) == 0)
                    return atom;
            }
            for (Entry<Object, String> entry : instance.getAtom2name().entrySet()) {
                if (entry.getKey() instanceof String) {
                    String key = (String) entry.getKey();
                    if (key.compareToIgnoreCase(entry.getValue()) == 0)
                        break;
                    if (key.compareToIgnoreCase(name) == 0 || key.compareToIgnoreCase(TestGeneratorHelper.alloyVarNameToInternalAtomNotation(name, false)) == 0) {
                        name = entry.getValue();
                        if (!name.startsWith("unused")) {
                            retry = true;
                        }
                        break;
                    }
                }
            }
        }
        return null;
    }

    private List<Expr> generateDisjointExpressions(List<ExprVar> vars, A4Solution instance, boolean skolemVars) {
        List<Expr> disjointExpressions = new LinkedList<>();
        List<Sig> nonBuiltIntSigs = instance.getAllReachableSigs().makeCopy().stream().filter(s -> !s.isBuiltInSig()).collect(Collectors.toList());
        for (int i = 0; i < vars.size(); i++) {
            ExprVar var1 = vars.get(i);
            for (int j = i + 1; j < vars.size(); j++) {
                ExprVar var2 = vars.get(j);
                if (var1 == var2)
                    continue;
                boolean typesMatch = var1.type().equals(var2.type()) || var2.type().isSubtypeOf(var1.type()) || var1.type().isSubtypeOf(var2.type());
                if (!typesMatch) {
                    List<Sig> var1ExtendingSigs = parentSignatures(unaryTypeToSig(var1.type()), nonBuiltIntSigs);
                    List<Sig> var2ExtendingSigs = parentSignatures(unaryTypeToSig(var2.type()), nonBuiltIntSigs);
                    typesMatch = var1ExtendingSigs.stream().anyMatch(var2ExtendingSigs::contains);
                }
                if (!typesMatch)
                    continue;
                ExprVar var1Updated = skolemVars?getSkolemVarFromInstance(instance, var1.label):getSignatureVarFromInstance(instance, var1.label);
                ExprVar var2Updated = skolemVars?getSkolemVarFromInstance(instance, var2.label):getSignatureVarFromInstance(instance, var2.label);
                Expr equalityCheck = ExprBinary.Op.EQUALS.make(null, null, var1Updated, var2Updated);
                if (equalityCheck.errors != null && !equalityCheck.errors.isEmpty())
                    throw new IllegalStateException("Bad expression generated when creating variable equality check (\n" +
                            " var1: "  + var1Updated +
                            " var2: " + var2Updated +
                            "\n) : " + equalityCheck.errors.stream().map(Throwable::toString).collect(Collectors.joining(","))
                    );
                Optional<Boolean> constantValue = ExpressionEvaluator.evaluateFormula(instance, equalityCheck);
                if (constantValue.isPresent()) {
                    if (!constantValue.get()) {
                        Expr disjoint = ExprBinary.Op.NOT_EQUALS.make(null, null, var1, var2);
                        disjointExpressions.add(disjoint);
                    } else {
                        Expr disjoint = ExprBinary.Op.EQUALS.make(null, null, var1, var2);
                        disjointExpressions.add(disjoint);
                    }
                } else {
                    throw new IllegalStateException("Variable equality check failed to evaluate (" +  equalityCheck + ")");
                }
            }
        }
        return disjointExpressions;
    }

    private Expr generateSome(List<Decl> decls, Expr sub) {
        if (decls.isEmpty())
            return sub;
        return ExprQt.Op.SOME.make(null, null, ConstList.make(decls), sub);
    }

    private Expr getFacts(CompModule context) {
        Expr facts = context.getAllReachableFacts();
        for (Sig s : context.getAllSigs()) {
            for (Expr sFact : s.getFacts()) {
                Expr fixedSignatureFacts = fixFacts(sFact, s);
                if (facts instanceof ExprConstant)
                    facts = fixedSignatureFacts;
                else
                    facts = ExprBinary.Op.AND.make(null, null, facts, fixedSignatureFacts);
            }
        }
        return facts;
    }

    private Expr fixFacts(Expr facts, Sig sig) {
        ExprVar thisVar = ExprVar.make(null, "this");
        FactsFixer factsFixer = new FactsFixer(thisVar, sig);
        Optional<Expr> fixedFacts = factsFixer.visitThis(facts);
        return fixedFacts.orElse(facts);
    }

    private boolean hasFacts(CompModule context) {
        return !(getFacts(context) instanceof ExprConstant);
    }

    private ExprList generateInitialization(Map<Sig, List<ExprVar>> signaturesValues, Map<Field, List<Expr>> fieldsValues, Map<ExprVar, List<Expr>> variablesValues, List<Expr> fieldOverridesInitialization) {
        List<Expr> sigsInitialization = generateInitialization(signaturesValues);
        List<Expr> fieldsInitialization = generateInitialization(fieldsValues);
        List<Expr> variablesInitialization = generateInitialization(variablesValues);
        List<Expr> initExpressions = new LinkedList<>(sigsInitialization);
        initExpressions.addAll(fieldsInitialization);
        initExpressions.addAll(variablesInitialization);
        initExpressions.addAll(fieldOverridesInitialization);
        Expr initialization = initExpressions.isEmpty()?null:ExprList.make(null, null, ExprList.Op.AND, initExpressions);
        if (initialization != null && initialization.errors != null && !initialization.errors.isEmpty())
            throw new IllegalStateException("Bad expression generated when creating test initialization (\n" +
                    " signatures: "  + signaturesValues +
                    " fields: " + fieldsValues +
                    " variables: " + variablesValues +
                    "\n) : " + initialization.errors.stream().map(Throwable::toString).collect(Collectors.joining(","))
            );
        return (ExprList) initialization;
    }

    private <E extends Expr, F extends Expr> List<Expr> generateInitialization(Map<E, List<F>> map) {
        List<Expr> initExpressions = new LinkedList<>();
        for (Entry<E, List<F>> values : map.entrySet()) {
            Expr left;
            if (values.getKey() instanceof Field) {
                Field leftAsField = (Field) values.getKey();
                left = (Expr) leftAsField.fullCopy();
            } else
                left = (Expr) values.getKey().clone();
            Expr right = getInitValue(values.getValue());
            Expr init;
            if (right == null)
                init = ExprUnary.Op.NO.make(null, left);
            else
                init = ExprBinary.Op.EQUALS.make(null, null, left, right);
            initExpressions.add(init);
        }
        return initExpressions;
    }

    private final Map<String, ExprVar> varsCache = new TreeMap<>();
    private void clearVarsCache() {varsCache.clear();}

    private Map<Sig, List<ExprVar>> getSignaturesAtoms(A4Solution solution, CompModule context) {
        Map<Relation, TupleSet> counterExampleSignatures = getCounterExampleSignatures(solution, context); //TODO: This method can be simplified to just return the list of ExprVar
        Map<Sig, List<ExprVar>> signatureAtoms = new HashMap<>();
        for (Map.Entry<Relation, TupleSet> ceSignature : counterExampleSignatures.entrySet()) {
            Optional<Sig> oSig = nameToSig(ceSignature.getKey(), context);
            if (!oSig.isPresent())
                throw new IllegalStateException("No signature found for " + ceSignature);
            List<ExprVar> sigValues = new LinkedList<>();
            for (Tuple value : ceSignature.getValue()) {
                String varName = solution.getAtom2name().get(value.atom(0).toString());
                ExprVar valueAsVar;
                if (varsCache.containsKey(varName)) {
                    valueAsVar = varsCache.get(varName);
                } else {
                    valueAsVar = getExprVar(varName, solution);
                    varsCache.put(varName, valueAsVar);
                }
                sigValues.add(valueAsVar);
            }
            signatureAtoms.put(oSig.get(), sigValues);
        }
        return signatureAtoms;
    }

    private Map<Field, List<Expr>> getFieldsValues(A4Solution solution, CompModule context, Map<Sig, List<ExprVar>> signatureValues) {
        Map<Field, List<Expr>> fieldValues = new HashMap<>();
        Map<Relation, TupleSet> counterExampleFields = getCounterExampleFields(solution, context);
        for (Map.Entry<Relation, TupleSet> ceField : counterExampleFields.entrySet()) {
            Optional<Field> oField = nameToField(ceField.getKey(), context);
            if (!oField.isPresent())
                throw new IllegalStateException("No field found for " + ceField);
            List<Expr> fValues = new LinkedList<>();
            for (Tuple rawValue : ceField.getValue()) {
                Expr fValue = tupleToExpr(rawValue, signatureValues, solution);
                fValues.add(fValue);
            }
            fieldValues.put(oField.get(), fValues);
        }
        return fieldValues;
    }

    private List<Expr> getFieldOverrides(A4Solution solution, CompModule context, Map<Sig, List<ExprVar>> signatureValues) {
        List<Expr> fieldOverrides = new LinkedList<>();
        if (!useModelOverriding())
            return fieldOverrides;
        for (Field f : getAllFields(context)) {
            CompModule declaringModule = context.getDeclaringModule(f);
            if (declaringModule != null && CESigAndFieldOverriding.getInstance().fieldOverridePresent(f, declaringModule.getModelName())) {
                if (CESigAndFieldOverriding.getInstance().fieldIsOverridenByFunction(f, declaringModule.getModelName())) {
                    String function = CESigAndFieldOverriding.getInstance().getFieldOverridingFunction(f, declaringModule.getModelName());
                    Optional<Func> func = getNoParametersFunctionFromModel(function, declaringModule.getModelName(), context);
                    if (func.isPresent()) {
                        Expr fieldOverride = null;
                        ExprCall call = (ExprCall) ExprCall.make(
                                null, null, func.get(), null, 0
                        );
                        Object functionResult = getCounterExampleFunctionValues(solution, func.get());
                        if (functionResult instanceof Integer) {
                            int result = (Integer) functionResult;
                            ExprConstant intValue = (ExprConstant) ExprConstant.makeNUMBER(result);
                            fieldOverride = ExprBinary.Op.EQUALS.make(null, null, call, intValue);
                        } else if (functionResult instanceof Boolean) {
                            boolean result = (Boolean) functionResult;
                            if (!result)
                                fieldOverride = ExprUnary.Op.NO.make(null, call);
                            else
                                fieldOverride = call;
                        } else if (functionResult instanceof A4TupleSet) {
                            TupleSet tupleSet = ((A4TupleSet) functionResult).debugGetKodkodTupleset();
                            Expr rightHandSide = null;
                            for (Tuple rawValue : tupleSet) {
                                Expr vValue = tupleToExpr(rawValue, signatureValues, solution);
                                if (rightHandSide == null)
                                    rightHandSide = vValue;
                                else
                                    rightHandSide = ExprBinary.Op.PLUS.make(null, null, rightHandSide, vValue);
                            }
                            fieldOverride = ExprBinary.Op.EQUALS.make(null, null, call, rightHandSide);
                        }
                        if (fieldOverride != null)
                            fieldOverrides.add(fieldOverride);
                    }
                }
            }
        }
        return fieldOverrides;
    }

    private Map<ExprVar, List<Expr>> getVariablesValues(A4Solution solution, Map<Sig, List<ExprVar>> signatureValues) {
        Map<ExprVar, List<Expr>> variablesValues = new HashMap<>();
        Map<ExprVar, TupleSet> counterExampleVariables = getCounterExampleVariables(solution);
        for (Map.Entry<ExprVar, TupleSet> ceVariable : counterExampleVariables.entrySet()) {
            String varName = ceVariable.getKey().label;
            ExprVar varOriginal = ceVariable.getKey();
            ExprVar var = ExprVar.make(null, varName, varOriginal.type());
            List<Expr> vValues = new LinkedList<>();
            for (Tuple rawValue : ceVariable.getValue()) {
                Expr vValue = tupleToExpr(rawValue, signatureValues, solution);
                vValues.add(vValue);
            }
            variablesValues.put(var, vValues);
        }
        return variablesValues;
    }

    private Map<ExprVar, TupleSet> getCounterExampleVariables(A4Solution solution) {
        Map<ExprVar, TupleSet> variables = new HashMap<>();
        Evaluator evaluator = solution.getEvaluator();
        if (evaluator != null) {
            for (ExprVar var : solution.getAllSkolems()) {
                for (Map.Entry<Relation, TupleSet> relation : evaluator.instance().relationTuples().entrySet()) {
                    if (relation.getKey().name().equals(var.label))
                        variables.put(var, relation.getValue());
                }
            }
        }
        return variables;
    }

    private Map<Relation, TupleSet> getCounterExampleFields(A4Solution solution, CompModule context) {
        Map<Relation, TupleSet> fieldValues = new HashMap<>();
        for (Sig s : context.getAllReachableSigs()) {
            for (Field f : s.getFields()) {
                CompModule declaringModule = context.getDeclaringModule(f);
                if (declaringModule != null && CESigAndFieldOverriding.getInstance().fieldOverridePresent(f, declaringModule.getModelName())) {
                    continue;
                }
                A4TupleSet values = solution.eval(f);
                Relation rel = Relation.nary(s.label + "." + f.label, f.type().arity());
                TupleSet tupleSet = values.debugGetKodkodTupleset();
                fieldValues.put(rel, tupleSet);
            }
        }
        return fieldValues;
    }

    private Object getCounterExampleFunctionValues(A4Solution solution, Func f) {
        if (!f.decls.isEmpty())
            return Optional.empty();
        ExprCall call = (ExprCall) ExprCall.make(
                null, null, f, null, 0
        );
        return solution.eval(call);
    }

    private Map<Relation, TupleSet> getCounterExampleSignatures(A4Solution solution, CompModule context) {
        return getCounterExampleRelations(solution, context);
    }

    private Map<Relation, TupleSet> getCounterExampleRelations(A4Solution solution, CompModule context) {
        Map<Relation, TupleSet> sigValues = new HashMap<>();
        for (Sig s : context.getAllReachableSigs()) {
            if (s.label.compareTo(Sig.PrimSig.UNIV.label) == 0)
                continue;
            if (s.label.compareTo(Sig.PrimSig.SIGINT.label) == 0)
                continue;
            if (s.label.compareTo(Sig.PrimSig.SEQIDX.label) == 0)
                continue;
            if (s.label.compareTo(Sig.PrimSig.STRING.label) == 0)
                continue;
            if (s.label.compareTo(Sig.PrimSig.NONE.label) == 0)
                continue;
            CompModule declaringModule = context.getDeclaringModule(s);
            if (declaringModule != null && CESigAndFieldOverriding.getInstance().signatureShouldBeIgnored(s, declaringModule.getModelName())) {
                continue;
            }
            A4TupleSet values = solution.eval(s);
            Relation rel = Relation.nary(s.label, s.type().arity());
            TupleSet tupleSet = values.debugGetKodkodTupleset();
            sigValues.put(rel, tupleSet);
        }
        return sigValues;
    }

    private ExprVar getExprVar(String name, A4Solution solution) {
        for (ExprVar atom : solution.getAllAtoms()) {
            if (atom.label.compareTo(name) == 0)
                return atom;
        }
        return null;
    }

}

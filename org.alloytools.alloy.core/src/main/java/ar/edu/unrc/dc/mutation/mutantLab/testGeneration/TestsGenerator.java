package ar.edu.unrc.dc.mutation.mutantLab.testGeneration;

import ar.edu.unrc.dc.mutation.CheatingIsBadMkay;
import ar.edu.unrc.dc.mutation.Cheats;
import ar.edu.unrc.dc.mutation.MutationConfiguration;
import ar.edu.unrc.dc.mutation.util.ExpressionEvaluator;
import ar.edu.unrc.dc.mutation.util.RepairReport;
import ar.edu.unrc.dc.mutation.visitors.FunctionsCollector;
import ar.edu.unrc.dc.mutation.visitors.SearchCall;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.Pos;
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
    private static boolean GENERATE_DEBUG_TESTS = false;

    static {
        try {
            // This block configure the logger with handler and formatter
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

    private static TestsGenerator instance;

    public static TestsGenerator getInstance() {
        if (instance == null) {
            instance = new TestsGenerator();
        }
        return instance;
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

    public static String buggyFuncsFile() {
        return (String) MutationConfiguration.getInstance().getConfigValue(TEST_GENERATION_INSTANCES_TESTS_GENERATION_BUGGY_FUNCS_FILE).orElse(TEST_GENERATION_INSTANCES_TESTS_GENERATION_BUGGY_FUNCS_FILE.defaultValue());
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
            logger.info(msg + request.solution().getEvaluator().instance().relationTuples().entrySet().toString());
            if (positiveAndNegativeTestGeneration) {
                List<TestGenerationRequest> positiveAndNegativeRequests = request.splitABothRequestIntoPositiveAndNegative();
                Set<Command> positiveTests = generateNewTest(positiveAndNegativeRequests.get(0));
                generatedTests.addAll(positiveTests);
                Set<Command> negativeTests = generateNewTest(positiveAndNegativeRequests.get(1));
                generatedTests.addAll(negativeTests);
                testsGenerated += positiveTests.size() + negativeTests.size();
            } else {
                Set<Command> newTests = generateNewTest(request);
                generatedTests.addAll(newTests);
                testsGenerated += newTests.size();
            }
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
        Map<ExprVar, List<Expr>> variablesValues = getVariablesValues(solution, signatureValues, command);
        List<ExprVar> skolemVariables = new LinkedList<>(variablesValues.keySet());
        Browsable predicateOrAssertionCalled = getPredicateOrAssertionCalled(command, context);
        if (predicateOrAssertionCalled == null)
            throw new IllegalStateException("Couldn't get predicate or assertion for " + command.toString());
        PropertyExtractor propertyExtractor = new PropertyExtractor();
        ExtractedProperty extractedProperty;
        if (predicateOrAssertionCalled instanceof Func) {
            extractedProperty = propertyExtractor.extractFromPredicate((Func) predicateOrAssertionCalled);
        } else {
            extractedProperty = propertyExtractor.extractFromAssertion((Expr) predicateOrAssertionCalled);
        }
        List<ExprVar> originalVariables = extractedProperty.getVariables();
        VariableMapping variableMapping = new VariableMapping(originalVariables, skolemVariables, command);
        PropertyCleaner propertyCleaner = new PropertyCleaner();
        Expr cleanedFormula = propertyCleaner.cleanExpression(extractedProperty.getProperty());
        if (cleanedFormula == null)
            throw new IllegalStateException("Cleaned formula is null");
        if (arepairIntegration()) { //We will check that at most one predicate/function is present
            FunctionsCollector functionsCollector = new FunctionsCollector();
            Set<Func> functions = functionsCollector.visitThis(cleanedFormula);
            if (functions.size() > 1) {
                String message = "ARepair integration mode can only support at most one predicate, we found " + functions.size() + " for current expression " + cleanedFormula.toString();
                logger.severe(message);
                throw new IllegalStateException(message);
            } else if (functions.size() == 1 && !functions.stream().findFirst().get().isPred) {
                String message = "ARepair integration mode can only support predicates, we found function " + functions.stream().findFirst().get().toString() + " in current expression " + cleanedFormula.toString();
                logger.severe(message);
                throw new IllegalStateException(message);
            }
        }
        VariableExchanger variableExchanger = new VariableExchanger(variableMapping);
        Expr testFormula = variableExchanger.replaceVariables((Expr)cleanedFormula.clone());
        testFormula.setCommentBefore("testFormula");
        if (GENERATE_DEBUG_TESTS) {
            testFormula = ExprUnary.Op.NOT.make(null, testFormula);
        } else if (!request.isInstanceTestRequest()) {
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
        Map<ExprVar, List<Expr>> usedVariablesValues = new HashMap<>();
        List<ExprVar> usedVariables = new LinkedList<>();
        for (Entry<ExprVar, List<Expr>> vValues : variablesValues.entrySet()) {
            if (variableMapping.isSkolemUsed(vValues.getKey())) {
                usedVariablesValues.put(vValues.getKey(), vValues.getValue());
                usedVariables.add(vValues.getKey());
            }
        }
        List<Expr> fieldOverrides = getFieldOverrides(solution, context, signatureValues);
        Expr initialization = generateInitialization(signatureValues, fieldValues, usedVariablesValues, fieldOverrides);
        if (arepairIntegration()) {
            Set<Command> testCommands = new HashSet<>();
            if (!request.isInstanceTestRequest() || request.isInstanceNegativeTestRequest()) {
                List<TestBody> testBodies = getBodyForARepairIntegrationCounterExample(testFormula, context, solution);
                for (TestBody testBody : testBodies) {
                    testBody.fromInstance(true); //is a bad fix, but we don't want counterexample tests while in arepair integration mode
                    if (request.isInstanceTestRequest() && !request.fromTrustedCommand())
                        testBody.trusted = false;
                    Command testCommand = generateTest(initialization, testBody, usedVariables, declSignatureValues, command, context);
                    testCommands.add(testCommand);
                }
                return testCommands;
            } else if (request.isInstancePositiveTestRequest()) {
                logger.info("positive instance tests not yet supported while on arepair integration mode");
                return Collections.emptySet();
            } else {
                throw new IllegalStateException("Unexpected request: " + request.toString());
            }
        } else {
            TestBody testBody;
            if (!request.isInstanceTestRequest()) {
                testBody = TestBody.trustedSatisfiablePredicate(testFormula);
                testBody.fromInstance(false);
            } else if (request.isInstancePositiveTestRequest()) {
                testBody = request.fromTrustedCommand()?TestBody.trustedSatisfiablePredicate(testFormula):TestBody.untrustedSatisfiablePredicate(testFormula);
            } else if (request.isInstanceNegativeTestRequest()) {
                testBody = request.fromTrustedCommand()?TestBody.trustedUnsatisfiablePredicate(testFormula):TestBody.untrustedUnsatisfiablePredicate(testFormula);
            } else {
                throw new IllegalStateException("Unexpected request: " + request.toString());
            }
            Command testCommand = generateTest(initialization, testBody, usedVariables, declSignatureValues, command, context);
            return Collections.singleton(testCommand);
        }
//        Func testPredicate = generateTestPredicate(initialization, testFormula, usedVariables, declSignatureValues, command);
//        testPredicate.setCommentPreviousLine("TEST START");
//        testPredicate.setCommentNextLine("TEST FINISH");
//        Command testCommand = generateTestCommand(testPredicate, request);
//        logger.info("Test generated\n" +
//                    testPredicate.toString() + "\n" +
//                    testPredicate.getBody().toString()
//        );
//        try {
//            Cheats.addFunctionToModule(context, testPredicate);
//        } catch (CheatingIsBadMkay e) {
//            throw new Error("An error occurred while adding counter example predicate to ast", e);
//        }
//        try {
//            Cheats.addCommand(testCommand, context);
//        } catch (CheatingIsBadMkay e) {
//            throw new Error("An error occurred while adding counter example command to ast", e);
//        }
//        return testCommand;
    }

    private Command generateTest(Expr initialization, TestBody testBody, List<ExprVar> usedVariables, Map<Sig, List<ExprVar>> declSignatureValues, Command command, CompModule context) {
        Func testPredicate = generateTestPredicate(initialization, testBody.body(), usedVariables, declSignatureValues, command);
        testPredicate.setCommentPreviousLine("TEST START");
        testPredicate.setCommentNextLine("TEST FINISH");
        Command testCommand = generateTestCommand(testPredicate, !testBody.fromInstance(), testBody.isExpected(), testBody.isTrusted());
        logger.info("Test generated\n" +
                testPredicate.toString() + "\n" +
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
        else if (positiveTest && trusted)
            testCommand.testType(Command.TestType.POS_TRUSTED);
        else if (positiveTest)
            testCommand.testType(Command.TestType.POS_UNTRUSTED);
        else if (trusted)
            testCommand.testType(Command.TestType.NEG_TRUSTED);
        else testCommand.testType(Command.TestType.NEG_UNTRUSTED);
        return testCommand;
    }

    private Func generateTestPredicate(Expr initialization, Expr testFormula, List<ExprVar> skolemVariables, Map<Sig, List<ExprVar>> signatureValues, Command cmd) {
        Expr sub = testFormula!=null?ExprBinary.Op.AND.make(null, null, initialization, testFormula):initialization;
        List<Decl> signatureDecls = getVariablesDecls(signatureValues);//getVariablesDecls(varsToDeclare);
        List<Decl> skolemDecls = getVariablesDecls(skolemVariables);
        Expr body;
        if (!skolemDecls.isEmpty()) {
            body = generateDisjSome(skolemDecls, sub);
            body = generateDisjSome(signatureDecls, body);
        } else {
            body = generateDisjSome(signatureDecls, sub);
        }
        String from = cmd.nameExpr instanceof ExprVar?((ExprVar) cmd.nameExpr).label:"NO_NAME";
        String name = getTestName(from);
        Func testPredicate = new Func(null, name, null, null, body);
        testPredicate.setGenerated();
        return testPredicate;
    }


    private static class TestBody {
        private final Expr body;
        private final boolean expect;
        private boolean trusted;

        private boolean fromInstance = false;

        public static TestBody trustedUnexpectedInstance() {
            return new TestBody(null, false, true);
        }

        public static TestBody untrustedUnexpectedInstance() {
            return new TestBody(null, false, false);
        }

        public static TestBody trustedSatisfiablePredicate(Expr pred) {
            return new TestBody(pred, true, true);
        }

        public static TestBody untrustedSatisfiablePredicate(Expr pred) {
            return new TestBody(pred, true, false);
        }

        public static TestBody trustedUnsatisfiablePredicate(Expr pred) {
            return new TestBody(pred, false, true);
        }

        public static TestBody untrustedUnsatisfiablePredicate(Expr pred) {
            return new TestBody(pred, false, false);
        }

        private TestBody(Expr body, boolean expect, boolean trusted) {
            this.body = body;
            this.expect = expect;
            this.trusted = trusted;
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

    }
    private List<TestBody> getBodyForARepairIntegrationCounterExample(Expr originalBody, CompModule context, A4Solution instance) {
        FunctionsCollector functionsCollector = new FunctionsCollector();
        Set<Func> functions = functionsCollector.visitThis(originalBody);
        boolean hasFacts = hasFacts(context);
        if (originalBody instanceof ExprUnary && ((ExprUnary)originalBody).op.equals(ExprUnary.Op.NOOP))
            return getBodyForARepairIntegrationCounterExample(((ExprUnary)originalBody).sub, context, instance);
        if (functions.isEmpty())
            return Collections.singletonList(TestBody.trustedUnexpectedInstance());
        Func pred = functions.stream().findFirst().get();
        Boolean expected = null;
        boolean invert;
        boolean lor; //false: left, true: right
        Expr current = originalBody;
        SearchCall searchExpr = new SearchCall(null);
        while (true) {
            invert = false;
            if (current instanceof ExprBinary) {
                ExprBinary currentAsBinaryExpr = (ExprBinary) current;
                Expr formulaToEvaluate;
                searchExpr.setTarget(pred);
                if (searchExpr.visitThis(currentAsBinaryExpr.left)) {
                    formulaToEvaluate = currentAsBinaryExpr.right;
                    lor = false;
                } else if (searchExpr.visitThis(currentAsBinaryExpr.right)) {
                    formulaToEvaluate = currentAsBinaryExpr.left;
                    lor = true;
                } else {
                    throw new IllegalStateException("Predicate exists but can't be located in the expression (pred: " + pred.toString() + ") (expr: " + current.toString() + ")");
                }
                Optional<Boolean> formulaEvaluation = ExpressionEvaluator.evaluateFormula(context, instance, formulaToEvaluate);
                if (!formulaEvaluation.isPresent())
                    throw new IllegalStateException("Failed to evaluate " + formulaToEvaluate.toString() + " to a boolean value");
                Expr predsFormula = lor?currentAsBinaryExpr.right:currentAsBinaryExpr.left;
                switch (currentAsBinaryExpr.op) {
                    case NOT_EQUALS: invert = true;
                    case IFF:
                    case EQUALS: {
                        if (expected == null)
                            expected = Boolean.TRUE;
                        if (invert)
                            expected = !expected;
                        boolean isFormulaSameAsExpected = expected == formulaEvaluation.get();
                        if (needToIterate(predsFormula)) {
                            current = predsFormula;
                        } else {
                            if (!isFormulaSameAsExpected) {
                                return Collections.singletonList(TestBody.trustedUnsatisfiablePredicate(predsFormula));
                            } else {
                                if (hasFacts) {
                                    return Arrays.asList(
                                            TestBody.untrustedSatisfiablePredicate(predsFormula),
                                            TestBody.untrustedUnexpectedInstance()
                                    );
                                } else {
                                    return Collections.singletonList(
                                            TestBody.trustedSatisfiablePredicate(predsFormula)
                                    );
                                }
                            }
                        }
                        //(F == pred) should be E {true, false}
                            //si existen facts -> {instancia ^ pred expect (F = E?1:0)), instancia expect 0} (ambos no confiables)
                            //sino existen facts -> instancia ^ pred expect (F = E?1:0) (confiable)
                        //-------------------------------------------------------------------------
                        //(F == pred) should be E {true, false}
                            //F = E
                                //si existen facts -> {instancia ^ pred expect 1, instancia expect 0} (ambos no confiables)
                                //sino existen facts -> instancia ^ pred expect 1 (confiable)
                            //F != E
                                //si existen facts -> {instancia ^ pred expect 0, instancia expect 0} (ambos no confiables)
                                //sino existen facts -> instancia ^ pred expect 0 (confiable)
                        //-------------------------------------------------------------------------
                        //(F == pred) == G
                            //G = true
                                //<por contraejemplo>
                                //F == pred = false y deberían ser true
                                    //F = true
                                        //<por contraejemplo>
                                        //pred = false y debería ser true
                                        //sino existen facts -> instancia ^ pred expect 1
                                        //si existen facts -> {instancia ^ pred expect 1, instancia expect 0} (ambos no confiables)
                                    //F = false
                                        //<por contraejemplo>
                                        //pred = true y debería ser false
                                        //sino existen facts -> instancia ^ pred expect 0
                                        //si existen facts -> {instancia ^ pred expect 0, instancia expect 0} (ambos no confiables)
                            //G = false
                                //F == pred = true y deberían ser false
                                    //F = true
                                        //<por contraejemplo>
                                        //pred = true y debería ser false
                                        //sino existen facts -> instancia ^ pred expect 0
                                        //si existen facts -> {instancia ^ pred expect 0, instancia expect 0} (ambos no confiables)
                                    //F = false
                                        //<por contraejemplo>
                                        //pred = false y debería ser true
                                        //sino existen facts -> instancia ^ pred expect 1
                                        //si existen facts -> {instancia ^ pred expect 1, instancia expect 0} (ambos no confiables)

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
                            if (hasFacts) {
                                return Arrays.asList(TestBody.untrustedSatisfiablePredicate(predsFormula), TestBody.untrustedUnexpectedInstance());
                            } else {
                                return Collections.singletonList(TestBody.trustedSatisfiablePredicate(predsFormula));
                            }
                        } else if (expected || !impliesLor && formulaEvaluation.get()) {
                            if (needToIterate(predsFormula)) {
                                expected = false;
                                current = predsFormula;
                                continue;
                            }
                            return Collections.singletonList(TestBody.trustedUnsatisfiablePredicate(predsFormula));
                        } else
                          return Collections.singletonList(TestBody.trustedUnexpectedInstance());
                        //impliesLor = F is left or right
                        //formulaToEvaluateEval = the value (true or false) of the formulate to evaluate
                        //if ((expected && !impliesLor) || (!expected && impliesLor && !formulaToEvaluateEval))
                        //  if (facts)
                        //      {instance ^ pred expect 1, instance expect 0} (both untrusted)
                        //  else
                        //      {instance ^ pred expect 1} (trusted)
                        //else if ((expected && impliesLor) || (!expected && !impliesLor && formulaToEvaluateEval))
                        //  {instance ^ pred expect 0} (trusted)
                        //else
                        //  {instance expect 0} (trusted)
                        //---------------------------------------------------------------
                        //If expected is true
                            //F implies pred
                                //<by counterexample>
                                //F must be true and pred must be false
                                //if facts exist -> {instance ^ pred expect 1, instance expect 0} (both untrusted)
                                //no facts exist -> {instance ^ pred expect 1} (trusted)
                            //pred implies F
                                //<by counterexample>
                                //F must be false and pred must be true
                                //<by trusted formula F, pred should not be true or instance should not exist>
                                //{instance ^ pred expect 0} (trusted)
                        //If expected is false
                            //F implies pred
                                //F = false
                                    //<by trusted formula F, pred has nothing to do with the issue>
                                    //{instance expect 0} (trusted)
                                //F = true
                                    //<by counterexample>
                                    //pred must be true, and should be false, or the facts should not let that instance exist
                                    //{instance ^ pred expect 0} (trusted)
                            //pred implies F
                                //<by counterexample>
                                //pred and F must be true, or pred must be false, if F is false then pred should be true
                                    //F = false
                                        //<by counterexample and trusted formula F, pred should be true or the instance should not exist>
                                        //if facts exist -> {instance ^ pred expect 1, instance expect 0} (both untrusted)
                                        //no facts exist -> {instance ^ pred expect 1} (trusted)
                                    //F = true
                                        //<by counterexample and trusted formula F>
                                        //{instance expect 0} (trusted)
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
                            if (hasFacts) {
                                return Arrays.asList(TestBody.untrustedSatisfiablePredicate(predsFormula), TestBody.untrustedUnexpectedInstance());
                            } else {
                                return Collections.singletonList(TestBody.trustedSatisfiablePredicate(predsFormula));
                            }
                        } else {
                            if (!formulaEvaluation.get())
                                return Collections.singletonList(TestBody.trustedUnexpectedInstance());
                            else {
                                return Collections.singletonList(TestBody.trustedUnsatisfiablePredicate(predsFormula));
                            }
                        }
                        //if expected
                            //if facts exist -> {instance ^ pred expect 1, instance expect 0} (both untrusted)
                            //no facts exist -> {instance ^ pred expect 1} (trusted)
                        //if not expected
                            //F = false
                                //{instance expect 0}
                            //F = true
                                //{instance ^ pred expect 0} (trusted)
                    }
                    case OR: {
                        if (expected == null)
                            expected = Boolean.TRUE;
                        if (needToIterate(predsFormula)) { //expected value doesn't change
                            current = predsFormula;
                            continue;
                        }
                        if (expected) {
                            return hasFacts?
                                    Arrays.asList(
                                            TestBody.untrustedSatisfiablePredicate(predsFormula),
                                            TestBody.untrustedUnexpectedInstance()
                                    ):
                                    Collections.singletonList(TestBody.trustedSatisfiablePredicate(predsFormula));
                        } else if (formulaEvaluation.get())
                            return Collections.singletonList(TestBody.trustedUnexpectedInstance());
                        else {
                            return Collections.singletonList(TestBody.trustedUnsatisfiablePredicate(predsFormula));
                        }
                        //if expected
                            //F || pred is false and should be true, both F and pred are false
                            //if facts exist -> {instance ^ pred expect 1, instance expect 0} (both untrusted)
                            //no facts exist -> {instance ^ pred expect 1} (trusted)
                        //if not expected
                            //F || pred is true and should be false, either F, pred, or both are true
                            //F is true
                                //{instance expect 0}
                            //F is false
                                //{instance ^ pred expect 0} (trusted)
                    }
                    default: throw new IllegalStateException("Unsupported binary operator (" + currentAsBinaryExpr.op.toString() + ") in expression: " + currentAsBinaryExpr.toString());
                }
            } else if (current instanceof ExprUnary) {
                ExprUnary currentAsExprUnary = (ExprUnary) current;
                if (currentAsExprUnary.op.equals(ExprUnary.Op.NOOP))
                    return getBodyForARepairIntegrationCounterExample(currentAsExprUnary.sub, context, instance);
                if (currentAsExprUnary.op.equals(ExprUnary.Op.NOT)) {
                    if (expected == null)
                        expected = Boolean.TRUE;
                    if (needToIterate(currentAsExprUnary.sub)) {
                        expected = !expected;
                        current = currentAsExprUnary.sub;
                        continue;
                    }
                    if (expected) {
                        return Collections.singletonList(TestBody.trustedUnsatisfiablePredicate(currentAsExprUnary.sub));
                    } else {
                        return hasFacts?
                                Arrays.asList(
                                        TestBody.untrustedSatisfiablePredicate(currentAsExprUnary.sub),
                                        TestBody.untrustedUnexpectedInstance()
                                ):
                                Collections.singletonList(TestBody.trustedSatisfiablePredicate(currentAsExprUnary.sub));
                    }
                    //!F
                        //instance should not exist (this is treated at the start of the method)
                    //!pred
                        //if expected
                            //either pred is true and should be false, or instance should not exist
                            //{instance ^ pred expect 0} (trusted)
                        //if not expected
                            //either pred is false and should be true, or instance should not exist
                            //if facts exist -> {instance ^ pred expect 1, instance expect 0} (both untrusted)
                            //no facts exist -> {instance ^ pred expect 1} (trusted)
                } else {
                    throw new IllegalStateException("Unsupported unary operator (" + currentAsExprUnary.op.toString() + ") in expression: " + currentAsExprUnary.toString());
                }
            } else if (current instanceof ExprCall) {
                if (expected == null)
                    expected = Boolean.TRUE;
                if (expected && hasFacts) {
                    return Arrays.asList(TestBody.untrustedSatisfiablePredicate(current), TestBody.untrustedUnexpectedInstance());
                }
                if (expected) {
                    return Collections.singletonList(TestBody.trustedUnexpectedInstance());
                }
                if (hasFacts) {
                    return Arrays.asList(TestBody.untrustedUnsatisfiablePredicate(current), TestBody.untrustedUnexpectedInstance());
                }
                return Collections.singletonList(TestBody.trustedUnexpectedInstance());
            } else if (current instanceof ExprConstant) {
                return Collections.singletonList(TestBody.trustedUnexpectedInstance());
            } else if (current instanceof ExprList) {
                //NOT YET
                throw new IllegalStateException("ExprList not yet supported (" + current.toString() + ")");
            } else if (current instanceof ExprQt) {
                //NOT YET
                throw new IllegalStateException("ExprQT not yet supported (" + current.toString() + ")");
            } else if (current instanceof ExprLet) {
                //NOT YET
                throw new IllegalStateException("ExprLet not yet supported (" + current.toString() + ")");
            } else {
                //??? SOMETHING
                throw new IllegalStateException("Expr not yet supported (" + current.toString() + ")");
            }
        }
    }

    private boolean needToIterate(Expr x) {
        if (x instanceof ExprCall)
            return false;
        if (x instanceof ExprUnary) {
            ExprUnary exprUnary = (ExprUnary) x;
            if (exprUnary.op.equals(ExprUnary.Op.NOOP))
                return needToIterate(exprUnary.sub);
            return !(exprUnary.sub instanceof ExprCall);
        } else if (x instanceof ExprBinary) {
            ExprBinary exprBinary = (ExprBinary) x;
            return !(exprBinary.left instanceof ExprCall) && !(exprBinary.right instanceof ExprCall);
        } else {
            throw new IllegalStateException("Other classes of expressions are currently not supported");
        }
    }

    private String getTestName(String from) {
        if (this.testName == null) {
            return "CE_" + from + "_" + generateRandomName(10);
        }
        return this.testName + this.testIndex++;
    }

    private List<Decl> getVariablesDecls(Map<Sig, List<ExprVar>> varsPerSignature) {
        List<Decl> decls = new LinkedList<>();
        for (Entry<Sig, List<ExprVar>> sigVars : varsPerSignature.entrySet()) {
            Type t = sigVars.getKey().type();
            List<ExprHasName> variables = new LinkedList<>(sigVars.getValue());
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

    private Expr generateDisjSome(List<Decl> decls, Expr sub) {
        if (decls.isEmpty())
            return sub;
        List<Decl> newdecls = new LinkedList<>();
        Expr guard = null;
        for (Decl d : decls) {
            if (d.names.size() <= 1 || arepairIntegration()) {
                boolean disjoint = d.names.size() > 1 && arepairIntegration();
                if (disjoint) {
                    Decl disjDecl = new Decl(null, Pos.UNKNOWN, null, d.names, d.expr);
                    newdecls.add(disjDecl);
                } else {
                    newdecls.add(d);
                }
                continue;
            }
            guard = ExprList.makeDISJOINT(d.disjoint, null, d.names).and(guard);
            newdecls.add(new Decl(null, null, null, d.names, d.expr));
        }
        if (guard != null) {
            sub = guard.and(sub);
        }
        return ExprQt.Op.SOME.make(null, null, ConstList.make(newdecls), sub);
    }

    private Expr getFacts(CompModule context) {
        Expr facts = context.getAllReachableFacts();
        for (Sig s : context.getAllSigs()) {
            for (Expr sFact : s.getFacts()) {
                facts = ExprBinary.Op.AND.make(null, null, facts, sFact);
            }
        }
        return facts;
    }

    private boolean hasFacts(CompModule context) {
        return !(getFacts(context) instanceof ExprConstant);
    }

    private Expr generateInitialization(Map<Sig, List<ExprVar>> signaturesValues, Map<Field, List<Expr>> fieldsValues, Map<ExprVar, List<Expr>> variablesValues, List<Expr> fieldOverridesInitialization) {
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
                    " signatures: "  + signaturesValues.toString() +
                    " fields: " + fieldsValues.toString() +
                    " variables: " + variablesValues.toString() +
                    "\n) : " + initialization.errors.stream().map(Throwable::toString).collect(Collectors.joining(","))
            );
        return initialization;
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
        Map<Relation, TupleSet> counterExampleSignatures = getCounterExampleSignatures(solution, context);
        Map<Sig, List<ExprVar>> signatureAtoms = new HashMap<>();
        for (Map.Entry<Relation, TupleSet> ceSignature : counterExampleSignatures.entrySet()) {
            Optional<Sig> oSig = nameToSig(ceSignature.getKey(), context);
            if (!oSig.isPresent())
                throw new IllegalStateException("No signature found for " + ceSignature.toString());
            List<ExprVar> sigValues = new LinkedList<>();
            for (Tuple value : ceSignature.getValue()) {
                String varName = internalAtomNotationToAlloyName(value.atom(0).toString());
                ExprVar valueAsVar;
                if (varsCache.containsKey(varName)) {
                    valueAsVar = varsCache.get(varName);
                } else {
                    valueAsVar = ExprVar.make(null, varName, oSig.get().type());
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
                throw new IllegalStateException("No field found for " + ceField.toString());
            List<Expr> fValues = new LinkedList<>();
            for (Tuple rawValue : ceField.getValue()) {
                Expr fValue = tupleToExpr(rawValue, signatureValues);
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
                            TupleSet tset = ((A4TupleSet) functionResult).debugGetKodkodTupleset();
                            Expr rightHandSide = null;
                            for (Tuple rawValue : tset) {
                                Expr vValue = tupleToExpr(rawValue, signatureValues);
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

    private Map<ExprVar, List<Expr>> getVariablesValues(A4Solution solution, Map<Sig, List<ExprVar>> signatureValues, Command cmd) {
        Map<ExprVar, List<Expr>> variablesValues = new HashMap<>();
        Map<ExprVar, TupleSet> counterExampleVariables = getCounterExampleVariables(solution);
        for (Map.Entry<ExprVar, TupleSet> ceVariable : counterExampleVariables.entrySet()) {
            String fullVarName = ceVariable.getKey().label;
            String varName = skolemNameToAlloyName(fullVarName, cmd);
            ExprVar varOriginal = ceVariable.getKey();
            ExprVar var = ExprVar.make(null, varName, varOriginal.type());
            List<Expr> vValues = new LinkedList<>();
            for (Tuple rawValue : ceVariable.getValue()) {
                Expr vValue = tupleToExpr(rawValue, signatureValues);
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

}

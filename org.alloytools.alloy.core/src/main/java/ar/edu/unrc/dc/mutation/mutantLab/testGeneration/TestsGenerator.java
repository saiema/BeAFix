package ar.edu.unrc.dc.mutation.mutantLab.testGeneration;

import ar.edu.unrc.dc.mutation.CheatingIsBadMkay;
import ar.edu.unrc.dc.mutation.Cheats;
import ar.edu.unrc.dc.mutation.MutationConfiguration;
import ar.edu.unrc.dc.mutation.util.RepairReport;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.ast.*;
import edu.mit.csail.sdg.ast.Sig.Field;
import edu.mit.csail.sdg.parser.CompModule;
import edu.mit.csail.sdg.translator.A4Solution;
import edu.mit.csail.sdg.translator.A4TupleSet;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.IntExpression;
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
    private String testName;
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

    public List<Command> generateTestsFor(A4Solution solution, CompModule context, Command command) throws Err {
        if (solution.getOriginalCommand().compareTo(command.toString()) != 0)
            throw new IllegalArgumentException("Command argument doesn't match the one use for the obtained solution");
        logger.info("generate tests for\n" + command.toString());
        if (command.check || command.expects == 0) {
            List<Command> newTests = new LinkedList<>();
            String property;
            if (command.nameExpr instanceof ExprVar)
                property = ((ExprVar) command.nameExpr).label;
            else
                property = FACTS_PROPERTY;
            int currentTests = testsPerProperty.getOrDefault(property, 0);
            int testsToGenerate = testsToGeneratePerCommand() - currentTests;
            int testsGenerated = 0;
            for (int i = 0; i < testsToGenerate && i < testsPerGeneration(); i++) {
                logger.info("Generating test for instance\n" + solution.getEvaluator().instance().relationTuples().entrySet().toString());
                Command newTest = generateNewTest(solution, context, command);
                testsGenerated++;
                newTests.add(newTest);
                solution = solution.next();
                if (solution.getEvaluator() == null)
                    break;
            }
            testsPerProperty.put(property, currentTests + testsGenerated);
            return newTests;
        }
        return new LinkedList<>();
    }

    public Map<String, Integer> getTestAmountPerProperty() {
        return testsPerProperty;
    }

    private Command generateNewTest(A4Solution solution, CompModule context, Command command) {
        Command cmd;
        RepairReport.getInstance().testGenerationClockStart();
        try {
            cmd = generateNewTest_impl(solution, context, command);
        } catch (Exception e) {
            RepairReport.getInstance().testGenerationClockEnd();
            throw e;
        }
        RepairReport.getInstance().testGenerationClockEnd();
        return cmd;
    }

    private Command generateNewTest_impl(A4Solution solution, CompModule context, Command command) {
        clearVarsCache();
        Map<Sig, List<ExprVar>> signatureValues = getSignaturesAtoms(solution, context);
        mergeExtendingSignaturesValues(signatureValues);
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
        VariableExchanger variableExchanger = new VariableExchanger(variableMapping);
        Expr testFormula = variableExchanger.replaceVariables((Expr)cleanedFormula.clone());
        testFormula.setCommentBefore("testFormula");
        if (GENERATE_DEBUG_TESTS) {
            testFormula = ExprUnary.Op.NOT.make(null, testFormula);
        } else {
            Expr facts = getFacts(context);
            if (!(facts instanceof ExprConstant)) {
                Expr negateFacts = ExprUnary.Op.NOT.make(null, getFacts(context));
                testFormula = ExprBinary.Op.OR.make(null, null, negateFacts, testFormula);
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
        Func testPredicate = generateTestPredicate(initialization, testFormula, usedVariables, signatureValues, command);
        Command testCommand = generateTestCommand(testPredicate);
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

    private Command generateTestCommand(Func testPredicate) {
        ExprVar predName = ExprVar.make(null, testPredicate.label);
        predName.setReferenced(testPredicate);
        Expr formula = testPredicate.getBody();
        Command testCommand = new Command(null, predName, testPredicate.label, false, -1, -1, -1, 1, null, null, formula, null);
        testCommand.setAsVariabilizationTest();
        testCommand.setAsGenerated();
        return testCommand;
    }

    private Func generateTestPredicate(Expr initialization, Expr testFormula, List<ExprVar> skolemVariables, Map<Sig, List<ExprVar>> signatureValues, Command cmd) {
        Expr sub = ExprBinary.Op.AND.make(null, null, initialization, testFormula);
        List<ExprVar> varsToDeclare = new LinkedList<>();//new LinkedList<>(skolemVariables);
        for (Entry<Sig, List<ExprVar>> sValues : signatureValues.entrySet()) {
            varsToDeclare.addAll(sValues.getValue());
        }
        List<Decl> signatureDecls = getVariablesDecls(varsToDeclare);
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

    private String getTestName(String from) {
        if (this.testName == null) {
            return "CE_" + from + "_" + generateRandomName(10);
        }
        return this.testName + this.testIndex++;
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

    private Browsable getPredicateOrAssertionCalled(Command command, CompModule context) {
        Browsable predicateOrAssertionCalled = null;
        if (command.nameExpr instanceof ExprVar) {
            String callee = ((ExprVar) command.nameExpr).label;
            for (Func pred : context.getAllFunc()) {
                if (pred.label.replace("this/", "").compareTo(callee) == 0) {
                    predicateOrAssertionCalled = pred;
                    break;
                }
            }
            if (predicateOrAssertionCalled == null) {
                for (Pair<String, Expr> assertion : context.getAllAssertions()) {
                    if (assertion.a.compareTo(callee) == 0) {
                        predicateOrAssertionCalled = assertion.b;
                        break;
                    }
                }
            }
        } else {
            predicateOrAssertionCalled = command.nameExpr;
        }
        return predicateOrAssertionCalled == null?null:(Expr) predicateOrAssertionCalled.clone();
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
                        if (functionResult instanceof IntExpression) {
                            int result = solution.getEvaluator().evaluate((IntExpression) functionResult);
                            ExprConstant intValue = (ExprConstant) ExprConstant.makeNUMBER(result);
                            fieldOverride = ExprBinary.Op.EQUALS.make(null, null, call, intValue);
                        } else if (functionResult instanceof Formula) {
                            boolean result = solution.getEvaluator().evaluate((Formula) functionResult);
                            if (!result)
                                fieldOverride = ExprUnary.Op.NO.make(null, call);
                            else
                                fieldOverride = call;
                        } else if (functionResult instanceof Expression) {
                            TupleSet tset = solution.getEvaluator().evaluate((Expression) functionResult);
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

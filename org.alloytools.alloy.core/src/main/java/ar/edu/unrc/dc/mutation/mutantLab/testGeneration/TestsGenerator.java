package ar.edu.unrc.dc.mutation.mutantLab.testGeneration;

import ar.edu.unrc.dc.mutation.CheatingIsBadMkay;
import ar.edu.unrc.dc.mutation.Cheats;
import ar.edu.unrc.dc.mutation.MutationConfiguration;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.ast.*;
import edu.mit.csail.sdg.ast.Sig.Field;
import edu.mit.csail.sdg.parser.CompModule;
import edu.mit.csail.sdg.translator.A4Solution;
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

import static ar.edu.unrc.dc.mutation.MutationConfiguration.ConfigKey.TEST_GENERATION_MAX_TESTS_PER_COMMAND;
import static ar.edu.unrc.dc.mutation.MutationConfiguration.ConfigKey.TEST_GENERATION_TESTS_PER_STEP;
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

    private static TestsGenerator instance;

    public static TestsGenerator getInstance() {
        if (instance == null) {
            instance = new TestsGenerator();
        }
        return instance;
    }

    private TestsGenerator() {
        testsPerProperty = new TreeMap<>();
    }

    public static int testsToGeneratePerCommand() {
        return (Integer) MutationConfiguration.getInstance().getConfigValue(TEST_GENERATION_MAX_TESTS_PER_COMMAND).orElse(TEST_GENERATION_MAX_TESTS_PER_COMMAND.defaultValue());
    }

    public static int testsPerGeneration() {
        return (Integer) MutationConfiguration.getInstance().getConfigValue(TEST_GENERATION_TESTS_PER_STEP).orElse(TEST_GENERATION_TESTS_PER_STEP.defaultValue());
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
        if (GENERATE_DEBUG_TESTS) {
            testFormula = ExprUnary.Op.NOT.make(null, testFormula);
        } else {
            Expr negateFacts = ExprUnary.Op.NOT.make(null, getFacts(context));
            testFormula = ExprBinary.Op.OR.make(null, null, negateFacts, testFormula);
        }
        Expr initialization = generateInitialization(signatureValues, fieldValues, variablesValues);
        Func testPredicate = generateTestPredicate(initialization, testFormula, skolemVariables, signatureValues, command);
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
        List<Decl> decls = new LinkedList<>();
        Map<String, List<ExprHasName>> variablesPerType = new TreeMap<>();
        Map<String, Type> typeMap = new HashMap<>();
        List<ExprVar> varsToDeclare = new LinkedList<>(skolemVariables);
        for (Entry<Sig, List<ExprVar>> sValues : signatureValues.entrySet()) {
            if (extendsNonBuiltIn(sValues.getKey()))
                continue;
            varsToDeclare.addAll(sValues.getValue());
        }
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
        Expr body = ExprQt.Op.SOME.make(null, null, ConstList.make(decls), sub);
        String from = cmd.nameExpr instanceof ExprVar?((ExprVar) cmd.nameExpr).label:"NO_NAME";
        String name = "CE_" + from + "_" + generateRandomName(10);
        return new Func(null, name, null, null, body);
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

    private Expr generateInitialization(Map<Sig, List<ExprVar>> signaturesValues, Map<Field, List<Expr>> fieldsValues, Map<ExprVar, List<Expr>> variablesValues) {
        List<Expr> sigsInitialization = generateInitialization(signaturesValues);
        List<Expr> fieldsInitialization = generateInitialization(fieldsValues);
        List<Expr> variablesInitialization = generateInitialization(variablesValues);
        List<Expr> initExpressions = new LinkedList<>(sigsInitialization);
        initExpressions.addAll(fieldsInitialization);
        initExpressions.addAll(variablesInitialization);
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

    private Map<Sig, List<ExprVar>> getSignaturesAtoms(A4Solution solution, CompModule context) {
        Map<Relation, TupleSet> counterExampleSignatures = getCounterExampleSignatures(solution);
        Map<Sig, List<ExprVar>> signatureAtoms = new HashMap<>();
        for (Map.Entry<Relation, TupleSet> ceSignature : counterExampleSignatures.entrySet()) {
            Optional<Sig> oSig = nameToSig(ceSignature.getKey(), context);
            if (!oSig.isPresent())
                throw new IllegalStateException("No signature found for " + ceSignature.toString());
            List<ExprVar> sigValues = new LinkedList<>();
            for (Tuple value : ceSignature.getValue()) {
                String varName = internalAtomNotationToAlloyName(value.atom(0).toString());
                ExprVar valueAsVar = ExprVar.make(null, varName, oSig.get().type());
                sigValues.add(valueAsVar);
            }
            signatureAtoms.put(oSig.get(), sigValues);
        }
        return signatureAtoms;
    }

    private Map<Field, List<Expr>> getFieldsValues(A4Solution solution, CompModule context, Map<Sig, List<ExprVar>> signatureValues) {
        Map<Field, List<Expr>> fieldValues = new HashMap<>();
        Map<Relation, TupleSet> counterExampleFields = getCounterExampleFields(solution);
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

    private Map<Relation, TupleSet> getCounterExampleFields(A4Solution solution) {
        return getCounterExampleRelations(solution, false);
    }

    private Map<Relation, TupleSet> getCounterExampleSignatures(A4Solution solution) {
        return getCounterExampleRelations(solution, true);
    }

    private Map<Relation, TupleSet> getCounterExampleRelations(A4Solution solution, boolean sigs) {
        Map<Relation, TupleSet> relations = new HashMap<>();
        Evaluator evaluator = solution.getEvaluator();
        if (evaluator != null) {
            for (Map.Entry<Relation, TupleSet> relation : evaluator.instance().relationTuples().entrySet()) {
                if (relation.getKey().name().trim().isEmpty())
                    continue;
                if (filterRelation(relation.getKey(), solution))
                    continue;
                if (isSignature(relation.getKey()) && !sigs)
                    continue;
                if (isField(relation.getKey()) && sigs)
                    continue;
                relations.put(relation.getKey(), relation.getValue());
            }
        }
        return relations;
    }

}

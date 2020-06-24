package ar.edu.unrc.dc.mutation.mutantLab;

import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.MutationConfiguration;
import ar.edu.unrc.dc.mutation.MutationConfiguration.ConfigKey;
import ar.edu.unrc.dc.mutation.Ops;
import ar.edu.unrc.dc.mutation.util.BlockingCollection;
import ar.edu.unrc.dc.mutation.util.DependencyGraph;
import ar.edu.unrc.dc.mutation.util.RepairReport;
import ar.edu.unrc.dc.mutation.util.TypeChecking;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.ast.Browsable;
import edu.mit.csail.sdg.ast.Command;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.Func;
import edu.mit.csail.sdg.parser.CompModule;

import java.io.IOException;
import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.FileHandler;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;
import java.util.stream.Collectors;

public class MutantLab {

    private static final Logger logger = Logger.getLogger(MutantLab.class.getName());

    static {
        try {
            // This block configure the logger with handler and formatter
            FileHandler fh = new FileHandler("CandidateGeneration.log");
            logger.addHandler(fh);
            SimpleFormatter formatter = new SimpleFormatter();
            fh.setFormatter(formatter);
        } catch (SecurityException | IOException e) {
            e.printStackTrace();
        }
    }

    private static MutantLab instance;
    public synchronized static void initialize(CompModule context, int maxDepth, Ops...ops) {
        if (instance != null)
            throw new IllegalStateException("MutantLab already initialized");
        instance = new MutantLab(context, maxDepth, ops);
    }

    public synchronized static MutantLab getInstance() {
        if (instance == null)
            throw new IllegalStateException("MutantLab is not initialized");
        return instance;
    }

    public synchronized static void destroyInstance() {
        if (instance == null)
            throw new IllegalStateException("MutantLab is not initialized");
        instance = null;
    }

    private final CompModule context;
    private final SortedSet<Ops> ops;
    private boolean searchStarted;
    private final int maxDepth;
    private final int markedExpressions;

    public Boolean timeoutReached = false;
    public Boolean stopRepairProcess = false;

    private final boolean variabilizationSupported;

    //fields for partial repair for multiple bugged and independent functions (preds/fun/assertions)
    private final List<Browsable> affectedFunctionsPredicatesAndAssertions;
    private final boolean partialRepairSupported;
    private Map<Browsable, Pair<Integer,Integer>> indexesPerFPAs;

    private MutantLab(CompModule context, int maxDepth, Ops...ops) {
        if (context == null) throw new IllegalArgumentException("context can't be null");
        if (ops == null) throw new IllegalArgumentException("ops can't be null");
        if (maxDepth <= 0) throw new IllegalArgumentException("maxDepth must be a positive value");
        this.context = context;
        this.ops = new TreeSet<>((o1, o2) -> {
            if (o1.getComplexity() == o2.getComplexity()) {
                return Integer.compare(o1.ordinal(), o2.ordinal());
            } else {
                return Integer.compare(o1.getComplexity(), o2.getComplexity());
            }
        });
        this.ops.addAll(Arrays.asList(ops));
        this.maxDepth = maxDepth;
        searchStarted = false;
        Optional<List<Expr>> initialMarkedExpressions = Variabilization.getInstance().getMarkedExpressions(context);
        markedExpressions = initialMarkedExpressions.map(List::size).orElse(0);
        variabilizationSupported = markedExpressions > 1;
        AtomicBoolean factsAffected = new AtomicBoolean(false);
        affectedFunctionsPredicatesAndAssertions = new LinkedList<>();
        boolean independent = false;
        initialMarkedExpressions.ifPresent(mes -> mes.forEach(e -> RepairReport.getInstance().addMarkedExpression(e)));
        initialMarkedExpressions.ifPresent(mes -> factsAffected.set(isAnyFactBugged(this.context, mes)));
        initialMarkedExpressions.ifPresent(mes -> affectedFunctionsPredicatesAndAssertions.addAll(functionsPredicatesAndAssertionsInvolved(this.context, mes)));
        if (!factsAffected.get() && affectedFunctionsPredicatesAndAssertions.size() > 1) {
            independent = doesFunctionsPredicatesAndAssertionsInvolvedHaveIndependentTests(affectedFunctionsPredicatesAndAssertions);
        }
        partialRepairSupported = !factsAffected.get() && affectedFunctionsPredicatesAndAssertions.size() > 1 && independent;
    }

    public boolean isOperatorEnabled(Ops op) {
        return ops.contains(op);
    }

    public boolean isPartialRepairSupported() {
        return partialRepairSupported;
    }

    public boolean isVariabilizationSupported() {
        return variabilizationSupported;
    }

    private boolean isAnyFactBugged(CompModule context, List<Expr> initialMarkedExpressions) {
        if (context == null)
            throw new IllegalArgumentException("context can't be null");
        if (initialMarkedExpressions == null)
            throw new IllegalArgumentException("marked expressions list can't be null");
        for (Expr me : initialMarkedExpressions) {
            Expr mayorExpr = TypeChecking.getMayorExpression(me);
            for (Pair<String, Expr> namedFact : context.getAllFacts()) {
                if (Browsable.equals(mayorExpr, namedFact.b))
                    return true;
            }
        }
        return false;
    }

    private List<Browsable> functionsPredicatesAndAssertionsInvolved(CompModule context, List<Expr> initialMarkedExpressions) {
        if (context == null)
            throw new IllegalArgumentException("context can't be null");
        if (initialMarkedExpressions == null)
            throw new IllegalArgumentException("marked expressions list can't be null");
        List<Browsable> involved = new LinkedList<>();
        indexesPerFPAs = new HashMap<>();
        int currentIdx = 0;
        start:
        for (Expr me : initialMarkedExpressions) {
            Expr mayorExpr = TypeChecking.getMayorExpression(me);
            currentIdx++;
            for (Pair<String, Expr> namedAssertions : context.getAllAssertions()) {
                if (Browsable.equals(namedAssertions.b, mayorExpr)) {
                    updateInvolvedAndIndexPerFPA(involved, namedAssertions.b, currentIdx);
                    continue start;
                }
            }
            for (Func f : context.getAllFunc()) {
                if (Browsable.equals(f.getBody(), mayorExpr)) {
                    updateInvolvedAndIndexPerFPA(involved, f, currentIdx);
                    continue start;
                }
            }
        }
        return involved;
    }

    private void updateInvolvedAndIndexPerFPA(List<Browsable> involved, Browsable b, int index) {
        if (!involved.contains(b)) {
            involved.add(b);
            indexesPerFPAs.put(b, new Pair<>(index, index));
        } else if (!indexesPerFPAs.containsKey(b)) {
            throw new IllegalStateException("If " + b.toString() +  " is already in the involved list, then it must be in the indexes map");
        } else {
            Pair<Integer, Integer> indexes = indexesPerFPAs.get(b);
            if (index <= indexes.b)
                throw new IllegalStateException("Indexes for " + b.toString() + " are being updated with a index which is less than the current max index: " + indexes.toString() + "(" + index + ")");
            indexesPerFPAs.put(b, new Pair<>(indexes.a, index));
        }
    }

    public Map<Browsable, List<Command>> originalCommandsPerFPA = new HashMap<>();
    private boolean doesFunctionsPredicatesAndAssertionsInvolvedHaveIndependentTests(List<Browsable> involvedFPAs) {
        boolean atLeastOneFPAWithIndependentTests = false;
        boolean allFPAWithIndependentTests = true;
        for (Browsable involvedFPA : involvedFPAs) {
            List<Command> independentCommands = DependencyGraph.getInstance().getDirectIndependentCommandsFor(involvedFPA, involvedFPAs);
            if (independentCommands.isEmpty()) {
                allFPAWithIndependentTests = false;
            } else {
                atLeastOneFPAWithIndependentTests = true;
            }
            originalCommandsPerFPA.put(involvedFPA, independentCommands);
        }
        return partialRepairAllFPANeedTests()?allFPAWithIndependentTests:atLeastOneFPAWithIndependentTests;
    }

    public int getMaxDepth() {
        return maxDepth;
    }

    public int getMarkedExpressions() {
        return markedExpressions;
    }

    private BlockingCollection<Candidate> input;
    private BlockingCollection<Candidate> output;
    MutationTask mutationTask;
    public boolean advance() {
        if (!searchStarted) {
            logger.info("Starting search with timeout (" + candidateQueueTimeout() + ") and threshold (" + generatorTriggerThreshold() + ")");
            output = new BlockingCollection<>(candidateQueueTimeout());
            input = new BlockingCollection<>();
            input.insert(Candidate.original(context));
            mutationTask = new MutationTask(ops, input, output, generatorTriggerThreshold());
            Thread mtThread = new Thread(mutationTask);
            mtThread.start();
            searchStarted = true;
            return true;
        }
        try {
            Optional<Candidate> current = output.getAndAdvance();
            if (timeoutReached) {
                logger.info("TIMEOUT reached...");
                return true;
            }
            if (stopRepairProcess) {
                logger.info("STOP search...");
                return true;
            }
            if (!current.isPresent()) {
                logger.info("Got empty current candidate");
                return false;
            }
            if (current.get() == Candidate.SEARCH_SPACE_EXHAUSTED) {
                logger.info("Search space exhausted");
                return true;
            }
            if (current.get() == Candidate.INVALID) {
                logger.info("Received INVALID candidate, something went wrong on the generation process, stopping search");
                return false;
            }
            if (!current.get().isValid()) {
                logger.info("Received invalid candidate, skipping candidate");
                return advance();
            }
            logger.info("Inserting current to mutation task input channel");
            sendCandidateToInput(current.get());
            return true;
        } catch (InterruptedException e) {
            e.printStackTrace();
            logger.severe("Got interrupted exception when trying to advance");
            return false;
        }
    }

    private void sendCandidateToInput(Candidate current) {
        if (current.hasPartialResults()) { //partial repair
            List<Integer> indexesToBlock = new LinkedList<>();
            Map<Command, Boolean> testsResults = current.getCommandsResults();
            for (Browsable affectedFPA : affectedFunctionsPredicatesAndAssertions) {
                List<Command> independentCommandsForFPA = DependencyGraph.getInstance().getDirectIndependentCommandsFor(affectedFPA, affectedFunctionsPredicatesAndAssertions);
                boolean affectedFPAFixed = independentCommandsForFPA.size() > 0;
                for (Command affectedFPARelatedCommnand : independentCommandsForFPA) {
                    if (!testsResults.containsKey(affectedFPARelatedCommnand) || !testsResults.get(affectedFPARelatedCommnand)) {
                        affectedFPAFixed = false;
                        break;
                    }
                }
                if (affectedFPAFixed) {
                    for (int i = indexesPerFPAs.get(affectedFPA).a; i <= indexesPerFPAs.get(affectedFPA).b; i++) {
                        indexesToBlock.add(i);
                    }
                }
            }
            if (!indexesToBlock.isEmpty()) {
                Candidate partiallyFixedCandidate = current.copy();
                partiallyFixedCandidate.copyMutationsRepsFrom(current);
                for (int indexToBlock : indexesToBlock) {
                    partiallyFixedCandidate.blockIndex(indexToBlock);
                }
                if (goToFirstUnblockedIndex(partiallyFixedCandidate)) {
                    partiallyFixedCandidate.markAsPartialRepair();
                    input.priorityInsert(partiallyFixedCandidate);
                }
            }
        }
        input.insert(current); //the default behaviour must be kept
    }

    private boolean goToFirstUnblockedIndex(Candidate candidate) {
        if (!candidate.hasPartialResults())
            throw new IllegalStateException("Candidate is not partially fixed");
        int unblockedIndexes = 0;
        for (int i = 1; i <= markedExpressions; i++) {
            if (!candidate.isIndexBlocked(i)) {
                candidate.setCurrentMarkedExpression(i);
                unblockedIndexes++;
                break;
            }
        }
        return unblockedIndexes > 0;
    }

    public void timeout() {
        logger.info("Timeout reached (some candidates may still be analyzed)");
        timeoutReached = true;
        mutationTask.softStop();
    }

    public void stopSearch() {
        logger.info("Stopping search");
        if (searchStarted) {
            mutationTask.stop();
            searchStarted = false;
        }
    }

    public void lockCandidateGeneration() {
        mutationTask.getLock().lock();
    }

    public void unlockCandidateGeneration() {
        mutationTask.getLock().unlock();
    }

    public Optional<Candidate> getCurrentCandidate() {
        try {
            return output.current();
        } catch (InterruptedException e) {
            e.printStackTrace();
            return Optional.empty();
        }
    }

    public List<Command> getCommandsToRunFor(Candidate candidate) {
        return getCommandsToRunFor(candidate, false);
    }

    public List<Command> getCommandsToRunFor(Candidate candidate, boolean onlyVariabilizationTests) {
        List<Command> cmds;
        if (useDependencyGraphForChecking()) {
            List<Browsable> relatedAssertionsAndFunctions = candidate.getRelatedAssertionsAndFunctions();
            cmds = new LinkedList<>(DependencyGraph.getInstance().getPriorityCommands(relatedAssertionsAndFunctions));
            cmds.addAll(DependencyGraph.getInstance().getNonPriorityCommands(relatedAssertionsAndFunctions));
        } else {
            cmds = DependencyGraph.getInstance().getAllCommands();
        }
        if (useTestsOnly())
            cmds = cmds.stream().filter(c -> !c.isPerfectOracleTest()).collect(Collectors.toList());
        if (!onlyVariabilizationTests)
            return cmds;
        return cmds.stream().filter(Command::isVariabilizationTest).collect(Collectors.toList());
    }

    public List<Command> getPerfectOracleCommands() {
        return DependencyGraph.getInstance().getAllCommands().stream().filter(Command::isPerfectOracleTest).collect(Collectors.toList());
    }

    public static final Browsable PARTIAL_REPAIR_PRIORITY = Browsable.make(null, (Browsable) null);
    public static final Browsable PARTIAL_REPAIR_NON_PRIORITY = Browsable.make(null, (Browsable) null);
    public Map<Browsable, List<Command>> getCommandsToRunUsingPartialRepairFor(Candidate candidate) {
        logger.info("getCommandsToRunUsingPartialRepairFor\n" + candidate.toString() + "\n");
        logger.info("commands" + "\n" + DependencyGraph.getInstance().getAllCommands().stream().map(Command::toString).collect(Collectors.joining(",")));
        //List<Browsable> partiallyFixedPFAs = partiallyFixedPFAs(candidate);
        Map<Browsable, List<Command>> commands = new HashMap<>();
        List<Command> lastPriorityCommandsToRun = new LinkedList<>();
        List<Command> lastNonPriorityCommandsToRun = new LinkedList<>();
        List<Command> allIndependentTests = new LinkedList<>();
        for (Browsable relatedPFA : candidate.getRelatedAssertionsAndFunctions()) {
            List<Command> independentTests = DependencyGraph.getInstance().getDirectIndependentCommandsFor(relatedPFA);
            independentTests.stream().filter(c -> !allIndependentTests.contains(c)).forEach(allIndependentTests::add);
            commands.put(relatedPFA, independentTests);
            logger.info("Independent commands for " + relatedPFA.toString() + "\n" + independentTests.stream().map(Command::toString).collect(Collectors.joining(",")));
        }
        DependencyGraph.getInstance().getPriorityCommands(candidate.getRelatedAssertionsAndFunctions()).stream().filter(c -> !allIndependentTests.contains(c) && !lastPriorityCommandsToRun.contains(c)).forEach(lastPriorityCommandsToRun::add);
        commands.put(PARTIAL_REPAIR_PRIORITY, lastPriorityCommandsToRun);
        logger.info("Priority commands\n" + lastPriorityCommandsToRun.stream().map(Command::toString).collect(Collectors.joining(",")));
        DependencyGraph.getInstance().getNonPriorityCommands(candidate.getRelatedAssertionsAndFunctions()).stream().filter(c -> !allIndependentTests.contains(c) && !lastNonPriorityCommandsToRun.contains(c)).forEach(lastNonPriorityCommandsToRun::add);
        commands.put(PARTIAL_REPAIR_NON_PRIORITY, lastNonPriorityCommandsToRun);
        logger.info("Non priority commands\n" + lastNonPriorityCommandsToRun.stream().map(Command::toString).collect(Collectors.joining(",")));
        return commands;
    }

    private List<Browsable> partiallyFixedPFAs(Candidate candidate) {
        if (!partialRepairSupported)
            throw new IllegalStateException("This method shouldn't be called when partial repairs are not supprted");
        List<Browsable> partiallyFixedFPAs = new LinkedList<>();
        for (int i = 1; i <= markedExpressions; i++) {
            if (candidate.isIndexBlocked(i)) {
                for (Map.Entry<Browsable, Pair<Integer, Integer>> indexesPFA : indexesPerFPAs.entrySet()) {
                    Pair<Integer, Integer> indexes = indexesPFA.getValue();
                    if (i >= indexes.a && i <= indexes.b && !partiallyFixedFPAs.contains(indexesPFA.getKey())) {
                        partiallyFixedFPAs.add(indexesPFA.getKey());
                    }
                }
            }
        }
        return partiallyFixedFPAs;
    }

    public List<Expr> getModifiedAssertionsFunctionsAndFacts(Candidate candidate) {
        List<Expr> result = new LinkedList<>();
        context.getAllAssertions().forEach(namedAssertion -> candidate.getRelatedAssertionsAndFunctions().stream().filter(b -> Browsable.equals(b, namedAssertion.b)).forEach(b -> result.add((Expr)b)));
        context.getAllFacts().forEach(namedFact -> candidate.getRelatedAssertionsAndFunctions().stream().filter(b -> Browsable.equals(b, namedFact.b)).forEach(b -> result.add((Expr)b)));
        context.getAllFunc().forEach(func -> candidate.getRelatedAssertionsAndFunctions().stream().filter(b -> Browsable.equals(b, func)).forEach(b -> {
                Func f = (Func) b;
                result.add(f.getBody());
        }));
        return result;
    }

    public synchronized boolean applyCandidateToAst(Candidate candidate) {
        ASTMutator astMutator = ASTMutator.getInstance();
        candidate.getMutations().forEach(astMutator::pushNewMutation);
        return astMutator.applyMutations();
    }

    public synchronized List<Mutation> getMutationsAppliedToAst() {
        return ASTMutator.getInstance().appliedMutations();
    }

    public synchronized boolean undoChangesToAst() {
        return ASTMutator.getInstance().undoMutations();
    }

    public void reportCurrentAsInvalid() {
        if (getCurrentCandidate().isPresent()) {
            RepairReport.getInstance().incInvalidCandidates();
            getCurrentCandidate().get().markAsInvalid();
            logger.info("Current combination " + getCurrentCandidate().toString() + " reported as invalid by repair task");
        }
    }

    private static int generatorTriggerThreshold() {
        Optional<Object> configValue = MutationConfiguration.getInstance().getConfigValue(ConfigKey.REPAIR_GENERATOR_TRIGGER_THRESHOLD);
        return configValue.map(o -> (Integer) o).orElse((Integer) ConfigKey.REPAIR_GENERATOR_TRIGGER_THRESHOLD.defaultValue());
    }

    private static long candidateQueueTimeout() {
        Optional<Object> configValue = MutationConfiguration.getInstance().getConfigValue(ConfigKey.REPAIR_GENERATOR_CANDIDATE_GETTER_TIMEOUT);
        return configValue.map(o -> (Long) o).orElse((Long) ConfigKey.REPAIR_GENERATOR_CANDIDATE_GETTER_TIMEOUT.defaultValue());
    }

    private static boolean useDependencyGraphForChecking() {
        Optional<Object> configValue = MutationConfiguration.getInstance().getConfigValue(ConfigKey.REPAIR_USE_DEPENDENCY_GRAPH_FOR_CHECKING);
        return configValue.map(o -> (Boolean) o).orElse((Boolean) ConfigKey.REPAIR_USE_DEPENDENCY_GRAPH_FOR_CHECKING.defaultValue());
    }

    public static boolean useTestsOnly() {
        Optional<Object> configValue = MutationConfiguration.getInstance().getConfigValue(ConfigKey.REPAIR_TESTS_ONLY);
        return configValue.map(o -> (Boolean) o).orElse((Boolean) ConfigKey.REPAIR_TESTS_ONLY.defaultValue());
    }

    public static boolean partialRepairAllFPANeedTests() {
        Optional<Object> configValue = MutationConfiguration.getInstance().getConfigValue(ConfigKey.REPAIR_PARTIAL_REPAIR_REQUIRE_TESTS_FOR_ALL);
        return configValue.map(o -> (Boolean) o).orElse((Boolean) ConfigKey.REPAIR_PARTIAL_REPAIR_REQUIRE_TESTS_FOR_ALL.defaultValue());
    }

}

package ar.edu.unrc.dc.mutation.mutantLab;

import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.MutationConfiguration;
import ar.edu.unrc.dc.mutation.MutationConfiguration.ConfigKey;
import ar.edu.unrc.dc.mutation.Ops;
import ar.edu.unrc.dc.mutation.util.DependencyGraph;
import ar.edu.unrc.dc.mutation.util.RepairReport;
import edu.mit.csail.sdg.ast.Browsable;
import edu.mit.csail.sdg.ast.Command;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.parser.CompModule;

import java.io.IOException;
import java.util.*;
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
    public static void initialize(CompModule context, int maxDepth, Ops...ops) {
        if (instance != null)
            throw new IllegalStateException("MutantLab already initialized");
        instance = new MutantLab(context, maxDepth, ops);
    }

    public static void initialize(CompModule context, Ops...ops) {
        initialize(context, Integer.MAX_VALUE, ops);
    }

    public static MutantLab getInstance() {
        if (instance == null)
            throw new IllegalStateException("MutantLab is not yet initialized");
        return instance;
    }

    private CompModule context;
    private SortedSet<Ops> ops;
    private boolean searchStarted;
    private int maxDepth;
    private int markedExpressions;

    private MutantLab(CompModule context, Ops...ops) {
        this(context, Integer.MAX_VALUE, ops);
    }

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
        initialMarkedExpressions.ifPresent(mes -> mes.forEach(e -> RepairReport.getInstance().addMarkedExpression(e)));
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
            if (!current.isPresent()) {
                logger.info("Got empty current candidate");
                return false;
            }
            if (current.get() == Candidate.INVALID) {
                logger.info("Received INVALID candidate, something went wrong on the generation process, stopping search");
                return false;
            }
            if (!current.get().isValid()) {
                logger.info("Received invalid candidate, skipping candidate");
                return advance();
            }
            if (current.get() == Candidate.STOP) {
                mutationTask.stop();
                logger.info("Received STOP candidate, stopping generation...");
                return false;
            }
            logger.info("Inserting current to mutation task input channel");
            input.insert(current.get());
            return output.current().isPresent();
        } catch (InterruptedException e) {
            e.printStackTrace();
            logger.severe("Got interrupted exception when trying to advance");
            return false;
        }
    }

    public void stopSearch() {
        logger.info("Stopping search");
        if (searchStarted)
            mutationTask.stop();
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
        if (!onlyVariabilizationTests)
            return cmds;
        return cmds.stream().filter(Command::isVariabilizationTest).collect(Collectors.toList());
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

    private int generatorTriggerThreshold() {
        Optional<Object> configValue = MutationConfiguration.getInstance().getConfigValue(ConfigKey.REPAIR_GENERATOR_TRIGGER_THRESHOLD);
        return configValue.map(o -> (Integer) o).orElse((Integer) ConfigKey.REPAIR_GENERATOR_TRIGGER_THRESHOLD.defaultValue());
    }

    private long candidateQueueTimeout() {
        Optional<Object> configValue = MutationConfiguration.getInstance().getConfigValue(ConfigKey.REPAIR_GENERATOR_CANDIDATE_GETTER_TIMEOUT);
        return configValue.map(o -> (Long) o).orElse((Long) ConfigKey.REPAIR_GENERATOR_CANDIDATE_GETTER_TIMEOUT.defaultValue());
    }

    private boolean useDependencyGraphForChecking() {
        Optional<Object> configValue = MutationConfiguration.getInstance().getConfigValue(ConfigKey.REPAIR_USE_DEPENDENCY_GRAPH_FOR_CHECKING);
        return configValue.map(o -> (Boolean) o).orElse((Boolean) ConfigKey.REPAIR_USE_DEPENDENCY_GRAPH_FOR_CHECKING.defaultValue());
    }

}

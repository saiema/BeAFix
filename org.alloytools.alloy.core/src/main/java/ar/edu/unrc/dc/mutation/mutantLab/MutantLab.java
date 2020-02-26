package ar.edu.unrc.dc.mutation.mutantLab;

import ar.edu.unrc.dc.mutation.MutationConfiguration;
import ar.edu.unrc.dc.mutation.MutationConfiguration.ConfigKey;
import ar.edu.unrc.dc.mutation.Ops;
import edu.mit.csail.sdg.parser.CompModule;

import java.io.IOException;
import java.util.*;
import java.util.logging.FileHandler;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

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

    private CompModule context;
    private SortedSet<Ops> ops;
    private boolean searchStarted;
    private int maxDepth;

    public MutantLab(CompModule context, Ops...ops) {
        this(context, Integer.MAX_VALUE, ops);
    }

    public MutantLab(CompModule context, int maxDepth, Ops...ops) {
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
    }

    private BlockingCollection<Candidate> input;
    private BlockingCollection<Candidate> output;
    MutationTask mutationTask;
    public boolean advance() {
        if (!searchStarted) {
            logger.info("Starting search with timeout (" + candidateQueueTimeout() + ") and threshold (" + generatorTriggerThreshold() + ")");
            output = new BlockingCollection<>(candidateQueueTimeout());
            input = new BlockingCollection<>();
            output.insert(Candidate.original(context));
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
            } else if (current.get() == Candidate.INVALID) {
                logger.info("Received INVALID candidate, something went wrong on the generation process, stopping search");
                return false;
            } else if (!current.get().isValid()) {
                logger.info("Received invalid candidate, skipping candidate");
                return advance();
            } else if (current.get().mutations() < maxDepth) {
                logger.info("Inserting current to mutation task input channel");
                input.insert(current.get());
            }
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
        logger.info("locking candidate generation...");
        mutationTask.getLock().lock();
        logger.info("...candidate generation locked");
    }

    public void unlockCandidateGeneration() {
        logger.info("unlocking candidate generation...");
        mutationTask.getLock().unlock();
        logger.info("...candidate generation unlocked");
    }

    public Optional<Candidate> getCurrentCandidate() {
        try {
            return output.current();
        } catch (InterruptedException e) {
            e.printStackTrace();
            return Optional.empty();
        }
    }

    public void reportCurrentAsInvalid() {
        if (getCurrentCandidate().isPresent()) {
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

}

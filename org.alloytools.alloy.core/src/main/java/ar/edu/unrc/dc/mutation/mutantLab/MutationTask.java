package ar.edu.unrc.dc.mutation.mutantLab;

import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.MutationConfiguration;
import ar.edu.unrc.dc.mutation.MutationConfiguration.ConfigKey;
import ar.edu.unrc.dc.mutation.Ops;
import ar.edu.unrc.dc.mutation.util.IrrelevantMutationChecker;
import ar.edu.unrc.dc.mutation.util.RepairReport;
import edu.mit.csail.sdg.parser.CompModule;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.SortedSet;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.FileHandler;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

public class MutationTask implements Runnable {

    private static final Logger logger = Logger.getLogger(MutationTask.class.getName());

    static {
        try {
            // This block configure the logger with handler and formatter
            FileHandler fh = new FileHandler("Generator.log");
            logger.addHandler(fh);
            SimpleFormatter formatter = new SimpleFormatter();
            fh.setFormatter(formatter);
        } catch (SecurityException | IOException e) {
            e.printStackTrace();
        }
    }

    private final BlockingCollection<Candidate> outputChannel;
    private final BlockingCollection<Candidate> inputChannel;
    private final SortedSet<Ops> ops;
    private final int outputMinThreshold;
    private final static int DEFAULT_OUTPUT_MIN_THRESHOLD = 3;
    private final Object thresholdLock = new Object();
    private final MutantsHashes mutantsHashes;
    private final Lock lock = new ReentrantLock();
    public Lock getLock() {
        return lock;
    }
    private long thresholdRetryTimeout = 10000L;

    public MutationTask(SortedSet<Ops> ops, BlockingCollection<Candidate> inputChannel, BlockingCollection<Candidate> outputChannel) {
        this(ops, inputChannel, outputChannel, DEFAULT_OUTPUT_MIN_THRESHOLD);
    }

    public MutationTask(SortedSet<Ops> ops, BlockingCollection<Candidate> inputChannel, BlockingCollection<Candidate> outputChannel, int outputMinThreshold) {
        if (inputChannel == null)
            throw new IllegalArgumentException("inputChannel can't be null");
        if (outputChannel == null)
            throw new IllegalArgumentException("outputChannel can't be null");
        if (ops == null)
            throw new IllegalArgumentException("ops can't be null");
        this.inputChannel = inputChannel;
        this.outputChannel = outputChannel;
        this.ops = ops;
        this.outputMinThreshold = outputMinThreshold;
        mutantsHashes = new MutantsHashes();
        outputChannel.addDecreaseAssociatedObject(thresholdLock);
    }

    private boolean run = true;
    @Override
    public void run() {
        while (run) {
            try {
                waitForOutputThreshold();
                if (run) {
                    Optional<Candidate> current = inputChannel.getAndAdvance();
                    lock.lock();
                    if (run) {
                        current.ifPresent(this::checkAndGenerateNewCandidates);
                        if (run && mutationsAdded == 0 && outputChannel.isEmpty() && inputChannel.isEmpty()) {
                            outputChannel.insert(Candidate.STOP);
                            MutantLab.getInstance().stopSearch();
                        }
                    }
                    lock.unlock();
                }
            } catch (Exception e) {
                StringWriter sw = new StringWriter();
                e.printStackTrace(new PrintWriter(sw));
                logger.info("Exception in run method\n"+sw.toString());
                lock.unlock();
                return;
            }
        }
    }

    public synchronized void stop() {
        run = false;
        synchronized (thresholdLock) {
            thresholdLock.notify();
        }
        inputChannel.unlockAllWaitingThreads();
    }

    private void waitForOutputThreshold() throws InterruptedException {
        while (outputChannel.size() > outputMinThreshold) {
            synchronized (thresholdLock) {
                thresholdLock.wait(thresholdRetryTimeout);
            }
        }
    }

    private int mutationsAdded = 0;
    private void checkAndGenerateNewCandidates(Candidate from) {
        mutationsAdded = 0;
        logger.info("***Variabilization check started***");
        if (!from.isValid()) {
            logger.info("candidate was invalid, returning");
            return;
        }
        if (from.isLast()) { //TODO: check if this verification is needed, this method should not be getting called with a last candidate
            logger.info("candidate was last, sending stop signal");
            outputChannel.insert(Candidate.STOP);
            return;
        }
        //=============VARIABILIZATION=============
        logger.info("Current candidate:\n" + from.toString());
        if (variabilizationCheck(from)) {
            logger.info("variabilization check SUCCEEDED");
            Candidate nextMutationSpotCandidate = from.copy();
            nextMutationSpotCandidate.currentMarkedExpressionInc();
            if (from.isFirst()) {
                logger.info("Sending only next index candidate:\n" + nextMutationSpotCandidate.toString());
                outputChannel.insert(nextMutationSpotCandidate);
                return;
            } else if (!nextMutationSpotCandidate.isLast()){
                logger.info("Sending mutants of:\n" + nextMutationSpotCandidate.toString());
                generateMutationsFor(nextMutationSpotCandidate);
            }
        } else {
            logger.info("variabilization check FAILED");
            if (from.isFirst()) {
                logger.info("current candidate had index 0, sending CANT REPAIR signal");
                outputChannel.insert(Candidate.CANT_REPAIR);
                return;
            }
        }
        if (!from.isFirst()) {
            logger.info("Sending mutants of:\n" + from.toString());
            generateMutationsFor(from);
        }
        logger.info("***Variabilization check finished***");
        //=========================================
    }

    private void generateMutationsFor(Candidate from) {
        if (from.mutationsForCurrentIndex() >= MutantLab.getInstance().getMaxDepth())
            return;
        CompModule context = from.getContext();
        if (!MutantLab.getInstance().applyCandidateToAst(from)) {
            outputChannel.insert(Candidate.INVALID);
            return;
        }
        AtomicReference<Candidate> updatedFrom = new AtomicReference<>(Candidate.original(context));
        updatedFrom.get().setCurrentMarkedExpression(from.getCurrentMarkedExpression());
        AtomicReference<Candidate> lastCandidate = new AtomicReference<>(updatedFrom.get());
        MutantLab.getInstance().getMutationsAppliedToAst().forEach(m -> {
            updatedFrom.set(Candidate.mutantFromCandidate(lastCandidate.get(), m));
            lastCandidate.set(updatedFrom.get());
        });
        updatedFrom.get().copyMutationsFrom(from);
        Variabilization.getInstance().blockAllButCurrent(from);
        List<Candidate> newCandidates = new LinkedList<>();
        for (Ops o : ops) {
            if (!o.isImplemented())
                continue;
            Optional<List<Mutation>> mutationsOp = o.getOperator(context).getMutations();
            mutationsOp.ifPresent(mutations -> mutations.forEach(m -> {
                Candidate newCandidate = Candidate.mutantFromCandidate(updatedFrom.get(), m);
                newCandidate.increaseMutations(newCandidate.getCurrentMarkedExpression());
                RepairReport.getInstance().incGeneratedCandidates();
                if (mutantsHashes.add(newCandidate)) {
                    Optional<Mutation> lastMutation = newCandidate.getLastMutation();
                    if (lastMutation.isPresent()) {
                        if (IrrelevantMutationChecker.isIrrelevant(lastMutation.get())) {
                            Mutation.overrideFullToString(true);
                            logger.info("Irrelevant mutation detected: " + lastMutation.get().toString());
                            Mutation.overrideFullToString(false);
                            RepairReport.getInstance().incIrrelevantMutationsSkipped();
                        } else {
                            newCandidate.clearMutatedStatus();
                            RepairReport.getInstance().addMutations(1, from.getCurrentMarkedExpression());
                            if (from.isPartialRepair()) {
                                newCandidate.markAsPartialRepair();
                                for (int i = 1; i <= MutantLab.getInstance().getMarkedExpressions(); i++) {
                                    if (from.isIndexBlocked(i))
                                        newCandidate.blockIndex(i);
                                }
                            }
                            newCandidates.add(newCandidate);
                        }
                    } else {
                        throw new IllegalStateException("A new candidate was created with no mutations " + newCandidate.toString());
                    }
                }
            }));
        }
        if (!MutantLab.getInstance().undoChangesToAst())
            outputChannel.insert(Candidate.INVALID);
        else if (!newCandidates.isEmpty()) {
            RepairReport.getInstance().incGenerations(from.getCurrentMarkedExpression());
            mutationsAdded += newCandidates.size();
            outputChannel.insertBulk(newCandidates);
        }
    }

    private boolean variabilizationCheck(Candidate candidate) {
        if (!useVariabilization()) {
            logger.info("variabilization check is disabled, returning true");
            return true;
        }
        return Variabilization.getInstance().variabilizationCheck(candidate, MutantLab.getInstance().getCommandsToRunFor(candidate, true), logger);
    }

    private boolean useVariabilization() {
        Optional<Object> configValue = MutationConfiguration.getInstance().getConfigValue(ConfigKey.REPAIR_VARIABILIZATION);
        return configValue.map(o -> (Boolean) o).orElse((Boolean) ConfigKey.REPAIR_VARIABILIZATION.defaultValue());
    }


}

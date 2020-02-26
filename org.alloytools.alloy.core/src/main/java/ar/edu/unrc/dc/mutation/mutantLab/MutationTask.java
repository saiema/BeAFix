package ar.edu.unrc.dc.mutation.mutantLab;

import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.Ops;
import edu.mit.csail.sdg.parser.CompModule;

import java.io.IOException;
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

    private BlockingCollection<Candidate> outputChannel;
    private BlockingCollection<Candidate> inputChannel;
    private SortedSet<Ops> ops;
    private final int outputMinThreshold;
    private final static int DEFAULT_OUTPUT_MIN_THRESHOLD = 3;
    private final Object thresholdLock = new Object();
    private MutantsHashes mutantsHashes;
    private Lock lock = new ReentrantLock();
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
                    current.ifPresent(this::generateMutations);
                    lock.unlock();
                }
            } catch (Exception e) {
                e.printStackTrace();
                lock.unlock();
                return;
            }
        }
    }

    public void stop() {
        run = false;
        synchronized (thresholdLock) {
            thresholdLock.notify();
        }
    }

    private void waitForOutputThreshold() throws InterruptedException {
        logger.info("Checking threshold");
        while (outputChannel.size() > outputMinThreshold) {
            synchronized (thresholdLock) {
                thresholdLock.wait(thresholdRetryTimeout);
            }
        }
        logger.info("Threshold reached");
    }

    private void generateMutations(Candidate from) {
        if (!from.isValid())
            return;
        CompModule context = from.getContext();
        ASTMutator astMutator = ASTMutator.getInstance();
        from.getMutations().forEach(astMutator::pushNewMutation);
        if (!astMutator.applyMutations()) {
            outputChannel.insert(Candidate.INVALID);
            return;
        }
        AtomicReference<Candidate> updatedFrom = new AtomicReference<>(Candidate.original(context));
        AtomicReference<Candidate> lastCandidate = new AtomicReference<>(updatedFrom.get());
        astMutator.appliedMutations().forEach(m -> {
            updatedFrom.set(Candidate.mutantFromCandidate(lastCandidate.get(), m));
            lastCandidate.set(updatedFrom.get());
        });
        List<Candidate> newCandidates = new LinkedList<>();
        for (Ops o : ops) {
            if (!o.isImplemented())
                continue;
            Optional<List<Mutation>> mutationsOp = o.getOperator(context).getMutations();
            mutationsOp.ifPresent(mutations -> mutations.forEach(m -> {
                Candidate newCandidate = Candidate.mutantFromCandidate(updatedFrom.get(), m);
                //outputChannel.insert(newCandidate);
                if (mutantsHashes.add(newCandidate))
                    newCandidates.add(newCandidate);
            }));
        }
        if (!astMutator.undoMutations())
            outputChannel.insert(Candidate.INVALID);
        else
            outputChannel.insertBulk(newCandidates);
    }


}

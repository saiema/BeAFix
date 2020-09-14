package ar.edu.unrc.dc.mutation.mutantLab;

import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.Ops;
import ar.edu.unrc.dc.mutation.util.CandidateChannel;
import ar.edu.unrc.dc.mutation.util.IrrelevantMutationChecker;
import ar.edu.unrc.dc.mutation.util.MutantsHashes;
import ar.edu.unrc.dc.mutation.util.RepairReport;
import edu.mit.csail.sdg.parser.CompModule;

import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.SortedSet;
import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.FileHandler;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

public class CandidateGenerator  {

    private static final Logger logger = Logger.getLogger(CandidateGenerator.class.getName());

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

    private final CandidateChannel outputChannel;
    private final CandidateChannel inputChannel;
    private final SortedSet<Ops> ops;
    private final MutantsHashes mutantsHashes;

    public CandidateGenerator(SortedSet<Ops> ops, CandidateChannel inputChannel, CandidateChannel outputChannel) {
        if (inputChannel == null)
            throw new IllegalArgumentException("inputChannel can't be null");
        if (outputChannel == null)
            throw new IllegalArgumentException("outputChannel can't be null");
        if (ops == null)
            throw new IllegalArgumentException("ops can't be null");
        this.inputChannel = inputChannel;
        this.outputChannel = outputChannel;
        this.ops = ops;
        mutantsHashes = new MutantsHashes();
    }

    private int mutationsAdded = 0;


    public void generateNewCandidates() {
        mutationsAdded = 0;
        generateNewCandidates_impl();
    }

    private void generateNewCandidates_impl() {
        boolean generate = true;
        while (generate) {
            Optional<Candidate> fromOp = inputChannel.next();
            if (!fromOp.isPresent()) {
                outputChannel.addToPriorityChannel(Candidate.SEARCH_SPACE_EXHAUSTED);
                return;
            }
            Candidate from = fromOp.get();
            if (!from.isValid()) {
                logger.info("candidate was invalid, ignoring...");
                continue;
            }
            if (from.isLast()) {
                logger.info("candidate was last, ignoring...");
                continue;
            }
            //=============VARIABILIZATION=============
            logger.info("Current candidate:\n" + from.toString());
            if (variabilizationCheck(from)) {
                logger.info("variabilization check SUCCEEDED");
                Candidate nextMutationSpotCandidate = from.copy();
                nextMutationSpotCandidate.copyAllMutationsRepsFrom(from);
                nextMutationSpotCandidate.currentMarkedExpressionInc();
                logger.info("Sending next index candidate:\n" + nextMutationSpotCandidate.toString());
                if (!nextMutationSpotCandidate.isLast()) {
                    if (from.isPartialRepair())
                        outputChannel.addToPriorityChannel(nextMutationSpotCandidate);
                    else
                        outputChannel.add(nextMutationSpotCandidate);
                }
                if (!nextMutationSpotCandidate.isLast()) {
                    logger.info("Sending mutants of:\n" + nextMutationSpotCandidate.toString());
                    generateMutationsFor(nextMutationSpotCandidate);
                }
            } else {
                logger.info("variabilization check FAILED");
                if (from.isFirst()) {
                    logger.info("current candidate had index 0, sending CANT REPAIR signal");
                    outputChannel.addToPriorityChannel(Candidate.CANT_REPAIR);
                    return;
                }
            }
            //from.clearCommandsResults();
            if (!from.isFirst()) {
                logger.info("Sending mutants of:\n" + from.toString());
                generateMutationsFor(from);
            }
            if (mutationsAdded == 0 && outputChannel.isEmpty() && inputChannel.isEmpty()) {
                outputChannel.addToPriorityChannel(Candidate.SEARCH_SPACE_EXHAUSTED);
            }
            generate = (mutationsAdded == 0 && outputChannel.isEmpty() && !inputChannel.isEmpty());
            //=========================================
        }
    }

    private void generateMutationsFor(Candidate from) {
        RepairReport.getInstance().generationClockStart();
        generateMutationsFor_impl(from);
        RepairReport.getInstance().generationClockEnd();
    }

    private void generateMutationsFor_impl(Candidate from) {
        if (from.mutationsForCurrentIndex() >= MutantLab.getInstance().getMaxDepth())
            return;
        CompModule context = from.getContext();
        if (!MutantLab.getInstance().applyCandidateToAst(from)) {
            outputChannel.addToPriorityChannel(Candidate.GENERATION_FAILED);
            return;
        }
        boolean fromPriority = from.isPartialRepair() && Pruning.getInstance().partialPruning();
//        Optional<Mutation> fromLastMutation = from.getLastMutation();
        AtomicReference<Candidate> updatedFrom = new AtomicReference<>(Candidate.original(context));
        updatedFrom.get().setCurrentMarkedExpression(from.getCurrentMarkedExpression());
        AtomicReference<Candidate> lastCandidate = new AtomicReference<>(updatedFrom.get());
        MutantLab.getInstance().getMutationsAppliedToAst().forEach(m -> {
            updatedFrom.set(Candidate.mutantFromCandidate(lastCandidate.get(), m));
            lastCandidate.set(updatedFrom.get());
        });
        updatedFrom.get().copyMutationsFrom(from);
        Pruning.getInstance().blockAllButCurrent(from);
        List<Candidate> newCandidates = new LinkedList<>();
        for (Ops o : ops) {
            if (!o.isImplemented())
                continue;
            Optional<List<Mutation>> mutationsOp = o.getOperator(context).getMutations();
            mutationsOp.ifPresent(mutations -> mutations.forEach(m -> {
                Candidate newCandidate = Candidate.mutantFromCandidate(updatedFrom.get(), m);
                newCandidate.copyAllMutationsRepsFrom(from);
//                fromLastMutation.ifPresent(newCandidate::addMutationRep);
                newCandidate.addMutationRep(m);
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
                } else {
                    RepairReport.getInstance().incRepeatedCandidates();
                }
            }));
        }
        if (!MutantLab.getInstance().undoChangesToAst())
            outputChannel.addToPriorityChannel(Candidate.GENERATION_FAILED);
        else if (!newCandidates.isEmpty()) {
            RepairReport.getInstance().incGenerations(from.getCurrentMarkedExpression());
            mutationsAdded += newCandidates.size();
            //outputChannel.addAll(newCandidates);
            sendNewCandidatesToOutput(newCandidates, fromPriority);
        }
    }

    private void sendNewCandidatesToOutput(List<Candidate> nextGeneration, boolean priority) {
        if (priority)
            nextGeneration.forEach(outputChannel::addToPriorityChannel);
        else
            outputChannel.addAll(nextGeneration);
    }

    private boolean variabilizationCheck(Candidate candidate) {
        if (!Pruning.getInstance().useVariabilization() && !Pruning.getInstance().partialPruning()) {
            logger.info("variabilization check and partial pruning are disabled, returning true");
            return true;
        }
        boolean variabilization = true;
        if (Pruning.getInstance().partialPruning()) {
            RepairReport.getInstance().variabilizationClockStart();
            variabilization = Pruning.getInstance().independentCommandsVariabilizationCheck(candidate, logger);
            RepairReport.getInstance().variabilizationClockEnd();
        }
        if (variabilization && Pruning.getInstance().useVariabilization()) {
            RepairReport.getInstance().variabilizationClockStart();
            variabilization = Pruning.getInstance().variabilizationCheck(candidate, MutantLab.getInstance().getCommandsToRunFor(candidate, true), logger);
            RepairReport.getInstance().variabilizationClockEnd();
        }
        return variabilization;
    }


}

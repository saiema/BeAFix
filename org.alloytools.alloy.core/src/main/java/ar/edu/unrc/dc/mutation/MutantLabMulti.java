package ar.edu.unrc.dc.mutation;

import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.parser.CompModule;

import java.io.IOException;
import java.util.*;
import java.util.logging.FileHandler;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;
import java.util.Map.Entry;

public class MutantLabMulti {


    private static final Logger logger = Logger.getLogger(MutantLabMulti.class.getName());

    static {
        try {
            // This block configure the logger with handler and formatter
            FileHandler fh = new FileHandler("MutantLabMulti.log");
            logger.addHandler(fh);
            SimpleFormatter formatter = new SimpleFormatter();
            fh.setFormatter(formatter);
        } catch (SecurityException | IOException e) {
            e.printStackTrace();
        }
    }

    private CompModule context; //Source that the Mutant Laboratory is associated to.
    //Iterator that iterates over a all possible combinations of mutations
    private Candidate candidate;
    private Ops[] ops;
    private ConstList<Mutation> mutations;
    private long candidateCount;
    private int maxCombinations;



    /**
     * Constructor tha create a new  MutantLaboratory for a compmodule generating all possible mutations for its marked expr
     */
    public MutantLabMulti(CompModule context, Ops...ops) {
        this(context, Integer.MAX_VALUE, ops);
    }

    public MutantLabMulti(CompModule context, int maxCombinations, Ops...ops) {
        if (context == null) throw new IllegalArgumentException("context can't be null");
        if (ops == null) throw new IllegalArgumentException("ops can't be null");
        if (maxCombinations <= 0) throw new IllegalArgumentException("maxCombinations must be a positive value");
        this.context = context;
        this.ops = ops;
        this.maxCombinations = maxCombinations;
        generateMutations();
        candidateCount = mutations.size();
    }

    private Queue<BitSet> combinations;
    private List<BitSet> invalidCombinations;
    public boolean advance() {
        if (hasNext()) {
            if (combinations == null) {
                combinations = new LinkedList<>();
                combinations.add(new BitSet(mutations.size()));
                invalidCombinations = new LinkedList<>();
            }
            BitSet current = combinations.poll();
            if (current == null) throw new IllegalStateException("there shouldn't be a null combination in the queue");
            if (invalidCombinations.contains(current)) {
                logger.info("Skipped invalid combination: " + current.toString());
                return advance();
            }
            if (!isValid(current)) {
                logger.info("Skipped combination that includes and invalid combination: " + current.toString());
                return advance();
            }
            if (current.cardinality() < maxCombinations) {
                for (int i = current.nextClearBit(0); i < mutations.size(); i = current.nextClearBit(i + 1)) {
                    if (i == -1)
                        break;
                    BitSet newComb = (BitSet) current.clone();
                    newComb.set(i);
                    if (!combinations.contains(newComb))
                        combinations.add(newComb);
                }
            }
            if (current.isEmpty())
                return advance();
            candidate = new Candidate(mutations, current);
            logger.info(candidate.toString() + "\n");
            if (candidate.isValid())
                return true;
            else {
                logger.info("Invalid candidate included the following mutations:");
                for (int i = candidate.activeMutations.nextSetBit(0); i >= 0; i = candidate.activeMutations.nextSetBit(i+1)) {
                    // operate on index i here
                    if (i == Integer.MAX_VALUE) {
                        break; // or (i+1) would overflow
                    }
                    logger.info(candidate.mutations.get(i).toString());
                }
                invalidCombinations.add(current);
                logger.info("Added current combination to invalid ones: " + current.toString());
            }
            if (hasNext()) return advance();
        }
        candidate = null;
        return false;
    }

    private boolean isValid(BitSet combination) {
        for (BitSet invalid : invalidCombinations) {
            BitSet combClone = (BitSet) combination.clone();
            combClone.and(invalid);
            if (combClone.equals(invalid))
                return false;
        }
        return true;
    }

    public long mutantCount (){return candidateCount;}

    public String getCurrentMutationsStr(){
       if (candidate == null) throw new IllegalStateException("No available candidate (method advance() returned false)");
       return candidate.toString();
    }

    public Optional<Expr> getMutation(Expr x) {
        if (candidate == null) throw new IllegalStateException("No available candidate (method advance() returned false)");
        return candidate.getMutatedExpr(x);
    }

    private void generateMutations() {
        List<Mutation> generatedMutations = new LinkedList<>();
        for (Expr original : context.markedEprsToMutate) {
            for (Ops o : ops) {
                Mutator mutOperator = o.getOperator(context);
                Optional<List<Mutation>> opMutations = mutOperator.getMutations(original);
                opMutations.ifPresent(generatedMutations::addAll);
            }
        }
        mutations = ConstList.make(generatedMutations);
    }

    private boolean hasNext() {
        return combinations == null || !combinations.isEmpty();
    }

    private static class Candidate {

        private BitSet activeMutations;
        private ConstList<Mutation> mutations;
        private List<Mutation> processedMutations;
        private Map<Expr, Expr> originalToMutant;
        private boolean isValid;

        public Candidate(ConstList<Mutation> mutations, BitSet activeMutations) {
            this.mutations = mutations;
            this.activeMutations = activeMutations;
            isValid = true;
            processMutations();
            createMapping();
        }

        public boolean isValid() {
            return isValid;
        }

        public Optional<Expr> getMutatedExpr(Expr x) {
            if (originalToMutant.containsKey(x)) {
                return Optional.of(originalToMutant.get(x));
            }
            return Optional.empty();
        }

        @Override
        public String toString() {
            if (!isValid) return "INVALID";
            StringBuilder sb = new StringBuilder();
            for (Mutation m : processedMutations) {
                sb.append("Line: ").append(m.original().pos.y).append(" : ");
                sb.append(m.toString()).append("\n");
            }
            return sb.toString();
        }

        private void processMutations() {
            Map<Expr, List<Mutation>> mutationsToUseByMayorExpr = new HashMap<>();
            for (int i = activeMutations.nextSetBit(0); i >= 0; i = activeMutations.nextSetBit(i+1)) {
                // operate on index i here
                if (i == Integer.MAX_VALUE) {
                    break; // or (i+1) would overflow
                }
                Mutation m = mutations.get(i);
                Expr mayorExpr = m.getMayorAffectedExpression();
                List<Mutation> muts;
                if (mutationsToUseByMayorExpr.containsKey(mayorExpr)) {
                    muts = mutationsToUseByMayorExpr.get(mayorExpr);
                } else {
                    muts = new LinkedList<>();
                    mutationsToUseByMayorExpr.put(mayorExpr, muts);
                }
                muts.add(m);
            }
            mergeMutations(mutationsToUseByMayorExpr);
        }

        private void mergeMutations(Map<Expr, List<Mutation>> mutationsToUseByMayorExpr) {
            boolean modified;
            processedMutations = new LinkedList<>();
            do {
                modified = false;
                mainProcessing:
                for (Entry<Expr, List<Mutation>> mutationsPerExpr : mutationsToUseByMayorExpr.entrySet()) {
                    List<Mutation> muts = mutationsPerExpr.getValue(); //merging will modify this list
                    if (muts.size() > 1) {
                        //we need to check if there is two mutations that need to be merged
                        //we also need to check if there are pairs of mutation which are incompatible
                        for (Mutation m1 : muts) {
                            for (Mutation m2 : muts) {
                                if (System.identityHashCode(m1) == System.identityHashCode(m2))
                                    continue; //they are the same mutation, skip
                                if (Mutation.incompatible(m1, m2)) {
                                    isValid = false;
                                    return; //m1 and m2 not only cannot be merged, they cannot be applied to the same mayor expression
                                }
                                if (Mutation.compatible(m1, m2)) {
                                    //m1 and m2 should be merged into mergedMutation, and m1 and m2 should be removed from muts
                                    logger.info("Compatible mutations detected: " + "\n" + m1.toString() + "\n" + m2.toString() + "\n");
                                    Mutation mergedMutation = m1.merge(m2).compress();
                                    logger.info("Merged and compressed: " + "\n" + mergedMutation.toString() + "\n");
                                    muts.remove(m1);
                                    muts.remove(m2);
                                    muts.add(mergedMutation);
                                    modified = true;
                                    break mainProcessing; //now we need to restart the processing
                                }
                            }
                        }
                    }
                    processedMutations.addAll(muts); //if we didn't merge any mutations, we now add those to the processed mutations
                }
            } while(modified);
        }

        private void createMapping() {
            originalToMutant = new HashMap<>();
            for (Mutation m : processedMutations) {
                originalToMutant.put(m.original(), m.mutant());
            }
        }

    }

}



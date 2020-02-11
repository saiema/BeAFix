package ar.edu.unrc.dc.mutation;

import ar.edu.unrc.dc.mutation.util.ContextExpressionExtractor;
import ar.edu.unrc.dc.mutation.util.TypeChecking;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.alloy4.Triplet;
import edu.mit.csail.sdg.ast.Browsable;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.Func;
import edu.mit.csail.sdg.parser.CompModule;

import java.io.IOException;
import java.util.*;
import java.util.Map.Entry;
import java.util.logging.FileHandler;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;
import java.util.stream.Collectors;

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
    private SortedSet<Ops> ops;
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
        this.ops = new TreeSet<>((o1, o2) -> {
            if (o1.getComplexity() == o2.getComplexity()) {
                return Integer.compare(o1.ordinal(), o2.ordinal());
            } else {
                return Integer.compare(o1.getComplexity(), o2.getComplexity());
            }
        });
        this.ops.addAll(Arrays.asList(ops));
        this.maxCombinations = maxCombinations;
        generateMutations();
        candidateCount = mutations.size();
    }

    private Queue<BitSet> combinations;
    private List<BitSet> invalidCombinations;
    private BitSet current;
    public boolean advance() {
        if (hasNext()) {
            if (combinations == null) {
                combinations = new LinkedList<>();
                combinations.add(new BitSet(mutations.size()));
                invalidCombinations = new LinkedList<>();
            }
            current = combinations.poll();
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
        current = null;
        return false;
    }

    public void reportCurrentAsInvalid() {
        if (current != null && !current.isEmpty()) {
            if (!invalidCombinations.contains(current)) {
                invalidCombinations.add(current);
                logger.info("Current combination " + current.toString() + " reported as invalid by repair task");
            }
        }
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

    /**
     * Returns a list of triplet (place & OP, orig expr, mutation expr) of the current mutation
     * @return
     */
    public List<Triplet<String,String,String>>  getCurrentMutationsInfo(){
        if (candidate == null) throw new IllegalStateException("No available candidate (method advance() returned false)");
        ArrayList<Triplet<String,String,String>> l = new ArrayList<Triplet<String,String,String>>();
        if (!candidate.isValid) {
            Triplet t =  new Triplet ("INVALID","","");
            l.add(t);
            return l;
        }
        else {
            for (Mutation m : candidate.processedMutations) {
                Triplet<String, String, String> t = new Triplet("Line "+m.original().pos.y+" <"+ m.operator()+"> ", m.original().toString(),m.mutant().toString());
                l.add(t);
            }
            return l;
        }
    }

    public Optional<Expr> getMutation(Expr x) {
        if (candidate == null) throw new IllegalStateException("No available candidate (method advance() returned false)");
        return candidate.getMutatedExpr(x);
    }

    private void generateMutations() {
        List<Mutation> generatedMutations = new LinkedList<>();
        for (Expr original : context.markedEprsToMutate) {
            for (Ops o : ops) {
                if (!o.isImplemented()) continue;
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

    public void markAsAlreadyMutated(Expr x) {
        if (candidate == null)
            throw new IllegalStateException("There is no current candidate");
        candidate.markedAsAlreadyMutated(x);
    }

    public void clearMutatedStatus() {
        if (candidate == null)
            throw new IllegalStateException("There is no current candidate");
        candidate.clearMutatedStatus();
    }

    public List<Browsable> getRelatedAssertionsAndFunctions() {
        if (candidate == null)
            throw new IllegalStateException("There is no current candidate");
        List<Browsable> relatedAssertionsAndFunctions = new LinkedList<>();
        for (Expr x : candidate.originalToMutant.keySet()) {
            Optional<Func> contFunc = ContextExpressionExtractor.getContainerFunc(x);
            if (contFunc.isPresent() && !relatedAssertionsAndFunctions.contains(contFunc.get())) {
                relatedAssertionsAndFunctions.add(contFunc.get());
            } else {
                Expr mayorExpr = TypeChecking.getMayorExpression(x);
                Optional<Pair<String, Expr>> namedAssertion = context.getAllAssertions().stream().filter(na -> {
                    Expr body = na.b;
                    return body.toString().compareTo(mayorExpr.toString()) == 0;
                }).findFirst();
                if (namedAssertion.isPresent() && !relatedAssertionsAndFunctions.contains(namedAssertion.get().b))
                    relatedAssertionsAndFunctions.add(namedAssertion.get().b);
            }
        }
        return relatedAssertionsAndFunctions;
    }

    private static class Candidate {

        private BitSet activeMutations;
        private ConstList<Mutation> mutations;
        private List<Mutation> processedMutations;
        private Map<Expr, Expr> originalToMutant;
        private Map<Expr, Boolean> isAlreadyMutated;
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
            if (originalToMutant.containsKey(x) && !isAlreadyMutated.get(x)) {
                return Optional.of(originalToMutant.get(x));
            }
            return Optional.empty();
        }

        public void markedAsAlreadyMutated(Expr x) {
            if (isAlreadyMutated.containsKey(x))
                isAlreadyMutated.put(x, Boolean.TRUE);
            else
                throw new IllegalArgumentException("This expression has no mutation associated");
        }

        public void clearMutatedStatus() {
            Set<Expr> expressions = mutations.stream().map(Mutation::original).collect(Collectors.toSet());
            for (Expr x : expressions) {
                isAlreadyMutated.put(x, Boolean.FALSE);
            }
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
                    muts.forEach(x -> { //if we didn't merge any mutations, we now add those to the processed mutations
                        if (!processedMutations.contains(x))
                            processedMutations.add(x);
                    });
                }
            } while(modified);
        }

        private void createMapping() {
            originalToMutant = new HashMap<>();
            isAlreadyMutated = new HashMap<>();
            for (Mutation m : processedMutations) {
                originalToMutant.put(m.original(), m.mutant());
                isAlreadyMutated.put(m.original(), Boolean.FALSE);
            }
        }

    }

}



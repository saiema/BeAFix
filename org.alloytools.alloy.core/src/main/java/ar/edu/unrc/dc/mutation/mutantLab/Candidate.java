package ar.edu.unrc.dc.mutation.mutantLab;

import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.Ops;
import ar.edu.unrc.dc.mutation.util.ContextExpressionExtractor;
import ar.edu.unrc.dc.mutation.util.TypeChecking;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.alloy4.Triplet;
import edu.mit.csail.sdg.ast.*;
import edu.mit.csail.sdg.parser.CompModule;

import java.util.*;

public class Candidate {

    //signals
    public static final Candidate INVALID;
    public static final Candidate CANT_REPAIR;

    private Candidate parent;
    private Mutation mutation;
    private final CompModule context;
    List<Browsable> relatedAssertionsAndFunctions;
    private boolean isAlreadyMutated;
    private boolean markedAsInvalid;
    private int markedExpressions;
    private int currentMarkedExpression;
    private int[] mutationsPerIndex;
    private boolean[] blockedIndexes;
    private boolean isFromFact;
    private Map<Command, Boolean> commandsResults;
    private boolean hasPartialResults;
    private boolean isPartialRepair;

    static {
        Candidate invalid = new Candidate(null);
        invalid.markAsInvalid();
        INVALID = invalid;
        CANT_REPAIR = new Candidate(null);
    }

    private Candidate(CompModule context) {
        this(null, null, context);
    }

    private Candidate(Candidate parent, Mutation mutation, CompModule context) {
        isFromFact = false;
        isAlreadyMutated = false;
        markedAsInvalid = false;
        this.mutation = mutation;
        this.parent = parent;
        this.context = context;
        if (mutation != null)
            collectRelatedAssertionsAndFunctions();
        markedExpressions = MutantLab.getInstance().getMarkedExpressions();
        currentMarkedExpression = 0;
        mutationsPerIndex = new int[markedExpressions];
        blockedIndexes = new boolean[markedExpressions];
        Arrays.fill(mutationsPerIndex, 0);
        Arrays.fill(blockedIndexes, Boolean.FALSE);
    }

    public static Candidate original(CompModule context) {
        if (context == null)
            throw new IllegalArgumentException("context can't be null");
        return new Candidate(context);
    }

    public static Candidate mutantFromCandidate(Candidate from, Mutation mutation) {
        if (from == null)
            throw new IllegalArgumentException("from can't be null");
        if (mutation == null)
            throw new IllegalArgumentException("mutation can't be null");
        if (!from.verifyParentStructure())
            throw new IllegalArgumentException("from candidate doesn't have a valid structure");
        CompModule context = from.getContext();
        if (context == null)
            throw new IllegalArgumentException("from candidate doesn't have a context associated");
        Candidate newCandidate;
        if (from.mutation != null && Browsable.equals(from.mutation.mutant(), mutation.original())) {
            Mutation merged = new Mutation(Ops.MULTI, from.mutation.original(), mutation.mutant());
            newCandidate = new Candidate(from.parent, merged, from.getContext());
        } else {
            newCandidate = new Candidate(from, mutation, context);
        }
        newCandidate.currentMarkedExpression = from.currentMarkedExpression;
        if (from.markedExpressions >= 0) {
            System.arraycopy(from.mutationsPerIndex, 0, newCandidate.mutationsPerIndex, 0, from.markedExpressions);
            System.arraycopy(from.blockedIndexes, 0, newCandidate.blockedIndexes, 0, from.markedExpressions);
        }
        return newCandidate;
    }

    private boolean verifyParentStructure() {
        if (parent == null)
            return mutation == null;
        return mutation != null && parent.verifyParentStructure();
    }

    public boolean isFirst() {
        return currentMarkedExpression == 0;
    }

    public boolean isLast() {
        return currentMarkedExpression == markedExpressions + 1;
    }

    public int getCurrentMarkedExpression() {
        return currentMarkedExpression;
    }

    public void setCurrentMarkedExpression(int newValue) {
        if (newValue < 0 || newValue > markedExpressions + 1)
            throw new IllegalArgumentException("marked expression index must be between 0 and " + (markedExpressions + 1) + " (" + newValue + ")");
        currentMarkedExpression = newValue;
    }

    public void currentMarkedExpressionInc() {
        int nextIndex = currentMarkedExpression + 1;
        while (nextIndex <= currentMarkedExpression && isIndexBlocked(nextIndex)) {
            nextIndex++;
        }
        setCurrentMarkedExpression(nextIndex);
    }

    public CompModule getContext() {
        if (context != null) return context;
        if (parent != null) return parent.getContext();
        return null;
    }

    public Optional<Expr> getMutatedExpr(Expr x) {
        if (mutation != null) {
            if (!isAlreadyMutated) {
                if (Browsable.equals(mutation.original(), x))
                    return Optional.of(mutation.mutant());
                else {
                    Expr replacement = isFromFact?findSubExpressionMatchFrom(mutation.original(), x, mutation.mutant()):null;
                    if (replacement != null) {
                        return Optional.of(replacement);
                    }
                }

            }
            return parent.getMutatedExpr(x); //the parent structure guaranties that if mutation != null then parent != null
        }
        return Optional.empty();
    }

    public boolean[] getAllMutatedStatus() {
        List<Boolean> mutatedStatus = new ArrayList<>();
        Candidate current = this;
        while (current != null) {
            if (current.mutation != null) {
                mutatedStatus.add(current.isAlreadyMutated);
            } else {
                break;
            }
            current = current.parent;
        }
        boolean[] result = new boolean[mutatedStatus.size()];
        int idx = 0;
        for (boolean b : mutatedStatus) {
            result[idx] = b;
            idx++;
        }
        return result;
    }

    public void restoreMutatedStatus(boolean[] mutatedStatusBackup) {
        if (mutatedStatusBackup == null)
            throw new IllegalArgumentException("Mutated status backup can't be null");
        int candidateWithMutations = 0;
        int idx = 0;
        Candidate current = this;
        while (current != null) {
            if (current.mutation != null) {
                current.isAlreadyMutated = mutatedStatusBackup[idx];
                candidateWithMutations++;
            } else {
                break;
            }
            current = current.parent;
            idx++;
        }
        if (mutatedStatusBackup.length != candidateWithMutations)
            throw new IllegalStateException("The backup correspond to " + mutatedStatusBackup.length + " mutations but " + candidateWithMutations + " candidates with mutations were found");
    }

    private Expr findSubExpressionMatchFrom(Expr from, Expr target, Expr replacement) {
         if (Browsable.equals(from, target))
             return replacement;
         if (from instanceof ExprUnary && ((ExprUnary)from).op.equals(ExprUnary.Op.NOOP)) {
             if (replacement instanceof ExprUnary && ((ExprUnary)replacement).op.equals(ExprUnary.Op.NOOP))
                return findSubExpressionMatchFrom(((ExprUnary) from).sub, target, ((ExprUnary) replacement).sub);
             else
                 return findSubExpressionMatchFrom(((ExprUnary) from).sub, target, replacement);
         }
         if (from instanceof ExprList) {
             ExprList fromAsList = (ExprList) from;
             if (replacement instanceof ExprList) {
                 ExprList replacementAsList = (ExprList) replacement;
                 int i = 0;
                 for (Expr subExpr : fromAsList.args) {
                     Expr res = findSubExpressionMatchFrom(target, subExpr, replacementAsList.args.get(i));
                     i++;
                     if (res != null)
                         return res;
                 }
             } else {
                 for (Expr subExpr : fromAsList.args) {
                     Expr res = findSubExpressionMatchFrom(subExpr, target, replacement);
                     if (res != null)
                         return res;
                 }
             }
         }
         return null;
    }

    public void markAsAlreadyMutated(Expr x) {
        if (mutation != null) {
            if (Browsable.equals(mutation.original(), x) && !isAlreadyMutated)
                isAlreadyMutated = true;
            else if (findSubExpressionMatchFrom(mutation.original(), x, mutation.mutant()) != null && !isAlreadyMutated)
                isAlreadyMutated = true;
            else
                parent.markAsAlreadyMutated(x);
        } else
            throw new IllegalArgumentException("This expression has no mutation associated");
    }

    public void clearMutatedStatus() {
        isAlreadyMutated = false;
        if (parent != null)
            parent.clearMutatedStatus();
    }

    public void markAsInvalid() {
        markedAsInvalid = true;
    }

    public void markAsPartialRepair() {
        isPartialRepair = true;
    }

    public boolean isPartialRepair() {
        return isPartialRepair;
    }

    public boolean isValid() {
        return !markedAsInvalid;
    }

    public List<Browsable> getRelatedAssertionsAndFunctions() {
        List<Browsable> relatedAssertionsAndFunctions = new LinkedList<>();
        if (this.relatedAssertionsAndFunctions != null) {
            relatedAssertionsAndFunctions.addAll(this.relatedAssertionsAndFunctions);
            parent.getRelatedAssertionsAndFunctions().forEach(aof -> {
                if (!relatedAssertionsAndFunctions.contains(aof))
                    relatedAssertionsAndFunctions.add(aof);
            });
        }
        return relatedAssertionsAndFunctions;
    }

    private void collectRelatedAssertionsAndFunctions() {
        if (relatedAssertionsAndFunctions == null) {
            relatedAssertionsAndFunctions = new LinkedList<>();
            Optional<Func> contFunc = ContextExpressionExtractor.getContainerFunc(mutation.original());
            if (contFunc.isPresent() && !relatedAssertionsAndFunctions.contains(contFunc.get())) {
                relatedAssertionsAndFunctions.add(contFunc.get());
            } else {
                Expr mayorExpr = TypeChecking.getMayorExpression(mutation.original());
                Optional<Pair<String, Expr>> namedAssertion = findAssertionMatchFor(mayorExpr);
                if (namedAssertion.isPresent() && !relatedAssertionsAndFunctions.contains(namedAssertion.get().b))
                    relatedAssertionsAndFunctions.add(namedAssertion.get().b);
                else if (!namedAssertion.isPresent()) {
                    Optional<Pair<String, Expr>> namedFact = findFactMatchFor(mayorExpr);
                    if (namedFact.isPresent() && !relatedAssertionsAndFunctions.contains(namedFact.get().b)) {
                        relatedAssertionsAndFunctions.add(namedFact.get().b);
                        isFromFact = true;
                    } else {
                        //Facts are weird, let's try searching for the mutated fact
                        mayorExpr = mutation.original();
                        namedFact = findFactMatchFor(TypeChecking.getMayorExpression(mutation.mutant()));
                        if (namedFact.isPresent()) {
                            //although we found the mutated fact, we should use the original
                            if (!relatedAssertionsAndFunctions.contains(mayorExpr)) {
                                relatedAssertionsAndFunctions.add(mayorExpr);
                                isFromFact = true;
                            }
                        }
                    }
                }
            }
        }
    }

    private Optional<Pair<String, Expr>> findAssertionMatchFor(Expr mayorExpr) {
        return findMatchInsideFor(context.getAllAssertions(), mayorExpr);
    }

    private Optional<Pair<String, Expr>> findFactMatchFor(Expr mayorExpr) {
        return findMatchInsideFor(context.getAllFacts().makeCopy(), mayorExpr);
    }

    private Optional<Pair<String, Expr>> findMatchInsideFor(List<Pair<String, Expr>> pairs, Expr mayorExpr) {
        return pairs.stream().filter(na -> {
            Expr body = na.b;
            return body.toString().compareTo(mayorExpr.toString()) == 0;
        }).findFirst();
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("CANDIDATE {").append("INDEX : ");
        sb.append(currentMarkedExpression);
        sb.append(" FROM RANGE [0..");
        sb.append(markedExpressions + 1).append("] ;");
        sb.append(Arrays.toString(mutationsPerIndex)).append(" mutations}");
        if (isPartialRepair) {
            sb.append("\n");
            sb.append("PARTIAL REPAIR OF ");
            sb.append(Arrays.toString(blockedIndexes));
        }
        sb.append("\n");
        sb.append(toString(this));
        return sb.toString();
    }

    private String toString(Candidate current) {
        StringBuilder sb = new StringBuilder();
        if (current.mutation == null)
            sb.append("ORIGINAL").append("\n");
        else {
            sb.append(toString(current.parent));
            sb.append("Line: ").append(current.mutation.original().pos.y).append(" : ");
            sb.append(current.mutation.toString()).append("\n");
        }
        return sb.toString();
    }

    public int mutationsForIndex(int index) {
        if (index < 1 || index > markedExpressions)
            throw new IllegalArgumentException("Index must go between 1 and " + markedExpressions);
        return mutationsPerIndex[index-1];
    }

    public int mutationsForCurrentIndex() {
        return mutationsForIndex(currentMarkedExpression);
    }

    public int mutations() {
        if (mutation != null && mutation.operator().equals(Ops.VAR))
            return variabilizationMutation();
        int totalMutations = 0;
        for (int i = 1; i <= markedExpressions; i++)
            totalMutations += mutationsForIndex(i);
        return totalMutations;
    }

    private int variabilizationMutation() {
        int totalMutations = 0;
        Candidate current = this;
        while (current != null) {
            if (current.mutation != null) {
                if (!current.mutation.operator().equals(Ops.VAR))
                    throw new IllegalStateException("This method should only be called for Variabilization candidates");
                totalMutations++;
            }
            current = current.parent;
        }
        return totalMutations;
    }

    public void increaseMutations(int index) {
        if (index < 1 || index > markedExpressions)
            throw new IllegalArgumentException("Index must go between 1 and " + markedExpressions);
        mutationsPerIndex[index-1]++;
    }

    public void blockIndex(int index) {
        if (index < 1 || index > markedExpressions)
            throw new IllegalArgumentException("Index must go between 1 and " + markedExpressions);
        blockedIndexes[index-1] = true;
    }

    public void unblockIndex(int index) {
        if (index < 1 || index > markedExpressions)
            throw new IllegalArgumentException("Index must go between 1 and " + markedExpressions);
        blockedIndexes[index-1] = false;
    }

    public boolean isIndexBlocked(int index) {
        if (index < 1 || index > markedExpressions)
            throw new IllegalArgumentException("Index must go between 1 and " + markedExpressions + " (" + index + ")");
        return blockedIndexes[index-1];
    }

    public void setCommandsResults(Map<Command, Boolean> commandsResults) {
        if (commandsResults == null)
            throw new IllegalArgumentException("commands results map can't be null");
        this.commandsResults = commandsResults;
        this.hasPartialResults = !commandsResults.isEmpty();
    }

    public boolean hasPartialResults() {
        return hasPartialResults;
    }

    public Map<Command, Boolean> getCommandsResults() {
        if (commandsResults == null)
            throw new IllegalStateException("This method should only be called if #hasRepairedCommandsResults() returned true");
        return commandsResults;
    }

    public void clearPartialResultsData() {
        if (!hasPartialResults())
            throw new IllegalStateException("This is not a candidate with partial results");
        commandsResults = null;
        hasPartialResults = false;
        Arrays.fill(blockedIndexes, false);
    }

    public List<Mutation> getMutations() {
        List<Mutation> mutations = new LinkedList<>();
        if (mutation != null) {
            mutations.add(0, mutation);
            mutations.addAll(parent.getMutations());
        }
        return mutations;
    }

    public Optional<Mutation> getLastMutation() {
        return Optional.ofNullable(mutation);
    }

    public void copyMutationsFrom(Candidate from) {
        System.arraycopy(from.mutationsPerIndex, 0, mutationsPerIndex ,0, markedExpressions);
    }

    /**
     * Returns a list of triplet (place & OP, orig expr, mutation expr) of the current mutation
     * @return a representation of a this candidate mutations showing line, operator used, original and mutated expression
     */
    public List<Triplet<String,String,String>>  getCurrentMutationsInfo(){
        ArrayList<Triplet<String,String,String>> l = new ArrayList<>();
        if (!isValid()) {
            Triplet<String, String, String> t =  new Triplet<>("INVALID","","");
            l.add(t);
        }
        else {
            for (Mutation m : getMutations()) {
                Triplet<String, String, String> t = new Triplet<>("Line "+m.original().pos.y+" <"+ m.operator()+"> ", m.original().toString(),m.mutant().toString());
                l.add(t);
            }
        }
        return l;
    }

    public Candidate copy() {
        if (markedAsInvalid)
            throw new IllegalStateException("Shouldn't clone an invalid candidate");
        Candidate clone = new Candidate(context);
        clone.mutation = mutation;
        clone.relatedAssertionsAndFunctions = relatedAssertionsAndFunctions != null? (new LinkedList<>(relatedAssertionsAndFunctions)):null;
        clone.isAlreadyMutated = false;
        clone.markedAsInvalid = false;
        clone.markedExpressions = markedExpressions;
        clone.currentMarkedExpression = currentMarkedExpression;
        clone.parent = parent != null? parent.copy() : null;
        clone.mutationsPerIndex = new int[markedExpressions];
        clone.blockedIndexes = new boolean[markedExpressions];
        clone.hasPartialResults = hasPartialResults;
        clone.isPartialRepair = isPartialRepair;
        System.arraycopy(mutationsPerIndex, 0, clone.mutationsPerIndex, 0, markedExpressions);
        System.arraycopy(blockedIndexes, 0, clone.blockedIndexes, 0, markedExpressions);
        return clone;
    }

}

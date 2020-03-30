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

    private Candidate parent;
    private Mutation mutation;
    private CompModule context;
    List<Browsable> relatedAssertionsAndFunctions;
    private boolean isAlreadyMutated;
    private boolean markedAsInvalid;
    public static final Candidate INVALID;
    public static final Candidate STOP;
    private int markedExpressions;
    private int currentMarkedExpression;
    private int[] mutationsPerIndex;
    private boolean isFromFact;

    static {
        Candidate invalid = new Candidate(null);
        invalid.markAsInvalid();
        INVALID = invalid;
        STOP = new Candidate(null);
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
        Arrays.fill(mutationsPerIndex, 0);
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
        if (from.mutation != null && from.mutation.mutant().equals(mutation.original())) {
            Mutation merged = new Mutation(Ops.MULTI, from.mutation.original(), mutation.mutant());
            newCandidate = new Candidate(from.parent, merged, from.getContext());
        } else {
            newCandidate = new Candidate(from, mutation, context);
        }
        newCandidate.currentMarkedExpression = from.currentMarkedExpression;
        if (from.markedExpressions >= 0)
            System.arraycopy(from.mutationsPerIndex, 0, newCandidate.mutationsPerIndex, 0, from.markedExpressions);
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
        setCurrentMarkedExpression(currentMarkedExpression + 1);
    }

    public CompModule getContext() {
        if (context != null) return context;
        if (parent != null) return parent.getContext();
        return null;
    }

    public Optional<Expr> getMutatedExpr(Expr x) {
        if (mutation != null) {
            if (!isAlreadyMutated) {
                if (mutation.original().equals(x))
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

    private Expr findSubExpressionMatchFrom(Expr from, Expr target, Expr replacement) {
         if (from.equals(target))
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
            if (mutation.original().equals(x) && !isAlreadyMutated)
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
        return "CANDIDATE {" +
                "INDEX : " + currentMarkedExpression +
                " FROM RANGE [0.." + (markedExpressions + 1) + "] ; " + Arrays.toString(mutationsPerIndex) + " mutations}\n" +
                toString(this);
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

    public int mutations(int index) {
        if (index < 1 || index > markedExpressions)
            throw new IllegalArgumentException("Index must go between 1 and " + markedExpressions);
        return mutationsPerIndex[index-1];
    }

    public int mutations() {
        return mutations(currentMarkedExpression);
    }

    public void increaseMutations(int index) {
        if (index < 1 || index > markedExpressions)
            throw new IllegalArgumentException("Index must go between 1 and " + markedExpressions);
        mutationsPerIndex[index-1]++;
    }

    public List<Mutation> getMutations() {
        List<Mutation> mutations = new LinkedList<>();
        if (mutation != null) {
            mutations.add(0, mutation);
            mutations.addAll(parent.getMutations());
        }
        return mutations;
    }

    public void copyMutationsFrom(Candidate from) {
        System.arraycopy(from.mutationsPerIndex, 0, mutationsPerIndex ,0, markedExpressions);
    }

    public Optional<Mutation> getLastMutation() {
        return Optional.ofNullable(mutation);
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
        System.arraycopy(mutationsPerIndex, 0, clone.mutationsPerIndex, 0, markedExpressions);
        return clone;
    }

}

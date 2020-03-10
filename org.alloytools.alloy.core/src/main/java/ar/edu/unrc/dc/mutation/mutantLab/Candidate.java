package ar.edu.unrc.dc.mutation.mutantLab;

import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.Ops;
import ar.edu.unrc.dc.mutation.util.ContextExpressionExtractor;
import ar.edu.unrc.dc.mutation.util.TypeChecking;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.alloy4.Triplet;
import edu.mit.csail.sdg.ast.Browsable;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.Func;
import edu.mit.csail.sdg.parser.CompModule;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

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
    private int mutations;

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
        isAlreadyMutated = false;
        markedAsInvalid = false;
        this.mutation = mutation;
        this.parent = parent;
        this.context = context;
        if (mutation != null)
            collectRelatedAssertionsAndFunctions();
        markedExpressions = MutantLab.getInstance().getMarkedExpressions();
        currentMarkedExpression = 0;
    }

    public static Candidate original(CompModule context) {
        if (context == null)
            throw new IllegalArgumentException("context can't be null");
        Candidate original = new Candidate(context);
        original.mutations = 0;
        return original;
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
        if (from.mutation != null && from.mutation.mutant().equals(mutation.original())) {
            Mutation merged = new Mutation(Ops.MULTI, from.mutation.original(), mutation.mutant());
            return new Candidate(from.parent, merged, from.getContext());
        }
        Candidate newCandidate = new Candidate(from, mutation, context);
        newCandidate.mutations = 1 + from.mutations;
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
            if (mutation.original().equals(x) && !isAlreadyMutated)
                return Optional.of(mutation.mutant());
            return parent.getMutatedExpr(x); //the parent structure guaranties that if mutation != null then parent != null
        }
        return Optional.empty();
    }

    public void markAsAlreadyMutated(Expr x) {
        if (mutation != null) {
            if (mutation.original().equals(x) && !isAlreadyMutated)
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
                if (relatedAssertionsAndFunctions.contains(aof))
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
                Optional<Pair<String, Expr>> namedAssertion = context.getAllAssertions().stream().filter(na -> {
                    Expr body = na.b;
                    return body.toString().compareTo(mayorExpr.toString()) == 0;
                }).findFirst();
                if (namedAssertion.isPresent() && !relatedAssertionsAndFunctions.contains(namedAssertion.get().b))
                    relatedAssertionsAndFunctions.add(namedAssertion.get().b);
            }
        }
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        if (mutation == null)
            sb.append("ORIGINAL").append("\n");
        else {
            sb.append(parent.toString());
            sb.append("Line: ").append(mutation.original().pos.y).append(" : ");
            sb.append(mutation.toString()).append("\n");
        }
        return sb.toString();
    }

    public int mutations() {
        return mutations;
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
        return clone;
    }

}

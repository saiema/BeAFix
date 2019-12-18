package ar.edu.unrc.dc.mutation;

import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.parser.CompModule;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;

public class MutantLab {

    private CompModule context; //Source that the Mutant Laboratory is associated to.
    private Mutation currentMutation = null; //current mutation to apply
    private Iterator<Mutation> it = null;

//    /**
//     * For each marked Expression the laboratory has a ExprMutations that manage
//     * all the expression or subexpressions mutated for it.
//     */
//    private class ExprMutations{
//        private Expr markedExpr;
//        //TODO build a filter for possible mutations (Op for example) to take into account
//
//        private List<Mutation> mutations;
//
//        /**
//         * Constructor
//         * @param markedExpr original source marked expression
//         * @param mutations list of mutations of the marked expression or subexpression of it
//         */
//        public ExprMutations(Expr markedExpr, List<Mutation> mutations) {
//            this.markedExpr = markedExpr;
//            this.mutations = mutations;
//        }
//    }

    //TODO build a filter for marked expression to take into account
    private List<Mutation> laboratory =new ArrayList<>(); //List of Original Exprs to mutate and their available mutations

    /**
     * Constructor tha create a new  MutantLaboratory for a compmodule generating all possible mutations for its marked expr
     */
    public MutantLab(CompModule contex) {
        this.context=contex;
        for (Expr e:contex.markedEprsToMutate) {
            addMutationsForMark(e);
        }
    }


    /**
     * Add the mutations available for a marked to mutate expression
     * @param origin
     */
    private void addMutationsForMark(Expr origin){
        if (it!=null){
            throw new IllegalStateException("You cant add mutations in initiated repair process");
        }
        for (Ops o : Ops.values()) {
            if (o.isImplemented()) {
                Optional<List<Mutation>> opMutations = o.getOperator(context).getMutations(origin);
                if (opMutations.isPresent())
                    laboratory.addAll(opMutations.get());
            }
        }
    }


    /**
     * set the next available mutation to apply
     */
    public void next(){
        if (it == null) it = laboratory.iterator();
        if (it.hasNext()) currentMutation = it.next();
    }

    /**
     * Return the mutation to apply if corresponds, else return the original Expression queried
     * @return
     */
    public Expr getMutation(Expr query){
        if ((currentMutation!=null) && (query==currentMutation.original()))
            return currentMutation.mutant();
        return query;
    }

    /**
     * set the next available mutation to apply
     */
    public Mutation getCurrentMutant(){
        if ( currentMutation != null) return currentMutation;
        return null;
    }
    public int mutantCount (){return laboratory.size();}

};



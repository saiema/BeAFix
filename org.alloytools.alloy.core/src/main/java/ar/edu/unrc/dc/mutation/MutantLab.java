package ar.edu.unrc.dc.mutation;

import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.parser.CompModule;
import org.apache.commons.math3.util.Combinations;

import java.util.*;

public class MutantLab {

    private CompModule context; //Source that the Mutant Laboratory is associated to.
    //Iterator that iterates over a all possible combinations of mutations index
    // (take into account that combinations of the same expressions will be avoided)
    private Iterator<int[]> it = null;
    private List<int[]> mutationsIndexes;


    //keep the current mapping from original Expr to Mutated ones to apply at each step
    private Map<Expr,Expr> currentMutations2Apply = new HashMap<Expr,Expr>();

//    /**
//     * For each marked Expression the laboratory has a ExprMutations that manage
//     * all the expression or subexpressions mutated for it.
//     */
//    private class ExprMutations{
//        private Expr markedExpr;
//        //TODO build a filter for possible mutations (Op for example) to take into account
//
//        private List<Mutation> mutations;
//        private Iterator<Mutation> it = null;
//
//        /**
//         * Constructor
//         * @param markedExpr original source marked expression
//         * @param mutations list of mutations of the marked expression or subexpression of it
//         */
//        public ExprMutations(Expr markedExpr, List<Mutation> mutations) {
//            this.markedExpr = markedExpr;
//            this.mutations = mutations;
//            this.it = mutations.iterator();
//        }
//
//        public void init(){
//            it = mutations.iterator();
//        }
//
//        /**
//         * set the next available mutation to apply
//         */
//        public Mutation next(){
//            if (it.hasNext()) return it.next();
//            return null;
//        }
//    }

    //TODO build a filter for marked expression to take into account
    private List<Pair<Integer,Mutation>> laboratory =new ArrayList<>(); //List of Original Exprs to mutate and their available mutations

    /**
     * Constructor tha create a new  MutantLaboratory for a compmodule generating all possible mutations for its marked expr
     */
    public MutantLab(CompModule contex) {
        this.context=contex;
        for (Expr e:contex.markedEprsToMutate) {
            addMutationsForMark(e);
        }
        generate(laboratory.size(),contex.markedEprsToMutate.size());
        it = mutationsIndexes.iterator();
    }


    private void addMutationExpr2Lab(Expr e, List<Mutation> mutList){
        for (Mutation m:mutList ) {
            laboratory.add(new Pair<Integer,Mutation>(e.getID(),m));
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
                if (opMutations.isPresent()) {
                    addMutationExpr2Lab(origin,opMutations.get());
                }
            }
        }
    }

    /**
     * generates a list of array index with all possible combinations of n indexes.
     * e.g. for n=3 -> [[0],[1],[2],[0,1],[0,2],[1,2],[0,1,2]]
     */
    private void generate(int mutationCount, int exprCount) {
        mutationsIndexes = new ArrayList<int[]>();
        int i;
        for (i = 1; i<=exprCount; i++) {
            Iterator<int[]> iterIdx = new Combinations(mutationCount, i).iterator();
            //set to control valid indexes
            HashSet<Integer> uniques = new HashSet<>();
            while (iterIdx.hasNext()) {
                final int[] combination = iterIdx.next();
                //check if is a valid index (i.e. every mutation's index belongs to different marked expression)
                uniques.clear();
                for(int j=0;j<combination.length;j++){
                    uniques.add(laboratory.get(combination[j]).a);
                }
                if (uniques.size()==combination.length){
                    //valid index
                    mutationsIndexes.add(combination);
                }

            }
        }
    }

    /**
     * set the next available mutation to apply
     */
    public void next(){
        if (it.hasNext()) {
            //get the index of the next combination
            int[] idxs = it.next();
            //build the map to be queried
            currentMutations2Apply.clear();
            for(int i=0;i<idxs.length;i++){
                currentMutations2Apply.put(laboratory.get(idxs[i]).b.original(),laboratory.get(idxs[i]).b.mutant());
            }
        };
    }

    /**
     * Return the mutation to apply if corresponds, else return the original Expression queried
     * @return
     */
    public Expr getMutation(Expr query){
        Expr res = currentMutations2Apply.get(query);
        if ((res!=null))
            return res;
        return query;
    }

    public int mutantCount (){return mutationsIndexes.size();}

    public List<Pair<String,String>> getCurrentMutationsAsList(){
        List<Pair<String,String>> res = new ArrayList<Pair<String,String>>();
        for (final Map.Entry<Expr, Expr> pair : currentMutations2Apply.entrySet()){
            Pair<String,String> r = new Pair(pair.getKey().toString(),pair.getValue().toString());
            res.add(r);
        }
       return res;
    }

};



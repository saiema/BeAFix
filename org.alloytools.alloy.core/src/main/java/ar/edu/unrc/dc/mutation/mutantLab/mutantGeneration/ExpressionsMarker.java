package ar.edu.unrc.dc.mutation.mutantLab.mutantGeneration;

import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.Func;
import edu.mit.csail.sdg.parser.CompModule;

public class ExpressionsMarker {

    public static void markAllExpressions(CompModule root) {
        if (root.markedEprsToMutate.size() > 0)
            throw new IllegalArgumentException("Can't mark expressions on a module that already has marked expressions");
        for (Pair<String, Expr> assertion : root.getAllAssertions()) {
            root.addMarkedExprToMutate(assertion.b);
        }
        for (Pair<String, Expr> fact : root.getAllFacts()) {
            root.addMarkedExprToMutate(fact.b);
        }
        for (Func func : root.getAllFunc()) {
            root.addMarkedExprToMutate(func.getBody());
        }
    }

}

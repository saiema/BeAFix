package ar.edu.unrc.dc.mutation.op;

import java.util.LinkedList;
import java.util.List;

import ar.edu.unrc.dc.mutation.Mutator;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.ExprBinary;
import edu.mit.csail.sdg.ast.ExprBinary.Op;


public class RO extends Mutator {

    @Override
    public List<Expr> visit(ExprBinary x) throws Err {
        return mutants(x);
    }

    private List<Expr> mutants(ExprBinary x) {
        List<Expr> mutants = new LinkedList<>();
        for (Op o : RELATIONAL_OPS) {
            if (x.op.equals(o))
                continue;
            ExprBinary mutant = x.mutateOp(o);
            mutants.add(mutant);
        }
        return mutants;
    }

}

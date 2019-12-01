package ar.edu.unrc.dc.mutation.op;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.Mutator;
import ar.edu.unrc.dc.mutation.Ops;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.ExprUnary;

/**
 * Relational Unary Operator Replacement
 * <p>
 *
 * Replaces a unary relational operator in a unary expression, relational unary
 * operators being:
 * <li>transpose (~)</li>
 * <li>closure (^)</li>
 * <li>reflexive closure (*)</li>
 */
public class RUOR extends Mutator {

    @Override
    public Optional<List<Mutation>> visit(ExprUnary x) throws Err {
        if (!isUnaryRelationalExpression(x))
            return super.visit(x);
        return mutants(x);
    }

    private Optional<List<Mutation>> mutants(ExprUnary x) {
        List<Mutation> mutants = new LinkedList<>();
        for (ExprUnary.Op o : RELATIONAL_UNARY_OPS) {
            if (x.op.equals(o))
                continue;
            ExprUnary mutant = x.mutateOp(o);
            mutants.add(new Mutation(Ops.RUOR, x, mutant));
        }
        return Optional.of(mutants);
    }

}

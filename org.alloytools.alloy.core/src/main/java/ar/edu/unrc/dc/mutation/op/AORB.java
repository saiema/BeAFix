package ar.edu.unrc.dc.mutation.op;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.Mutator;
import ar.edu.unrc.dc.mutation.Ops;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.ExprBinary;
import edu.mit.csail.sdg.ast.ExprBinary.Op;


public class AORB extends Mutator {

    @Override
    public Optional<List<Mutation>> visit(ExprBinary x) throws Err {
        if (!isArithmeticExpression(x))
            return super.visit(x);
        return mutants(x);
    }

    private Optional<List<Mutation>> mutants(ExprBinary x) {
        List<Mutation> mutants = new LinkedList<>();
        for (Op o : ARITHMETIC_BINARY_OPS) {
            if (x.op.equals(o))
                continue;
            ExprBinary mutant = x.mutateOp(o);
            mutants.add(new Mutation(Ops.AORB, x, mutant));
        }
        return Optional.of(mutants);
    }

}

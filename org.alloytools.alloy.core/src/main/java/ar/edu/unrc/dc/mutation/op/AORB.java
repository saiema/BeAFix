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
import edu.mit.csail.sdg.parser.CompModule;


/**
 * Arithmetic Operator Replacement Binary
 * <p>
 *
 * Replaces an arithmetic operator in a binary expression, binary arithmetic
 * operators being:
 * <li>divide</li>
 * <li>multiply</li>
 * <li>remainder</li>
 * <li>plus (+)</li>
 * <li>minus (-)</li>
 *
 */
public class AORB extends Mutator {

    public AORB(CompModule context) {
        super(context);
    }

    @Override
    public Optional<List<Mutation>> visit(ExprBinary x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        if (isArithmeticExpression(x)) {
            Optional<List<Mutation>> mutants = mutants(x);
            if (mutants.isPresent())
                mutations.addAll(mutants.get());
        }
        Optional<List<Mutation>> leftMutations = x.left.accept(this);
        Optional<List<Mutation>> rightMutations = x.right.accept(this);
        if (leftMutations.isPresent())
            mutations.addAll(leftMutations.get());
        if (rightMutations.isPresent())
            mutations.addAll(rightMutations.get());
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return EMPTY;
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

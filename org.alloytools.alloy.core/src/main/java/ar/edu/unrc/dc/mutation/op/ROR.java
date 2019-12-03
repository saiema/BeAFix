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

/**
 * Relational Operator Replacement
 * <p>
 *
 * Replaces a relational operator in a binary expression, relational operators
 * being:
 * <li>Equal (=)</li>
 * <li>Not Equal (!=)</li>
 * <li>Greater (>)</li>
 * <li>Not Greater (!>)</li>
 * <li>Greater or equal (>=)</li>
 * <li>Not Greater or equal (!>=)</li>
 * <li>Less (<)</li>
 * <li>Not Less (!<)</li>
 * <li>Less or equal (=<)</li>
 * <li>Not Less or equal (!=<)</li>
 *
 */
public class ROR extends Mutator {

    @Override
    public Optional<List<Mutation>> visit(ExprBinary x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        if (isRelationalExpression(x)) {
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
        for (Op o : RELATIONAL_OPS) {
            if (x.op.equals(o))
                continue;
            ExprBinary mutant = x.mutateOp(o);
            mutants.add(new Mutation(Ops.ROR, x, mutant));
        }
        return Optional.of(mutants);
    }

}

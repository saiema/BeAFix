package ar.edu.unrc.dc.mutation.op;

import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.Mutator;
import ar.edu.unrc.dc.mutation.Ops;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.ExprUnary;
import edu.mit.csail.sdg.parser.CompModule;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

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

    //    @Override
    //    public Optional<List<Mutation>> visit(ExprBinary x) throws Err {
    //        List<Mutation> mutations = new LinkedList<>();
    //        Optional<List<Mutation>> leftMutations = x.left != null ? x.left.accept(this) : EMPTY;
    //        Optional<List<Mutation>> rightMutations = x.right != null ? x.right.accept(this) : EMPTY;
    //        if (leftMutations.isPresent())
    //            mutations.addAll(leftMutations.get());
    //        if (rightMutations.isPresent())
    //            mutations.addAll(rightMutations.get());
    //        if (!mutations.isEmpty())
    //            return Optional.of(mutations);
    //        return EMPTY;
    //    }

    public RUOR(CompModule context) {
        super(context);
    }

    @Override
    public Optional<List<Mutation>> visit(ExprUnary x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        if (isUnaryRelationalExpression(x)) {
            Optional<List<Mutation>> mutants = mutants(x);
            if (mutants.isPresent())
                mutations.addAll(mutants.get());
        }
        Optional<List<Mutation>> subMutations = x.sub != null ? x.sub.accept(this) : EMPTY;
        if (subMutations.isPresent())
            mutations.addAll(subMutations.get());
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return EMPTY;
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

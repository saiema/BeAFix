package ar.edu.unrc.dc.mutation.op;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.Mutator;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.ExprBinary;
import edu.mit.csail.sdg.ast.ExprBinary.Op;
import edu.mit.csail.sdg.parser.CompModule;


public abstract class BinOpReplacer extends Mutator {

    protected BinOpReplacer(CompModule context) {
        super(context);
    }

    @Override
    public final Optional<List<Mutation>> visit(ExprBinary x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        if (canMutate(x)) {
            Optional<List<Mutation>> mutants = mutants(x);
            mutants.ifPresent(mutations::addAll);
        }
        Optional<List<Mutation>> leftMutations = x.left.accept(this);
        Optional<List<Mutation>> rightMutations = x.right.accept(this);
        leftMutations.ifPresent(mutations::addAll);
        rightMutations.ifPresent(mutations::addAll);
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return EMPTY;
    }

    protected abstract boolean canMutate(ExprBinary x);

    protected abstract List<Op> getOperators();

    protected abstract boolean validate(ExprBinary original, Op newOperator);

    protected final Optional<List<Mutation>> mutants(ExprBinary x) {
        List<Mutation> mutants = new LinkedList<>();
        for (Op o : getOperators()) {
            if (x.op.equals(o))
                continue;
            if (!validate(x, o))
                continue;
            ExprBinary mutant = x.mutateOp(o);
            mutants.add(new Mutation(whoiam(), x, mutant));
        }
        return Optional.of(mutants);
    }

}

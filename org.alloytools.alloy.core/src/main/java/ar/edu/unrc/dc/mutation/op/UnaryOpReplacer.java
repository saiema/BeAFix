package ar.edu.unrc.dc.mutation.op;

import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.Mutator;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.ExprUnary;
import edu.mit.csail.sdg.parser.CompModule;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;


public abstract class UnaryOpReplacer extends Mutator {

    protected UnaryOpReplacer(CompModule context) {
        super(context);
    }

    @Override
    public Optional<List<Mutation>> visit(ExprUnary x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        if (canMutate(x)) {
            Optional<List<Mutation>> mutants = mutants(x);
            mutants.ifPresent(mutations::addAll);
        }
        Optional<List<Mutation>> subMutations = x.sub != null ? x.sub.accept(this) : EMPTY;
        subMutations.ifPresent(mutations::addAll);
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return EMPTY;
    }

    protected abstract boolean canMutate(ExprUnary x);

    protected abstract List<ExprUnary.Op> getOperators();

    private Optional<List<Mutation>> mutants(ExprUnary x) {
        List<Mutation> mutants = new LinkedList<>();
        for (ExprUnary.Op o : getOperators()) {
            if (x.op.equals(o))
                continue;
            ExprUnary mutant = x.mutateOp(o);
            mutants.add(new Mutation(whoiam(), x, mutant));
        }
        return Optional.of(mutants);
    }

}

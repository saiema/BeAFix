package ar.edu.unrc.dc.mutation.op;

import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.MutationConfiguration;
import ar.edu.unrc.dc.mutation.MutationConfiguration.ConfigKey;
import ar.edu.unrc.dc.mutation.Mutator;
import ar.edu.unrc.dc.mutation.util.TypeChecking;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.ExprBinary;
import edu.mit.csail.sdg.ast.ExprBinary.Op;
import edu.mit.csail.sdg.parser.CompModule;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;


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
        Optional<List<Mutation>> leftMutations = visitThis(x.left);
        Optional<List<Mutation>> rightMutations = visitThis(x.right);
        leftMutations.ifPresent(mutations::addAll);
        rightMutations.ifPresent(mutations::addAll);
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return Optional.empty();
    }

    protected abstract boolean canMutate(ExprBinary x);

    protected abstract List<Op> getOperators();

    protected abstract boolean validate(ExprBinary original, Op newOperator);

    protected final Optional<List<Mutation>> mutants(ExprBinary x) {
        if (!mutGenLimitCheck(x))
            return Optional.empty();
        List<Mutation> mutants = new LinkedList<>();
        for (Op o : getOperators()) {
            if (x.op.equals(o))
                continue;
            if (!validate(x, o))
                continue;
            Expr mutant = x.mutateOp(o);
            if (TypeChecking.canReplace(x, mutant, strictTypeCheck()))
                mutants.add(new Mutation(whoiam(), x, mutant));
        }
        return Optional.of(mutants);
    }

    private boolean strictTypeCheck() {
        Optional<Object> configValue = MutationConfiguration.getInstance().getConfigValue(ConfigKey.OPERATOR_BIN_OP_REPLACEMENT_STRICT_TYPE_CHECK);
        return configValue.map(o -> (Boolean) o).orElse((Boolean)ConfigKey.OPERATOR_BIN_OP_REPLACEMENT_STRICT_TYPE_CHECK.defaultValue());
    }

}

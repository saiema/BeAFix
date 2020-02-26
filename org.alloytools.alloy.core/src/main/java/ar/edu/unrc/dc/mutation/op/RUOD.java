package ar.edu.unrc.dc.mutation.op;

import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.MutationConfiguration;
import ar.edu.unrc.dc.mutation.Mutator;
import ar.edu.unrc.dc.mutation.Ops;
import ar.edu.unrc.dc.mutation.util.TypeChecking;
import ar.edu.unrc.dc.mutation.MutationConfiguration.ConfigKey;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.ExprUnary;
import edu.mit.csail.sdg.parser.CompModule;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

/**
 * Relational Unary Operator Deletion
 * <p>
 *
 * Removes a unary relational operator in a compatible expression, relational
 * unary operators being:
 * <li>transpose (~)</li>
 * <li>closure (^)</li>
 * <li>reflexive closure (*)</li>
 */
public class RUOD extends Mutator {


    public RUOD(CompModule context) {
        super(context);
    }

    @Override
    public Optional<List<Mutation>> visit(ExprUnary x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        Optional<List<Mutation>> mutants = mutate(x);
        mutants.ifPresent(mutations::addAll);
        Optional<List<Mutation>> subExprMutations = visitThis(x.sub);
        subExprMutations.ifPresent(mutations::addAll);
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return Optional.empty();
    }

    private Optional<List<Mutation>> mutate(ExprUnary x) {
        if (!mutGenLimitCheck(x))
            return Optional.empty();
        List<Mutation> mutations = new LinkedList<>();
        if (isUnaryRelationalExpression(x)) {
            Expr mutant = (Expr) x.sub.clone();
            if (TypeChecking.canReplace(x, mutant, strictTypeCheck())) {
                mutations.add(new Mutation(whoiam(), x, mutant));
            }
        }
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return Optional.empty();

    }

    @Override
    protected Ops whoiam() {
        return Ops.RUOD;
    }

    private boolean strictTypeCheck() {
        Optional<Object> configValue = MutationConfiguration.getInstance().getConfigValue(ConfigKey.OPERATOR_RUOD_STRICT_TYPE_CHECK);
        return configValue.map(o -> (Boolean) o).orElse((Boolean)ConfigKey.OPERATOR_RUOD_STRICT_TYPE_CHECK.defaultValue());
    }

}

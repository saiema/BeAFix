package ar.edu.unrc.dc.mutation.op;

import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.MutationConfiguration;
import ar.edu.unrc.dc.mutation.MutationConfiguration.ConfigKey;
import ar.edu.unrc.dc.mutation.Mutator;
import ar.edu.unrc.dc.mutation.Ops;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.ExprBinary;
import edu.mit.csail.sdg.ast.Type;
import edu.mit.csail.sdg.parser.CompModule;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import static ar.edu.unrc.dc.mutation.util.TypeChecking.*;

/**
 * Binary Expression Simplifier
 * <p>
 *
 * Replaces a binary expression with its subexpressions
 * <p>
 *
 * <pre>
 *      a op b  => a
 *              => b
 * </pre>
 *
 */
public class BES extends Mutator {

    public BES(CompModule context) {
        super(context);
    }

    @Override
    public Optional<List<Mutation>> visit(ExprBinary x) throws Err {
        Type binaryExpressionType = x.type();
        Type leftType = x.left.type();
        Type rightType = x.right.type();
        List<Mutation> mutations = new LinkedList<>();
        if (!binaryExpressionType.equals(Type.EMPTY) && !leftType.equals(Type.EMPTY) && !rightType.equals(Type.EMPTY)) {
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

    private Optional<List<Mutation>> mutants(ExprBinary x) throws Err {
        if (!mutGenLimitCheck(x))
            return Optional.empty();
        List<Mutation> mutants = new LinkedList<>();
        if (canReplace(x, getType(x.left), strictTypeCheck())) {
            Expr mutant = (Expr) x.left.clone();
            mutants.add(new Mutation(whoiam(), x, mutant));
        }
        if (canReplace(x, getType(x.right), strictTypeCheck())) {
            Expr mutant = (Expr) x.right.clone();
            mutants.add(new Mutation(whoiam(), x, mutant));
        }
        if (mutants.isEmpty())
            return Optional.empty();
        return Optional.of(mutants);
    }

    private boolean strictTypeCheck() {
        Optional<Object> configValue = MutationConfiguration.getInstance().getConfigValue(ConfigKey.OPERATOR_BES_STRICT_TYPE_CHECK);
        return configValue.map(o -> (Boolean) o).orElse((Boolean)ConfigKey.OPERATOR_BES_STRICT_TYPE_CHECK.defaultValue());
    }

    @Override
    protected Ops whoiam() {
        return Ops.BES;
    }

}

package ar.edu.unrc.dc.mutation.op;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.MutationConfiguration;
import ar.edu.unrc.dc.mutation.MutationConfiguration.ConfigKey;
import ar.edu.unrc.dc.mutation.Mutator;
import ar.edu.unrc.dc.mutation.Ops;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.ExprBinary;
import edu.mit.csail.sdg.ast.Type;

/**
 * Binary Expression Simplifier
 * <p>
 *
 * Replaces a binary expression with its subexpressions
 * <p>
 *
 * <pre>
 *      a && b  => a
 *              => b
 * </pre>
 *
 */
public class BES extends Mutator {

    @Override
    public Optional<List<Mutation>> visit(ExprBinary x) throws Err {
        Type binaryExpressionType = x.type();
        Type leftType = x.left.type();
        Type rightType = x.right.type();
        List<Mutation> mutations = new LinkedList<>();
        if (!binaryExpressionType.equals(Type.EMPTY) && !leftType.equals(Type.EMPTY) && !rightType.equals(Type.EMPTY)) {
            Optional<List<Mutation>> mutants = mutants(x);
            if (mutants.isPresent())
                mutations.addAll(mutants.get());
        }
        Optional<List<Mutation>> leftMutations = x.left != null ? x.left.accept(this) : EMPTY;
        Optional<List<Mutation>> rightMutations = x.left != null ? x.left.accept(this) : EMPTY;
        if (leftMutations.isPresent())
            mutations.addAll(leftMutations.get());
        if (rightMutations.isPresent())
            mutations.addAll(rightMutations.get());
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return EMPTY;
    }

    private Optional<List<Mutation>> mutants(ExprBinary x) throws Err {
        Type binaryExpressionType = x.type();
        Type leftType = x.left.type();
        Type rightType = x.right.type();
        List<Mutation> mutants = new LinkedList<>();
        if (strictTypeCheck() && leftType.canOverride(binaryExpressionType))
            mutants.add(new Mutation(Ops.BES, x, x.left));
        else if (leftType.equals(binaryExpressionType))
            mutants.add(new Mutation(Ops.BES, x, x.left));

        if (strictTypeCheck() && rightType.canOverride(binaryExpressionType))
            mutants.add(new Mutation(Ops.BES, x, x.right));
        else if (rightType.equals(binaryExpressionType))
            mutants.add(new Mutation(Ops.BES, x, x.right));
        if (mutants.isEmpty())
            return EMPTY;
        return Optional.of(mutants);
    }

    private boolean strictTypeCheck() {
        Optional<Object> configValue = MutationConfiguration.getInstance().getConfigValue(ConfigKey.OPERATOR_BES_TYPE_CHECK);
        if (configValue.isPresent())
            return (Boolean) configValue.get();
        return false;
    }

}

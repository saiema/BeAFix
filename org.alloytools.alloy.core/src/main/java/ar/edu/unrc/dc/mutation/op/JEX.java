package ar.edu.unrc.dc.mutation.op;

import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.MutationConfiguration;
import ar.edu.unrc.dc.mutation.MutationConfiguration.ConfigKey;
import ar.edu.unrc.dc.mutation.Mutator;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.ExprBinary;
import edu.mit.csail.sdg.parser.CompModule;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;


public abstract class JEX extends Mutator {

    protected JEX(CompModule context) {
        super(context);
    }

    @Override
    public Optional<List<Mutation>> visit(ExprBinary x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        if (isMutable(x)) {
            Optional<List<Mutation>> mutantsLeft = generateMutants(x, x.left);
            Optional<List<Mutation>> mutantsRight = generateMutants(x, x.right);
            mutantsLeft.ifPresent(mutations::addAll);
            mutantsRight.ifPresent(mutations::addAll);
        }
        Optional<List<Mutation>> leftMutations = visitThis(x.left);
        Optional<List<Mutation>> rightMutations = visitThis(x.right);
        leftMutations.ifPresent(mutations::addAll);
        rightMutations.ifPresent(mutations::addAll);
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return Optional.empty();
    }

    protected abstract boolean isMutable(Expr x);

    protected abstract Optional<List<Mutation>> generateMutants(Expr from, Expr replace);

    protected boolean strictTypeCheck() {
        Optional<Object> configValue = MutationConfiguration.getInstance().getConfigValue(ConfigKey.OPERATOR_JEX_STRICT_TYPE_CHECK);
        return configValue.map(o -> (Boolean) o).orElse((Boolean) ConfigKey.OPERATOR_JEX_STRICT_TYPE_CHECK.defaultValue());
    }

    protected boolean checkJoin(ExprBinary from, Expr replace, Expr with) {
        return true;
//        Type withType = getType(with);
//        return TypeChecking.canReplace(replace, withType, strictTypeCheck());
    }

}

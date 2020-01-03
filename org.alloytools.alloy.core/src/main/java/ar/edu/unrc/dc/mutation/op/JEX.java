package ar.edu.unrc.dc.mutation.op;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.MutationConfiguration;
import ar.edu.unrc.dc.mutation.MutationConfiguration.ConfigKey;
import ar.edu.unrc.dc.mutation.Mutator;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.ExprBinary;
import edu.mit.csail.sdg.ast.Type;
import edu.mit.csail.sdg.parser.CompModule;


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
            if (mutantsLeft.isPresent()) {
                mutations.addAll(mutantsLeft.get());
            }
            if (mutantsRight.isPresent()) {
                mutations.addAll(mutantsRight.get());
            }
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

    protected abstract boolean isMutable(Expr x);

    protected abstract Optional<List<Mutation>> generateMutants(Expr from, Expr replace);

    protected boolean strictTypeCheck() {
        Optional<Object> configValue = MutationConfiguration.getInstance().getConfigValue(ConfigKey.OPERATOR_JEX_STRICT_TYPE_CHECK);
        if (configValue.isPresent())
            return (Boolean) configValue.get();
        return false;
    }

    protected boolean checkJoin(ExprBinary from, Expr replace, Expr with) {
        Type withType = getType(with);
        if (from.left.getID() == replace.getID()) {
            //check with joined from.right
            Type rightType = getType(from.right);
            return !emptyOrNone(withType.join(rightType));
        } else if (from.right.getID() == replace.getID()) {
            //check from.left joined with
            Type leftType = getType(from.left);
            return !emptyOrNone(leftType.join(withType));
        } else {
            throw new IllegalArgumentException("replace expression is neither the left nor the right expression of from");
        }
    }

}

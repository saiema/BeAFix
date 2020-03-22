package ar.edu.unrc.dc.mutation.op;

import ar.edu.unrc.dc.mutation.*;
import ar.edu.unrc.dc.mutation.MutationConfiguration.ConfigKey;
import ar.edu.unrc.dc.mutation.util.TypeChecking;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.*;
import edu.mit.csail.sdg.parser.CompModule;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import static ar.edu.unrc.dc.mutation.util.ContextExpressionExtractor.getCompatibleVariablesFor;

public class VCR extends Mutator {

    public VCR(CompModule context) {
        super(context);
    }

    @Override
    public Optional<List<Mutation>> visit(ExprVar x) throws Err {
        if (parentIsBinaryJoinExpression(x))
            return Optional.empty();
        return generateMutations(x);
    }

    @Override
    public Optional<List<Mutation>> visit(ExprConstant x) throws Err {
        if (parentIsBinaryJoinExpression(x))
            return Optional.empty();
        return generateMutations(x);
    }

    private Optional<List<Mutation>> generateMutations(Expr x) throws Err {
        if (!mutGenLimitCheck(x))
            return Optional.empty();
        Optional<List<Expr>> replacements;
        try {
            replacements = getCompatibleVariablesFor(x, strictTypeCheck());
        } catch (CheatingIsBadMkay e) {
            throw new Error("There was a problem obtaining replacements", e);
        }
        List<Mutation> mutations = new LinkedList<>();
        if (replacements.isPresent()) {
            for (Expr r : replacements.get()) {
                if (x.toString().compareTo(r.toString()) == 0)
                    continue;
                r = (Expr) r.clone();
                r.newID();
                if (TypeChecking.canReplace(x, r, strictTypeCheck()))
                    mutations.add(new Mutation(whoiam(), x, r));
            }
        }
        return mutations.isEmpty() ? Optional.empty() : Optional.of(mutations);
    }

    private boolean parentIsBinaryJoinExpression(Expr x) {
        if (x instanceof ExprBinary)
            throw new IllegalArgumentException("You should'n be calling this method with a binary expression");
        Browsable current = x.getBrowsableParent();
        while (current != null) {
            if (current instanceof Func)
                return false;
            if (current instanceof Command)
                return false;
            if (current instanceof ExprBinary) {
                ExprBinary currAsBinary = (ExprBinary) current;
                return currAsBinary.op.equals(ExprBinary.Op.JOIN);
            }
            current = current.getBrowsableParent();
        }
        return false;
    }

    protected boolean strictTypeCheck() {
        Optional<Object> configValue = MutationConfiguration.getInstance().getConfigValue(ConfigKey.OPERATOR_VCR_STRICT_TYPE_CHECK);
        return configValue.map(o -> (Boolean) o).orElse((Boolean) ConfigKey.OPERATOR_VCR_STRICT_TYPE_CHECK.defaultValue());
    }

    @Override
    protected Ops whoiam() {
        return Ops.VCR;
    }

}

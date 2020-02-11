package ar.edu.unrc.dc.mutation.op;

import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.MutationConfiguration;
import ar.edu.unrc.dc.mutation.Mutator;
import ar.edu.unrc.dc.mutation.Ops;
import ar.edu.unrc.dc.mutation.util.TypeChecking;
import ar.edu.unrc.dc.mutation.MutationConfiguration.ConfigKey;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.ExprBinary;
import edu.mit.csail.sdg.ast.ExprBinary.Op;
import edu.mit.csail.sdg.parser.CompModule;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

/**
 * Equality Membership Operator Replacement
 * <p>
 * This operator will replace equality (==/!=) operators and membership
 * operators (in/!in) according to the following rules:
 * <li>{@code ==} -> {@code in}</li>
 * <li>{@code !=} -> {@code !in}</li>
 * <li>{@code in} -> {@code ==}</li>
 * <li>{@code !in} -> {@code !=}</li>
 *
 */
public class EMOR extends Mutator {

    public EMOR(CompModule context) {
        super(context);
    }

    @Override
    public Optional<List<Mutation>> visit(ExprBinary x) throws Err {
        ExprBinary mutant = null;
        switch (x.op) {
            case EQUALS : {
                mutant = x.mutateOp(Op.IN);
                break;
            }
            case IN : {
                mutant = x.mutateOp(Op.EQUALS);
                break;
            }
            case NOT_EQUALS : {
                mutant = x.mutateOp(Op.NOT_IN);
                break;
            }
            case NOT_IN : {
                mutant = x.mutateOp(Op.NOT_EQUALS);
                break;
            }
        }
        if (mutant != null && TypeChecking.canReplace(x, mutant, strictTypeCheck()))
            return Optional.of(Arrays.asList(new Mutation(whoiam(), x, mutant)));
        return Optional.empty();
    }

    @Override
    protected Ops whoiam() {
        return Ops.EMOR;
    }

    private boolean strictTypeCheck() {
        Optional<Object> configValue = MutationConfiguration.getInstance().getConfigValue(ConfigKey.OPERATOR_EMOR_STRICT_TYPE_CHECK);
        return configValue.map(o -> (Boolean) o).orElse((Boolean)ConfigKey.OPERATOR_EMOR_STRICT_TYPE_CHECK.defaultValue());
    }

}

package ar.edu.unrc.dc.mutation.op;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.Mutator;
import ar.edu.unrc.dc.mutation.Ops;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.ExprBinary;
import edu.mit.csail.sdg.ast.ExprBinary.Op;
import edu.mit.csail.sdg.parser.CompModule;

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
        switch (x.op) {
            case EQUALS : {
                ExprBinary mutant = x.mutateOp(Op.IN);
                return Optional.of(Arrays.asList(new Mutation(whoiam(), x, mutant)));
            }
            case IN : {
                ExprBinary mutant = x.mutateOp(Op.EQUALS);
                return Optional.of(Arrays.asList(new Mutation(whoiam(), x, mutant)));
            }
            case NOT_EQUALS : {
                ExprBinary mutant = x.mutateOp(Op.NOT_IN);
                return Optional.of(Arrays.asList(new Mutation(whoiam(), x, mutant)));
            }
            case NOT_IN : {
                ExprBinary mutant = x.mutateOp(Op.NOT_EQUALS);
                return Optional.of(Arrays.asList(new Mutation(whoiam(), x, mutant)));
            }
            default :
                return EMPTY;
        }
    }

    @Override
    protected Ops whoiam() {
        return Ops.EMOR;
    }

}

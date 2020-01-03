package ar.edu.unrc.dc.mutation.op;

import java.util.List;

import ar.edu.unrc.dc.mutation.Ops;
import edu.mit.csail.sdg.ast.ExprUnary;
import edu.mit.csail.sdg.ast.ExprUnary.Op;
import edu.mit.csail.sdg.parser.CompModule;

/**
 * Relational Unary Operator Replacement
 * <p>
 *
 * Replaces a unary relational operator in a unary expression, relational unary
 * operators being:
 * <li>transpose (~)</li>
 * <li>closure (^)</li>
 * <li>reflexive closure (*)</li>
 */
public class RUOR extends UnaryOpReplacer {

    public RUOR(CompModule context) {
        super(context);
    }

    @Override
    protected Ops whoiam() {
        return Ops.RUOR;
    }

    @Override
    protected boolean canMutate(ExprUnary x) {
        return isUnaryRelationalExpression(x);
    }

    @Override
    protected List<Op> getOperators() {
        return RELATIONAL_UNARY_OPS;
    }

}

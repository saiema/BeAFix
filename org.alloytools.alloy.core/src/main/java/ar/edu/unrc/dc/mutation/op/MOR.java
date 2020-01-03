package ar.edu.unrc.dc.mutation.op;

import java.util.List;

import ar.edu.unrc.dc.mutation.Ops;
import edu.mit.csail.sdg.ast.ExprUnary;
import edu.mit.csail.sdg.ast.ExprUnary.Op;
import edu.mit.csail.sdg.parser.CompModule;

/**
 * Multiplicity Operator Replacer
 * <p>
 *
 * Replaces a multiplicity operator in a unary expression, multiplicity
 * operators being:
 * <li>no</li>
 * <li>some</li>
 * <li>lone/li>
 * <li>one</li>
 *
 */
public class MOR extends UnaryOpReplacer {

    public MOR(CompModule context) {
        super(context);
    }

    @Override
    protected Ops whoiam() {
        return Ops.MOR;
    }

    @Override
    protected boolean canMutate(ExprUnary x) {
        return isMultiplicityExpression(x);
    }

    @Override
    protected List<Op> getOperators() {
        return MULTIPLICITY_OPERATORS;
    }

}

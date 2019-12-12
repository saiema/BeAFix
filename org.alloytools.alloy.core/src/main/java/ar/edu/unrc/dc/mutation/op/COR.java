package ar.edu.unrc.dc.mutation.op;

import java.util.List;

import ar.edu.unrc.dc.mutation.Ops;
import edu.mit.csail.sdg.ast.ExprBinary;
import edu.mit.csail.sdg.ast.ExprBinary.Op;
import edu.mit.csail.sdg.parser.CompModule;

/**
 * Conditional Operator Replacement
 * <p>
 *
 * Replaces a conditional operator in a binary expression, conditional operators
 * being:
 * <li>AND (&&)</li>
 * <li>OR (||)</li>
 * <li>Implies (=>)</li>
 * <li>If and only if (<=>)</li>
 */
public class COR extends BinOpReplacer {

    public COR(CompModule context) {
        super(context);
    }

    @Override
    protected boolean canMutate(ExprBinary x) {
        return isConditionalExpression(x);
    }

    @Override
    protected List<Op> getOperators() {
        return CONDITIONAL_OPS;
    }

    @Override
    protected Ops whoiam() {
        return Ops.COR;
    }

}

package ar.edu.unrc.dc.mutation.op;

import java.util.List;

import ar.edu.unrc.dc.mutation.Ops;
import edu.mit.csail.sdg.ast.ExprBinary;
import edu.mit.csail.sdg.ast.ExprBinary.Op;
import edu.mit.csail.sdg.parser.CompModule;


/**
 * Arithmetic Operator Replacement Binary
 * <p>
 *
 * Replaces an arithmetic operator in a binary expression, binary arithmetic
 * operators being:
 * <li>divide</li>
 * <li>multiply</li>
 * <li>remainder</li>
 * <li>plus (+)</li>
 * <li>minus (-)</li>
 *
 */
public class AORB extends BinOpReplacer {

    public AORB(CompModule context) {
        super(context);
    }

    @Override
    protected boolean canMutate(ExprBinary x) {
        return isArithmeticBinaryExpression(x);
    }

    @Override
    protected List<Op> getOperators() {
        return ARITHMETIC_BINARY_OPS;
    }

    @Override
    protected Ops whoiam() {
        return Ops.AORB;
    }

}

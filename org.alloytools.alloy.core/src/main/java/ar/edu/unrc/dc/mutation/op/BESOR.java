package ar.edu.unrc.dc.mutation.op;

import java.util.List;

import ar.edu.unrc.dc.mutation.Ops;
import edu.mit.csail.sdg.ast.ExprBinary;
import edu.mit.csail.sdg.ast.ExprBinary.Op;
import edu.mit.csail.sdg.parser.CompModule;

/**
 * Binary Expression Set Operator Replacer
 * <p>
 *
 * Replaces a set operator in a binary expression, binary set operators being:
 * <li>join(.)</li>
 * <li>union(+)</li>
 * <li>diff<(-)/li>
 * <li>intersection (&)</li>
 * <li>membership(in)</li>
 * <li>overriding(++)</li>
 *
 */
public class BESOR extends BinOpReplacer {

    public BESOR(CompModule context) {
        super(context);
    }

    @Override
    protected boolean canMutate(ExprBinary x) {
        return isSetBinaryExpression(x);
    }

    @Override
    protected List<Op> getOperators() {
        return SET_OPERATORS;
    }

    @Override
    protected Ops whoiam() {
        return Ops.BESOR;
    }

}

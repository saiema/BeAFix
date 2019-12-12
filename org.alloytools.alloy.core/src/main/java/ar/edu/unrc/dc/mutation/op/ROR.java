package ar.edu.unrc.dc.mutation.op;

import java.util.List;

import ar.edu.unrc.dc.mutation.Ops;
import edu.mit.csail.sdg.ast.ExprBinary;
import edu.mit.csail.sdg.ast.ExprBinary.Op;
import edu.mit.csail.sdg.parser.CompModule;

/**
 * Relational Operator Replacement
 * <p>
 *
 * Replaces a relational operator in a binary expression, relational operators
 * being:
 * <li>Equal (=)</li>
 * <li>Not Equal (!=)</li>
 * <li>Greater (>)</li>
 * <li>Not Greater (!>)</li>
 * <li>Greater or equal (>=)</li>
 * <li>Not Greater or equal (!>=)</li>
 * <li>Less (<)</li>
 * <li>Not Less (!<)</li>
 * <li>Less or equal (=<)</li>
 * <li>Not Less or equal (!=<)</li>
 *
 */
public class ROR extends BinOpReplacer {

    public ROR(CompModule context) {
        super(context);
    }

    @Override
    protected boolean canMutate(ExprBinary x) {
        return isRelationalExpression(x);
    }

    @Override
    protected List<Op> getOperators() {
        return RELATIONAL_OPS;
    }

    @Override
    protected Ops whoiam() {
        return Ops.ROR;
    }

}

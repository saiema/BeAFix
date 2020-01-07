package ar.edu.unrc.dc.mutation.op;

import java.util.List;

import ar.edu.unrc.dc.mutation.Ops;
import edu.mit.csail.sdg.ast.ExprBinary;
import edu.mit.csail.sdg.ast.ExprBinary.Op;
import edu.mit.csail.sdg.ast.Type;
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
    protected boolean validate(ExprBinary original, Op newOperator) {
        Type left = original.left.type();
        Type right = original.right.type();
        switch(newOperator) {
            case EQUALS:
            case NOT_EQUALS:
                return true;
            case LT:
            case LTE:
            case GT:
            case GTE:
            case NOT_LT:
            case NOT_LTE:
            case NOT_GT:
            case NOT_GTE:
                return (left.is_int() || left.is_small_int()) && (right.is_int() || right.is_small_int());
            default: throw new IllegalArgumentException("The new operator is not a supported one by this mutator");
        }
    }

    @Override
    protected Ops whoiam() {
        return Ops.ROR;
    }

}

package ar.edu.unrc.dc.mutation.op;

import java.util.List;

import ar.edu.unrc.dc.mutation.Ops;
import edu.mit.csail.sdg.ast.ExprBinary;
import edu.mit.csail.sdg.ast.ExprBinary.Op;
import edu.mit.csail.sdg.ast.Type;
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
    protected boolean validate(ExprBinary original, Op newOperator) {
        Type left = original.left.type();
        Type right = original.right.type();
        switch(newOperator) {
            case JOIN:
                return !emptyOrNone(left.join(right));
            case INTERSECT:
                return !emptyOrNone(left.intersect(right));
            case PLUSPLUS:
            case PLUS:
                return !emptyOrNone(left.unionWithCommonArity(right));
            case MINUS:
                return !emptyOrNone(left.pickCommonArity(right));
            case IN:
                return left.hasCommonArity(right);
            default: throw new IllegalArgumentException("The new operator is not a supported one by this mutator");
        }
    }

    @Override
    protected Ops whoiam() {
        return Ops.BESOR;
    }

}

package ar.edu.unrc.dc.mutation.op;

import ar.edu.unrc.dc.mutation.Ops;
import edu.mit.csail.sdg.ast.ExprBinary;
import edu.mit.csail.sdg.ast.ExprBinary.Op;
import edu.mit.csail.sdg.ast.Type;
import edu.mit.csail.sdg.parser.CompModule;

import java.util.List;

import static ar.edu.unrc.dc.mutation.util.TypeChecking.emptyOrNone;

/**
 * Binary Expression Set Operator Replacer
 * <p>
 *
 * Replaces a set operator in a binary expression, binary set operators being:
 * <li>join(.)</li>
 * <li>union(+)</li>
 * <li>diff<(-)/li>
 * <li>intersection (&)</li>
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
            case ARROW:
                return !emptyOrNone(original.left.type().product(original.right.type()));
            default: throw new IllegalArgumentException("The new operator is not a supported one by this mutator");
        }
    }

    @Override
    protected Ops whoiam() {
        return Ops.BESOR;
    }

}

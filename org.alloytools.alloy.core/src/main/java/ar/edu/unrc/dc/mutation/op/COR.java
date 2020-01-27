package ar.edu.unrc.dc.mutation.op;

import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.Ops;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.ExprBinary;
import edu.mit.csail.sdg.ast.ExprBinary.Op;
import edu.mit.csail.sdg.ast.ExprList;
import edu.mit.csail.sdg.parser.CompModule;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

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
    public Optional<List<Mutation>> visit(ExprList x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        switch (x.op) {
            case AND: {
                ExprList mutant = x.mutateOp(ExprList.Op.OR);
                mutations.add(new Mutation(whoiam(), x, mutant));
                break;
            }
            case OR: {
                ExprList mutant = x.mutateOp(ExprList.Op.AND);
                mutations.add(new Mutation(whoiam(), x, mutant));
                break;
            }
        }
        for (Expr arg : x.args) {
            Optional<List<Mutation>> argMutations = arg.accept(this);
            argMutations.ifPresent(mutations::addAll);
        }
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return EMPTY;
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
    protected boolean validate(ExprBinary original, Op newOperator) {
        return true;
    }

    @Override
    protected Ops whoiam() {
        return Ops.COR;
    }

}

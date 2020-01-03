package ar.edu.unrc.dc.mutation.op;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.Ops;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.ExprBinary;
import edu.mit.csail.sdg.parser.CompModule;

/**
 * Join Expression Replacement
 * <p>
 *
 * Given an expression with at least one join this operator will replace a
 * subexpression (with 0 joins) with another (with 0 joins) As an example, the
 * expression
 *
 * <pre>
 * a.b
 * </pre>
 *
 * can be mutated to
 *
 * <pre>
 * a.x
 * </pre>
 *
 * but not to
 *
 * <pre>
 * x.y
 * </pre>
 *
 */
public class JER extends JEX {

    public JER(CompModule context) {
        super(context);
    }

    @Override
    protected Optional<List<Mutation>> generateMutants(Expr from, Expr replace) {
        List<Mutation> mutations = new LinkedList<>();
        ExprBinary original = (ExprBinary) from;
        if (!isMemberOfBinaryExpression(original, replace))
            throw new IllegalArgumentException("replace expression must be either the left or right part of the from expression");
        Optional<List<Expr>> replacements = getCompatibleVariablesFor(replace, strictTypeCheck());
        if (replacements.isPresent()) {
            for (Expr r : replacements.get()) {
                r = (Expr) r.clone();
                r.newID();
                ExprBinary mutant = null;
                if (original.left.getID() == replace.getID() && checkJoin(original, replace, r))
                    mutant = original.mutateLeft(r);
                if (original.right.getID() == replace.getID() && checkJoin(original, replace, r))
                    mutant = original.mutateRight(r);
                if (mutant == null || from.toString().equals(mutant.toString()))
                    continue;
                mutations.add(new Mutation(whoiam(), from, mutant));
            }
        }
        return mutations.isEmpty() ? Optional.empty() : Optional.of(mutations);
    }

    @Override
    protected boolean isMutable(Expr x) {
        return (x instanceof ExprBinary) && ((ExprBinary) x).op == ExprBinary.Op.JOIN;
    }

    @Override
    protected Ops whoiam() {
        return Ops.JER;
    }

}

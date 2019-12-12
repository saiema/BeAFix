package ar.edu.unrc.dc.mutation.op;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.Ops;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.ExprBinary;
import edu.mit.csail.sdg.ast.Type;
import edu.mit.csail.sdg.parser.CompModule;

/**
 * Join Expression Shortener
 * <p>
 *
 * Given an expression with at least one join this operator will reduce the
 * expression by removing a subexpression with 0 joins, or by replacing one
 * expression with 1 join by another with 0 joins. As an example, the expression
 *
 * <pre>
 * a.b
 * </pre>
 *
 * can be mutated to
 *
 * <pre>
 * a
 * </pre>
 *
 * and also to
 *
 * <pre>
 * x
 * </pre>
 *
 */
public class JES extends JEX {

    public JES(CompModule context) {
        super(context);
    }

    @Override
    public Optional<List<Mutation>> visit(ExprBinary x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        if (isMutable(x)) {
            Optional<List<Mutation>> mutants = generateMutants(x, x);
            if (mutants.isPresent()) {
                mutations.addAll(mutants.get());
            }
        }
        Optional<List<Mutation>> leftMutations = x.left.accept(this);
        Optional<List<Mutation>> rightMutations = x.right.accept(this);
        if (leftMutations.isPresent())
            mutations.addAll(leftMutations.get());
        if (rightMutations.isPresent())
            mutations.addAll(rightMutations.get());
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return EMPTY;
    }

    @Override
    protected boolean isMutable(Expr x) {
        return (x instanceof ExprBinary) && ((ExprBinary) x).op == ExprBinary.Op.JOIN;
    }

    @Override
    protected Optional<List<Mutation>> generateMutants(Expr from, Expr replace) {
        //from and replace must be the same
        if (from.getID() != replace.getID())
            throw new IllegalArgumentException("from and replace must be the same");
        List<Mutation> mutations = new LinkedList<>();
        if (from instanceof ExprBinary) {
            Optional<List<Expr>> replacements = obtainReplacements((ExprBinary) replace);
            if (replacements.isPresent()) {
                for (Expr r : replacements.get()) {
                    mutations.add(new Mutation(Ops.JES, replace, r));
                }
            }
        }
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return Optional.empty();
    }

    private Optional<List<Expr>> obtainReplacements(ExprBinary replace) {
        List<Expr> replacements = new LinkedList<>();
        if (typeCheck(replace, replace.left, true)) {
            replacements.add(replace.left);
        }
        if (typeCheck(replace, replace.right, true)) {
            replacements.add(replace.right);
        }
        Optional<List<Expr>> simpleReplacementsOp = getCompatibleVariablesFor(replace, strictTypeCheck());
        if (simpleReplacementsOp.isPresent())
            replacements.addAll(simpleReplacementsOp.get());
        if (!replacements.isEmpty()) {
            return Optional.of(replacements);
        }
        return Optional.empty();
    }

    private boolean typeCheck(Expr exprA, Expr exprB, boolean replace) {
        Type replacementType = getType(exprB);
        if (replace) {
            return super.compatibleVariablesChecker(exprA, exprB, replacementType, strictTypeCheck());
        } else {
            Type joinedType = getType(exprA).join(replacementType);
            return !emptyOrNone(joinedType);
        }
    }

}

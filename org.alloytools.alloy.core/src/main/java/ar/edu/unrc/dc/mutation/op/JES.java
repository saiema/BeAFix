package ar.edu.unrc.dc.mutation.op;

import ar.edu.unrc.dc.mutation.CheatingIsBadMkay;
import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.Ops;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.ExprBinary;
import edu.mit.csail.sdg.parser.CompModule;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import static ar.edu.unrc.dc.mutation.util.ContextExpressionExtractor.getCompatibleVariablesFor;
import static ar.edu.unrc.dc.mutation.util.TypeChecking.canReplace;

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
            mutants.ifPresent(mutations::addAll);
        }
        Optional<List<Mutation>> leftMutations = visitThis(x.left);
        Optional<List<Mutation>> rightMutations = visitThis(x.right);
        leftMutations.ifPresent(mutations::addAll);
        rightMutations.ifPresent(mutations::addAll);
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return Optional.empty();
    }

    @Override
    protected boolean isMutable(Expr x) {
        return (x instanceof ExprBinary) && ((ExprBinary) x).op == ExprBinary.Op.JOIN;
    }

    @Override
    protected Optional<List<Mutation>> generateMutants(Expr from, Expr replace) throws Err {
        if (!mutGenLimitCheck(from))
            return Optional.empty();
        //from and replace must be the same
        if (from.getID() != replace.getID())
            throw new IllegalArgumentException("from and replace must be the same");
        List<Mutation> mutations = new LinkedList<>();
        if (from instanceof ExprBinary) {
            Optional<List<Expr>> replacements;
            try {
                replacements = obtainReplacements((ExprBinary) replace);
            } catch (CheatingIsBadMkay e) {
                throw new Error("There was a problem obtaining replacements", e);
            }
            if (replacements.isPresent()) {
                for (Expr r : replacements.get()) {
                    mutations.add(new Mutation(whoiam(), replace, r));
                }
            }
        }
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return Optional.empty();
    }

    private Optional<List<Expr>> obtainReplacements(ExprBinary replace) throws CheatingIsBadMkay {
        List<Expr> replacements = new LinkedList<>();
        if (size(replace.right) == 1 && canReplace(replace, replace.left,strictTypeCheck())) {
            replacements.add(replace.left);
        }
        if (size(replace.left) == 1 && canReplace(replace, replace.right, strictTypeCheck())) {
            replacements.add(replace.right);
        }
        if (size(replace) == 2) {
            Optional<List<Expr>> simpleReplacementsOp = getCompatibleVariablesFor(replace, strictTypeCheck());
            simpleReplacementsOp.ifPresent(replacements::addAll);
        }
        if (!replacements.isEmpty()) {
            return Optional.of(replacements);
        }
        return Optional.empty();
    }

    private int size(Expr x) {
        if (x instanceof ExprBinary) {
            ExprBinary xAsBinary = (ExprBinary) x;
            if (xAsBinary.op.equals(ExprBinary.Op.JOIN))
                return size(xAsBinary.left) + size(xAsBinary.right);
        }
        return 1;
    }

    @Override
    protected Ops whoiam() {
        return Ops.JES;
    }

}

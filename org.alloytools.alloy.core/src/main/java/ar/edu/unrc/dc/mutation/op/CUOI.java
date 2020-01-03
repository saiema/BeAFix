package ar.edu.unrc.dc.mutation.op;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.Mutator;
import ar.edu.unrc.dc.mutation.Ops;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.ExprBinary;
import edu.mit.csail.sdg.ast.ExprCall;
import edu.mit.csail.sdg.ast.ExprUnary;
import edu.mit.csail.sdg.ast.ExprVar;
import edu.mit.csail.sdg.parser.CompModule;

/**
 * Conditional Unary Operator Insertion
 * <p>
 *
 * Negates a boolean expression
 *
 */
public class CUOI extends Mutator {

    public CUOI(CompModule context) {
        super(context);
    }

    @Override
    public Optional<List<Mutation>> visit(ExprCall x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        Optional<Mutation> mutation = getMutation(x);
        if (mutation.isPresent())
            mutations.add(mutation.get());
        for (Expr arg : x.args) {
            Optional<List<Mutation>> argMutations = arg.accept(this);
            if (argMutations.isPresent())
                mutations.addAll(argMutations.get());
        }
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return EMPTY;
    }

    @Override
    public Optional<List<Mutation>> visit(ExprUnary x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        Optional<Mutation> mutation = getMutation(x);
        if (mutation.isPresent())
            mutations.add(mutation.get());
        Optional<List<Mutation>> subMutations = x.sub.accept(this);
        if (subMutations.isPresent())
            mutations.addAll(subMutations.get());
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return EMPTY;
    }

    @Override
    public Optional<List<Mutation>> visit(ExprBinary x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        Optional<Mutation> mutation = getMutation(x);
        if (mutation.isPresent())
            mutations.add(mutation.get());
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
    public Optional<List<Mutation>> visit(ExprVar x) throws Err {
        Optional<Mutation> mutant = getMutation(x);
        if (mutant.isPresent()) {
            return Optional.of(Arrays.asList(mutant.get()));
        }
        return EMPTY;
    }

    private boolean isBooleanExpression(Expr x) {
        return getType(x).is_bool;
    }

    private Optional<Mutation> getMutation(Expr x) {
        if (isBooleanExpression(x)) {
            return Optional.of(new Mutation(whoiam(), x, ExprUnary.Op.NOT.make(x.pos(), (Expr) x.clone())));
        }
        return Optional.empty();
    }

    @Override
    protected Ops whoiam() {
        return Ops.CUOI;
    }

}

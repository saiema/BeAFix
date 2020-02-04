package ar.edu.unrc.dc.mutation.op;

import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.Mutator;
import ar.edu.unrc.dc.mutation.Ops;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.*;
import edu.mit.csail.sdg.parser.CompModule;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import static ar.edu.unrc.dc.mutation.util.TypeChecking.getType;

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
        mutation.ifPresent(mutations::add);
        for (Expr arg : x.args) {
            Optional<List<Mutation>> argMutations = arg.accept(this);
            argMutations.ifPresent(mutations::addAll);
        }
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return EMPTY;
    }

    @Override
    public Optional<List<Mutation>> visit(ExprUnary x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        Optional<Mutation> mutation = getMutation(x);
        mutation.ifPresent(mutations::add);
        Optional<List<Mutation>> subMutations = x.sub.accept(this);
        subMutations.ifPresent(mutations::addAll);
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return EMPTY;
    }

    @Override
    public Optional<List<Mutation>> visit(ExprBinary x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        Optional<Mutation> mutation = getMutation(x);
        mutation.ifPresent(mutations::add);
        Optional<List<Mutation>> leftMutations = x.left.accept(this);
        Optional<List<Mutation>> rightMutations = x.right.accept(this);
        leftMutations.ifPresent(mutations::addAll);
        rightMutations.ifPresent(mutations::addAll);
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return EMPTY;
    }

    @Override
    public Optional<List<Mutation>> visit(ExprVar x) throws Err {
        Optional<Mutation> mutant = getMutation(x);
        return mutant.map(mutation -> Optional.of(Arrays.asList(mutation))).orElse(EMPTY);
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

package ar.edu.unrc.dc.mutation.op;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.Mutator;
import ar.edu.unrc.dc.mutation.Ops;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.ExprBinary;
import edu.mit.csail.sdg.ast.ExprUnary;
import edu.mit.csail.sdg.ast.ExprVar;
import edu.mit.csail.sdg.ast.Type;
import edu.mit.csail.sdg.parser.CompModule;

/**
 * Relational Unary Operator Insertion
 * <p>
 *
 * Inserts a unary relational operator in a compatible expression, relational
 * unary operators being:
 * <li>transpose (~)</li>
 * <li>closure (^)</li>
 * <li>reflexive closure (*)</li>
 */
public class RUOI extends Mutator {

    //    @Override
    //    public Optional<List<Mutation>> visit(ExprCall x) throws Err {
    //        return null;
    //    }

    public RUOI(CompModule context) {
        super(context);
    }

    @Override
    public Optional<List<Mutation>> visit(ExprUnary x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        Optional<List<Mutation>> mutants = mutate(x);
        if (mutants.isPresent())
            mutations.addAll(mutants.get());
        Optional<List<Mutation>> subExprMutations = x.sub.accept(this);
        if (subExprMutations.isPresent())
            mutations.addAll(subExprMutations.get());
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return EMPTY;
    }

    @Override
    public Optional<List<Mutation>> visit(ExprBinary x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        Optional<List<Mutation>> mutants = mutate(x);
        if (mutants.isPresent())
            mutations.addAll(mutants.get());
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
        List<Mutation> mutations = new LinkedList<>();
        Optional<List<Mutation>> mutants = mutate(x);
        if (mutants.isPresent())
            mutations.addAll(mutants.get());
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return EMPTY;
    }

    private Optional<List<Mutation>> mutate(Expr x) {
        List<Mutation> mutations = new LinkedList<>();
        Type xtype = x.type();
        if (xtype.transpose() != Type.EMPTY)
            mutations.add(new Mutation(Ops.RUOI, x, x.transpose()));
        if (xtype.closure() != Type.EMPTY) {
            mutations.add(new Mutation(Ops.RUOI, x, x.closure()));
            mutations.add(new Mutation(Ops.RUOI, x, x.reflexiveClosure()));
        }
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return EMPTY;

    }

}

package ar.edu.unrc.dc.mutation.op;

import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.Mutator;
import ar.edu.unrc.dc.mutation.Ops;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.*;
import edu.mit.csail.sdg.parser.CompModule;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import static ar.edu.unrc.dc.mutation.util.TypeChecking.emptyOrNone;
import static ar.edu.unrc.dc.mutation.util.TypeChecking.getType;

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


    public RUOI(CompModule context) {
        super(context);
    }

    @Override
    public Optional<List<Mutation>> visit(ExprCall x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        Optional<List<Mutation>> mutants = mutate(x);
        mutants.ifPresent(mutations::addAll);
        for (Expr arg : x.args) {
            Optional<List<Mutation>> argMutations = visitThis(arg);
            argMutations.ifPresent(mutations::addAll);
        }
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return Optional.empty();
    }

    @Override
    public Optional<List<Mutation>> visit(ExprUnary x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        Optional<List<Mutation>> mutants = mutate(x);
        mutants.ifPresent(mutations::addAll);
        Optional<List<Mutation>> subExprMutations = visitThis(x.sub);
        subExprMutations.ifPresent(mutations::addAll);
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return Optional.empty();
    }

    @Override
    public Optional<List<Mutation>> visit(ExprBinary x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        Optional<List<Mutation>> mutants = mutate(x);
        mutants.ifPresent(mutations::addAll);
        Optional<List<Mutation>> leftMutations = visitThis(x.left);
        Optional<List<Mutation>> rightMutations = visitThis(x.right);
        leftMutations.ifPresent(mutations::addAll);
        rightMutations.ifPresent(mutations::addAll);
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return Optional.empty();
    }

    @Override
    public Optional<List<Mutation>> visit(ExprVar x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        Optional<List<Mutation>> mutants = mutate(x);
        mutants.ifPresent(mutations::addAll);
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return Optional.empty();
    }

    private Optional<List<Mutation>> mutate(Expr x) {
        if (!mutGenLimitCheck(x))
            return Optional.empty();
        List<Mutation> mutations = new LinkedList<>();
        Type xtype = getType(x);
        if (!emptyOrNone(xtype.transpose()))
            mutations.add(new Mutation(whoiam(), x, ((Expr) x.clone()).transpose()));
        if (!emptyOrNone(xtype.closure())) {
            mutations.add(new Mutation(whoiam(), x, ((Expr) x.clone()).closure()));
            mutations.add(new Mutation(whoiam(), x, ((Expr) x.clone()).reflexiveClosure()));
        }
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return Optional.empty();

    }

    @Override
    protected Ops whoiam() {
        return Ops.RUOI;
    }

}

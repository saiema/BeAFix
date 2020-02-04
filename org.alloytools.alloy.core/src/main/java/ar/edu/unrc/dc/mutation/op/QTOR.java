package ar.edu.unrc.dc.mutation.op;

import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.Mutator;
import ar.edu.unrc.dc.mutation.Ops;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.ExprQt;
import edu.mit.csail.sdg.parser.CompModule;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

/**
 * Quantifier Operator Replacer
 * <p>
 * ExprQt.Op.ALL, ExprQt.Op.LONE, ExprQt.Op.NO, ExprQt.Op.ONE, ExprQt.Op.SOME
 * Replaces the operator in a quantifier expression, these operators are
 * <li>all</li>
 * <li>lone</li>
 * <li>no/li>
 * <li>one</li>
 * <li>some</li>
 * <hr>
 * The {@code comprehension} is not considered for the moment, and the
 * {@code sum} operator is also not considered being the only one that deals
 * with arithmetic expressions
 *
 */
public class QTOR extends Mutator {


    public QTOR(CompModule context) {
        super(context);
    }

    @Override
    public Optional<List<Mutation>> visit(ExprQt x) throws Err {
        if (QUANTIFIER_OPERATORS.contains(x.op)) {
            List<Mutation> mutations = new LinkedList<>();
            Optional<List<Mutation>> mutants = mutants(x);
            mutants.ifPresent(mutations::addAll);
            Optional<List<Mutation>> subMutations = x.sub != null ? x.sub.accept(this) : EMPTY;
            subMutations.ifPresent(mutations::addAll);
            if (!mutations.isEmpty())
                return Optional.of(mutations);
        }
        return EMPTY;
    }

    private Optional<List<Mutation>> mutants(ExprQt x) {
        List<Mutation> mutants = new LinkedList<>();
        for (ExprQt.Op o : QUANTIFIER_OPERATORS) {
            if (x.op.equals(o))
                continue;
            ExprQt mutant = x.mutateOp(o);
            mutants.add(new Mutation(whoiam(), x, mutant));
        }
        return Optional.of(mutants);
    }

    @Override
    protected Ops whoiam() {
        return Ops.QTOR;
    }

}

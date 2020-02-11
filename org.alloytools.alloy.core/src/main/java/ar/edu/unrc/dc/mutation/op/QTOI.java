package ar.edu.unrc.dc.mutation.op;

import ar.edu.unrc.dc.mutation.*;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.*;
import edu.mit.csail.sdg.parser.CompModule;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

/**
 * Quantifier Operator Insertion
 * <p>
 *     Given an expression x, this operator generates
 *     <li>no x</li>
 *     <li>some x</li>
 *     <li>lone x</li>
 *     <li>one x</li>
 * </p>
 */
public class QTOI extends Mutator {

    public QTOI(CompModule context) {
        super(context);
    }

    @Override
    public Optional<List<Mutation>> visit(ExprBinary x) throws Err {
        return generateMutationsFor(x);
    }

    @Override
    public Optional<List<Mutation>> visit(ExprCall x) throws Err {
        return generateMutationsFor(x);
    }

    @Override
    public Optional<List<Mutation>> visit(ExprUnary x) throws Err {
        return generateMutationsFor(x);
    }

    @Override
    public Optional<List<Mutation>> visit(ExprVar x) throws Err {
        return generateMutationsFor(x);
    }

    private Optional<List<Mutation>> generateMutationsFor(Expr x) throws Err {
        try {
            List<Expr> mutants = generateMutants(x);
            List<Mutation> mutations = new LinkedList<>();
            for (Expr m : mutants) {
                mutations.add(new Mutation(whoiam(), x, m));
            }
            return Optional.of(mutations);
        } catch (CheatingIsBadMkay e) {
            throw new Error("There was a problem obtaining mutations", e);
        }
    }

    private List<Expr> generateMutants(Expr x) throws CheatingIsBadMkay {
        List<Expr> mutants = new LinkedList<>();
        if (validateTypeAndOperator(x)) {
            mutants.add(ExprUnary.Op.NO.make(x.pos, Cheats.cheatedClone(x)));
            mutants.add(ExprUnary.Op.SOME.make(x.pos, Cheats.cheatedClone(x)));
            mutants.add(ExprUnary.Op.LONE.make(x.pos, Cheats.cheatedClone(x)));
            mutants.add(ExprUnary.Op.ONE.make(x.pos, Cheats.cheatedClone(x)));
        }
        return mutants;
    }

    private boolean validateTypeAndOperator(Expr x) {
        if (x.type().is_small_int() || x.type().is_int())
            return true;
        if (x.type().is_bool)
            return false;
        if (x.type().equals(Type.FORMULA))
            return false;
        if (x instanceof ExprUnary) {
            switch (((ExprUnary)x).op) {
                case SOMEOF:
                case LONEOF:
                case ONEOF:
                case SETOF:
                case EXACTLYOF:
                case NOT:
                case NO:
                case SOME:
                case LONE:
                case ONE:
                case CARDINALITY:
                case CAST2INT:
                case CAST2SIGINT:
                case NOOP:
                    return false;
                case TRANSPOSE:
                case RCLOSURE:
                case CLOSURE:
                    return true;
            }
        }
        return x.type().size() > 0;
    }

    @Override
    protected Ops whoiam() {
        return Ops.QTOI;
    }
}

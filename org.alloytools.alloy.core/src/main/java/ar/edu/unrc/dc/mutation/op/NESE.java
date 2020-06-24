package ar.edu.unrc.dc.mutation.op;

import ar.edu.unrc.dc.mutation.CheatingIsBadMkay;
import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.Mutator;
import ar.edu.unrc.dc.mutation.Ops;
import ar.edu.unrc.dc.mutation.visitors.SearchExpr;
import ar.edu.unrc.dc.mutation.visitors.VarsAndJoinExtractor;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.*;
import edu.mit.csail.sdg.parser.CompModule;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import static ar.edu.unrc.dc.mutation.Cheats.cheatedClone;

/**
 * Non Empty Set Extender
 * <p>
 * Given an expression A, this operator will add, for each variable or join
 * expression {@code x}, {@code some x} => A
 * <p>
 * For example
 *
 * <pre>
 * some list': List | list'.rest = list and list'.element = e
 * </pre>
 * <p>
 * Would be mutated to
 *
 * <pre>
 * some list': List | (some list'.rest && some list && some list'.element && some e) => list'.rest = list and list'.element = e
 * </pre>
 */
public class NESE extends Mutator {

    public NESE(CompModule context) {
        super(context);
    }

    @Override
    public Optional<List<Mutation>> visit(ExprQt x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        Optional<List<Mutation>> formulaMutations = visitThis(x.sub);
        formulaMutations.ifPresent(mutations::addAll);
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return Optional.empty();
    }

    @Override
    public Optional<List<Mutation>> visit(ExprUnary x) throws Err {
        if (x.op.equals(ExprUnary.Op.NOOP))
            return visitThis(x.sub);
        return generateMutation(x);
    }

    @Override
    public Optional<List<Mutation>> visit(ExprBinary x) throws Err {
        return generateMutation(x);
    }

    @Override
    public Optional<List<Mutation>> visit(ExprCall x) throws Err {
        return generateMutation(x);
    }

    private Optional<List<Mutation>> generateMutation(Expr x) throws Err {
        if (!mutGenLimitCheck(x))
            return Optional.empty();
        if (!x.type().is_bool)
            return Optional.empty();
        try {
            Optional<Expr> mutant = generateMutant(x);
            if (mutant.isPresent())
                return Optional.of(Arrays.asList(new Mutation(whoiam(), x, mutant.get())));
        } catch (CheatingIsBadMkay e) {
            throw new Error("There was a problem obtaining replacement types", e);
        }
        return Optional.empty();
    }

    private Optional<Expr> generateMutant(Expr x) throws CheatingIsBadMkay {
        Optional<List<Expr>> varsAndJoins = getVarsAndJoinsFrom(x);
        Expr some = null;
        if (varsAndJoins.isPresent()) {
            for (Expr v : varsAndJoins.get()) {
                v = cheatedClone(v);
                if (some == null) {
                    some = ExprUnary.Op.SOME.make(null, v);
                } else {
                    some = ExprBinary.Op.AND.make(null, null, some, ExprUnary.Op.SOME.make(null, v));
                }
            }
        }
        if (some != null) {
            Expr originalClone = cheatedClone(x);
            Expr mutant = ExprBinary.Op.IMPLIES.make(null, null, some, originalClone);
            return Optional.of(mutant);
        }
        return Optional.empty();
    }

    private Optional<List<Expr>> getVarsAndJoinsFrom(Expr x) {
        List<Expr> varsAndJoins = new LinkedList<>();
        VarsAndJoinExtractor extractor = new VarsAndJoinExtractor();
        Optional<List<Expr>> allExpressions = extractor.visitThis(x);
        if (allExpressions.isPresent()) {
            for (Expr expr : allExpressions.get()) {
                boolean accepted = true;
                for (Expr expr2 : allExpressions.get()) {
                    if (expr.toString().compareTo(expr2.toString()) == 0)
                        continue;
                    SearchExpr searcher = new SearchExpr(expr) {
                        @Override
                        protected boolean match(Expr a, Expr b) {
                            return a.toString().compareTo(b.toString()) == 0;
                        }
                    };
                    if (searcher.visitThis(expr2)) {
                        accepted = false;
                        break;
                    }
                }
                if (accepted) varsAndJoins.add(expr);
            }
        }
        if (!varsAndJoins.isEmpty())
            return Optional.of(varsAndJoins);
        return Optional.empty();
    }

    @Override
    protected Ops whoiam() {
        return Ops.NESE;
    }

}

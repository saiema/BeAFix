package ar.edu.unrc.dc.mutation.op;

import ar.edu.unrc.dc.mutation.*;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.*;
import edu.mit.csail.sdg.parser.CompModule;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import static ar.edu.unrc.dc.mutation.Cheats.cheatedClone;
import static ar.edu.unrc.dc.mutation.util.ContextExpressionExtractor.*;
import static ar.edu.unrc.dc.mutation.util.TypeChecking.*;

/**
 * Simple Set Extender
 * <p>
 *     Given an expression {@code x} this will generate {@code x + y} where {@code y} is a compatible set expression comprised by defined Signatures and Fields
 * </p>
 */
public class SSE extends Mutator {

    public SSE(CompModule context) {
        super(context);
    }

    @Override
    public Optional<List<Mutation>> visit(ExprBinary x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        Optional<List<Mutation>> mainMutations = generateMutationsFor(x);
        Optional<List<Mutation>> leftMutations = visitThis(x.left);
        Optional<List<Mutation>> rightMutations = visitThis(x.right);
        mainMutations.ifPresent(mutations::addAll);
        leftMutations.ifPresent(mutations::addAll);
        rightMutations.ifPresent(mutations::addAll);
        if (!mutations.isEmpty()) {
            return Optional.of(mutations);
        }
        return Optional.empty();
    }

    @Override
    public Optional<List<Mutation>> visit(ExprCall x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        Optional<List<Mutation>> mainMutations = generateMutationsFor(x);
        mainMutations.ifPresent(mutations::addAll);
        for (Expr arg : x.args) {
            Optional<List<Mutation>> argMutations = visitThis(arg);
            argMutations.ifPresent(mutations::addAll);
        }
        if (!mutations.isEmpty()) {
            return Optional.of(mutations);
        }
        return Optional.empty();
    }

    @Override
    public Optional<List<Mutation>> visit(ExprUnary x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        Optional<List<Mutation>> mainMutations = generateMutationsFor(x);
        Optional<List<Mutation>> subMutations = visitThis(x.sub); //(!x.op.equals(ExprUnary.Op.NOOP))? visitThis(x.sub) : Optional.empty();
        mainMutations.ifPresent(mutations::addAll);
        subMutations.ifPresent(mutations::addAll);
        if (!mutations.isEmpty()) {
            return Optional.of(mutations);
        }
        return Optional.empty();
    }

    @Override
    public Optional<List<Mutation>> visit(ExprVar x) throws Err {
        return generateMutationsFor(x);
    }

    private Optional<List<Mutation>> generateMutationsFor(Expr x) throws Err {
        if (!mutGenLimitCheck(x))
            return Optional.empty();
        if (x instanceof ExprUnary && ((ExprUnary)x).op.equals(ExprUnary.Op.NOOP))
            return Optional.empty();
        try {
            List<Mutation> mutations = new LinkedList<>();
            Optional<List<Expr>> mutants = generateMutants(x);
            if (mutants.isPresent()) {
                for (Expr mutant : mutants.get()) {
                    mutations.add(new Mutation(whoiam(), x, mutant));
                }
            }
            if (!mutations.isEmpty())
                return Optional.of(mutations);
            return Optional.empty();
        } catch (CheatingIsBadMkay e) {
            throw new Error("There was a problem obtaining mutations", e);
        }
    }

    private Optional<List<Expr>> generateMutants(Expr x) throws CheatingIsBadMkay {
        List<Expr> mutants = new LinkedList<>();
        Type leftType = x.type();
        Optional<List<Expr>> rightExpressions = getCompatibleVariablesFor(x, strictTypeCheck());
        if (rightExpressions.isPresent()) {
            for (Expr right : rightExpressions.get()) {
                Type rightType = right.type();
                if (strictTypeCheck() && !leftType.equals(rightType))
                    continue;
                else if (!strictTypeCheck()) {
                    Type unionType = leftType.unionWithCommonArity(rightType);
                    if (emptyOrNone(unionType))
                        continue;
                }
                Expr originalClone = cheatedClone(x);
                Expr rightClone = cheatedClone(right);
                Expr mutant = ExprBinary.Op.PLUS.make(originalClone.pos, null, originalClone, rightClone);
                mutants.add(mutant);
            }
        }
        if (!mutants.isEmpty())
            return Optional.of(mutants);
        return Optional.empty();
    }

    private boolean strictTypeCheck() {
        Optional<Object> configValue = MutationConfiguration.getInstance().getConfigValue(MutationConfiguration.ConfigKey.OPERATOR_SSE_STRICT_TYPE_CHECK);
        return configValue.map(o -> (Boolean) o).orElseGet(() -> (Boolean) MutationConfiguration.ConfigKey.OPERATOR_SSE_STRICT_TYPE_CHECK.defaultValue());
    }

    @Override
    protected Ops whoiam() {
        return Ops.SSE;
    }

}

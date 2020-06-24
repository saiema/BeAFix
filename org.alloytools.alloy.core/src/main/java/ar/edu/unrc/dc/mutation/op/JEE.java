package ar.edu.unrc.dc.mutation.op;

import ar.edu.unrc.dc.mutation.CheatingIsBadMkay;
import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.Ops;
import ar.edu.unrc.dc.mutation.mutantLab.MutantLab;
import ar.edu.unrc.dc.mutation.util.TypeChecking;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.*;
import edu.mit.csail.sdg.parser.CompModule;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import static ar.edu.unrc.dc.mutation.Cheats.cheatedClone;
import static ar.edu.unrc.dc.mutation.util.ContextExpressionExtractor.getAllVariablesFor;
import static ar.edu.unrc.dc.mutation.util.ContextExpressionExtractor.getCompatibleVariablesFor;
import static ar.edu.unrc.dc.mutation.util.TypeChecking.*;

/**
 * Join Expression Extender
 * <p>
 *
 * Given an expression with 0 or more joins this operator will extend it by
 * adding (joining) an subexpression with 0 joins, or by replacing one
 * expression with 0 joins by another with 1 join. As an example, the expression
 *
 * <pre>
 * a.b
 * </pre>
 *
 * can be mutated to
 *
 * <pre>
 * a.b.c
 * </pre>
 *
 * and also to
 *
 * <pre>
 * a.x.y
 * </pre>
 *
 */
public class JEE extends JEX {

    public JEE(CompModule context) {
        super(context);
    }

    @Override
    public Optional<List<Mutation>> visit(ExprUnary x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        Optional<List<Mutation>> unaryMutations = generateMutants(x, x);
        unaryMutations.ifPresent(mutations::addAll);
        Optional<List<Mutation>> subMutations = visitThis(x.sub);
        subMutations.ifPresent(mutations::addAll);
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return Optional.empty();
    }

    @Override
    public Optional<List<Mutation>> visit(ExprVar x) throws Err {
        return generateMutants(x, x);
    }

    @Override
    public Optional<List<Mutation>> visit(ExprCall x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        Optional<List<Mutation>> callMutations = generateMutants(x, x);
        callMutations.ifPresent(mutations::addAll);
        for (Expr arg : x.args) {
            Optional<List<Mutation>> argMutations = visitThis(arg);
            argMutations.ifPresent(mutations::addAll);
        }
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return Optional.empty();
    }

    @Override
    public Optional<List<Mutation>> visit(Sig.Field x) throws Err {
        if (MutantLab.getInstance().isOperatorEnabled(Ops.QTBER))
            return Optional.empty();
        return generateMutants(x, x);
    }

    @Override
    public Optional<List<Mutation>> visit(Sig x) throws Err {
        if (MutantLab.getInstance().isOperatorEnabled(Ops.QTBER))
            return Optional.empty();
        return generateMutants(x, x);
    }

    @Override
    protected boolean isMutable(Expr x) {
        return true;
    }

    @Override
    protected Optional<List<Mutation>> generateMutants(Expr from, Expr replace) throws Err {
        if (!mutGenLimitCheck(from))
            return Optional.empty();
        List<Mutation> mutations = new LinkedList<>();
        if (from instanceof ExprBinary) {
            //replace one subexpression of size 0 with one of size 1 (in terms of joins)
            ExprBinary original = (ExprBinary) from;
            if (!isMemberOfBinaryExpression(original, replace))
                throw new IllegalArgumentException("The expression to replace does not belongs to the from expression");
            Optional<List<Expr>> replacements;
            try {
                replacements = obtainReplacements(replace, false);
            } catch (CheatingIsBadMkay e) {
                throw new Error("There was a problem obtaining mutations", e);
            }
            if (replacements.isPresent()) {
                for (Expr r : replacements.get()) {
                    if (original.left.getID() == replace.getID()) {
                        if (emptyOrNone(getType(r).join(getType(original.right))))
                            continue;
                        Expr mutation = original.mutateLeft(r);
                        if (!canReplace(original, mutation, strictTypeCheck()))
                            continue;
                        mutations.add(new Mutation(whoiam(), original, mutation));
                    } else {
                        if (emptyOrNone(getType(original.left).join(getType(r))))
                            continue;
                        Expr mutation = original.mutateRight(r);
                        if (!canReplace(original, mutation, strictTypeCheck()))
                            continue;
                        mutations.add(new Mutation(whoiam(), original, mutation));
                    }
                }
            }
        } else if (from instanceof ExprUnary || from instanceof ExprVar || from instanceof ExprCall || from instanceof Sig || from instanceof Sig.Field) {
            if (from.getID() != replace.getID())
                throw new IllegalArgumentException("The replace expression for Expr(Unary,HasName,Var,Call) must be the same as the from expression");
            Optional<List<Expr>> replacements;
            try {
                replacements = obtainReplacements(replace, true);
            } catch (CheatingIsBadMkay e) {
                throw new Error("There was a problem obtaining mutations", e);
            }
            if (replacements.isPresent()) {
                for (Expr r : replacements.get()) {
                    if (r instanceof ExprBinary) {
                        mutations.add(new Mutation(whoiam(), from, r));
                    } else {
                        throw new IllegalStateException("Oops, there should only be ExprBinary expressions for replacements");
                    }
                }
            }
        }
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return Optional.empty();
    }

    private Optional<List<Expr>> obtainReplacements(Expr replace, boolean onlyBinary) throws CheatingIsBadMkay {
        List<Expr> replacements = new LinkedList<>();
        if (!onlyBinary) {
            Optional<List<Expr>> simpleReplacementsOp = getCompatibleVariablesFor(replace, strictTypeCheck());
            simpleReplacementsOp.ifPresent(replacements::addAll);
        }
        Optional<List<Expr>> allVariables = getAllVariablesFor(replace);
        //type check and filter simple replacements
        if (allVariables.isPresent() && !allVariables.get().isEmpty()) {
            for (Expr r : allVariables.get()) {
                r = cheatedClone(r);
                r.newID();
                for (Expr r2 : allVariables.get()) {
                    r2 = cheatedClone(r2);
                    r2.newID();
                    if (emptyOrNone(r.type().join(r2.type())))
                        continue;
                    ExprBinary joinReplacement = (ExprBinary) r.join(r2);
                    Type joinReplacementType = joinReplacement.type();
                    if (TypeChecking.canReplace(replace, joinReplacementType, strictTypeCheck())) {
                        replacements.add(joinReplacement);
                    }
                }
            }
        }
        if (!replacements.isEmpty()) {
            return Optional.of(replacements);
        }
        return Optional.empty();
    }

    @Override
    protected Ops whoiam() {
        return Ops.JEE;
    }


}

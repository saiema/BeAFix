package ar.edu.unrc.dc.mutation.op;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.Ops;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.ExprBinary;
import edu.mit.csail.sdg.ast.ExprCall;
import edu.mit.csail.sdg.ast.ExprUnary;
import edu.mit.csail.sdg.ast.ExprVar;
import edu.mit.csail.sdg.ast.Type;
import edu.mit.csail.sdg.parser.CompModule;

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
        if (unaryMutations.isPresent())
            mutations.addAll(unaryMutations.get());
        Optional<List<Mutation>> subMutations = x.accept(this);
        if (subMutations.isPresent())
            mutations.addAll(subMutations.get());
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return EMPTY;
    }

    @Override
    public Optional<List<Mutation>> visit(ExprVar x) throws Err {
        return generateMutants(x, x);
    }

    @Override
    public Optional<List<Mutation>> visit(ExprCall x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        Optional<List<Mutation>> callMutations = generateMutants(x, x);
        if (callMutations.isPresent())
            mutations.addAll(callMutations.get());
        for (Expr arg : x.args) {
            Optional<List<Mutation>> argMutations = x.accept(this);
            if (argMutations.isPresent())
                mutations.addAll(argMutations.get());
        }
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return EMPTY;
    }

    @Override
    protected boolean isMutable(Expr x) {
        return true;
    }

    @Override
    protected Optional<List<Mutation>> generateMutants(Expr from, Expr replace) {
        List<Mutation> mutations = new LinkedList<>();
        if (from instanceof ExprBinary) {
            //replace one subexpression of size 0 with one of size 1 (in terms of joins)
            ExprBinary original = (ExprBinary) from;
            if (!isMemberOfBinaryExpression(original, replace))
                throw new IllegalArgumentException("The expression to replace does not belongs to the from expression");
            Optional<List<Expr>> replacements = obtainReplacements(replace, false);
            if (replacements.isPresent()) {
                for (Expr r : replacements.get()) {
                    if (original.left.getID() == replace.getID()) {
                        if (emptyOrNone(getType(r).join(getType(original.right))))
                            continue;
                        mutations.add(new Mutation(Ops.JEE, original, original.mutateLeft(r)));
                    } else {
                        if (emptyOrNone(getType(original.left).join(getType(r))))
                            continue;
                        mutations.add(new Mutation(Ops.JEE, original, original.mutateRight(r)));
                    }
                }
            }
        } else if (from instanceof ExprUnary || from instanceof ExprVar || from instanceof ExprCall) {
            ExprUnary original = (ExprUnary) from;
            if (original.getID() != replace.getID())
                throw new IllegalArgumentException("The replace expression for Expr(Unary,HasName,Var,Call) must be the same as the from expression");
            Optional<List<Expr>> replacements = obtainReplacements(replace, true);
            if (replacements.isPresent()) {
                for (Expr r : replacements.get()) {
                    if (r instanceof ExprBinary) {
                        mutations.add(new Mutation(Ops.JEE, original, r));
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

    private Optional<List<Expr>> obtainReplacements(Expr replace, boolean onlyBinary) {
        List<Expr> replacements = new LinkedList<>();
        List<Expr> simpleReplacements = new LinkedList<>();
        if (onlyBinary)
            simpleReplacements.add(replace);
        else {
            Optional<List<Expr>> simpleReplacementsOp = getCompatibleVariablesFor(replace, strictTypeCheck());
            if (simpleReplacementsOp.isPresent())
                simpleReplacements.addAll(simpleReplacementsOp.get());
        }
        //type check and filter simple replacements
        if (!simpleReplacements.isEmpty()) {
            for (Expr r : simpleReplacements) {
                r = (Expr) r.clone();
                r.newID();
                if (typeCheck(replace, r, true)) {
                    replacements.add(r);
                }
                for (Expr r2 : simpleReplacements) {
                    r2 = (Expr) r2.clone();
                    r2.newID();
                    if (typeCheck(r, r2, false)) {
                        ExprBinary joinReplacement = (ExprBinary) r.join(r2);
                        if (typeCheck(replace, joinReplacement, true)) {
                            replacements.add(joinReplacement);
                        }
                    }
                }
            }
        }
        if (!replacements.isEmpty()) {
            return Optional.of(replacements);
        }
        return Optional.empty();
    }

    private boolean typeCheck(Expr exprA, Expr exprB, boolean replace) {
        Type replacementType = getType(exprB);
        if (replace) {
            return super.compatibleVariablesChecker(exprA, exprB, replacementType, strictTypeCheck());
        } else {
            Type joinedType = getType(exprA).join(replacementType);
            return !emptyOrNone(joinedType);
        }
    }

    @Override
    protected boolean compatibleVariablesChecker(Expr toReplace, Expr replacement, Type replacementType, boolean strictTypeChecking) {
        return true; //we will check types later
    }


}

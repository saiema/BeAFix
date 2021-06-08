package ar.edu.unrc.dc.mutation.mutantLab.testGeneration;

import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.*;

import java.util.Optional;

public class FactsFixer extends VisitReturn<Optional<Expr>> {

    private final ExprVar target;
    private final Sig replacement;
    private final boolean fixTrue;

    public FactsFixer(ExprVar expr, Sig replacement) {
        this.target = expr;
        this.replacement = replacement;
        this.fixTrue = false;
    }

    public FactsFixer() {
        this.target = null;
        this.replacement = null;
        this.fixTrue = true;
    }

    @Override
    public Optional<Expr> visitThis(Expr x) throws Err {
        return x.accept(this);
    }

    @Override
    public Optional<Expr> visit(ExprBinary x) throws Err {
        if (fixTrue && isTrueConstant(x.left)) {
            return x.left.accept(this);
        } else if (fixTrue && isTrueConstant(x.right)) {
            return x.right.accept(this);
        }
        Optional<Expr> leftReplacement = visitThis(x.left);
        Optional<Expr> rightReplacement = visitThis(x.right);
        boolean modified = false;
        if (leftReplacement.isPresent()) {
            x = (ExprBinary) x.mutateLeft(leftReplacement.get());
            modified = true;
        }
        if (rightReplacement.isPresent()) {
            x = (ExprBinary) x.mutateRight(rightReplacement.get());
            modified = true;
        }
        if (fixTrue && modified)
            return x.accept(this);
        return modified?Optional.of(x):Optional.empty();
    }

    @Override
    public Optional<Expr> visit(ExprList x) throws Err {
        Optional<Expr> argReplacement;
        boolean modified = false;
        for (Expr e : x.args) {
            if (fixTrue && isTrueConstant(e)) {
                x = x.removeArg(e);
                modified = true;
            } else {
                argReplacement = e.accept(this);
                if (argReplacement.isPresent()) {
                    if (fixTrue && isTrueConstant(argReplacement.get())) {
                        x = x.removeArg(argReplacement.get());
                    } else {
                        x = x.replaceArg(e, argReplacement.get());
                    }
                    modified = true;
                }
            }
        }
        if (fixTrue && x.args.isEmpty())
            return Optional.of(ExprConstant.Op.TRUE.make(null, 0));
        return modified?Optional.of(x):Optional.empty();
    }

    @Override
    public Optional<Expr> visit(ExprCall x) throws Err {
        Optional<Expr> argReplacement;
        boolean modified = false;
        for (Expr arg : x.args) {
            argReplacement = arg.accept(this);
            if (argReplacement.isPresent()) {
                x = x.mutateArgument(arg, argReplacement.get());
                modified = true;
            }
        }
        return modified?Optional.of(x):Optional.empty();
    }

    @Override
    public Optional<Expr> visit(ExprConstant x) throws Err {
        if (fixTrue && isTrueConstant(x))
            return Optional.of(x);
        return Optional.empty();
    }

    @Override
    public Optional<Expr> visit(ExprITE x) throws Err {
        Optional<Expr> conditionalReplacement = x.cond.accept(this);
        Optional<Expr> leftReplacement = x.left.accept(this);
        Optional<Expr> rightReplacement = x.right.accept(this);
        boolean modified = false;
        if (conditionalReplacement.isPresent()) {
            x = x.replaceCondition(conditionalReplacement.get());
            modified = true;
        }
        if (leftReplacement.isPresent()) {
            x = x.replaceThenClause(leftReplacement.get());
            modified = true;
        }
        if (rightReplacement.isPresent()) {
            x = x.replaceElseClause(rightReplacement.get());
            modified = true;
        }
        return modified?Optional.of(x):Optional.empty();
    }

    @Override
    public Optional<Expr> visit(ExprLet x) throws Err {
        Optional<Expr> exprReplacement = x.expr.accept(this);
        Optional<Expr> subReplacement = x.sub.accept(this);
        boolean modified = false;
        if (exprReplacement.isPresent()) {
            x = x.mutateBound(exprReplacement.get());
            modified = true;
        }
        if (subReplacement.isPresent()) {
            x = x.mutateBody(subReplacement.get());
            modified = true;
        }
        return modified?Optional.of(x):Optional.empty();
    }

    @Override
    public Optional<Expr> visit(ExprQt x) throws Err {
        boolean modified = false;
        for (Decl d : x.decls) {
            Optional<Expr> boundReplacement = d.expr.accept(this);
            if (boundReplacement.isPresent()) {
                x = x.replaceBoundForDecl(d, boundReplacement.get());
                modified = true;
            }
        }
        Optional<Expr> subReplacement = x.sub.accept(this);
        if (subReplacement.isPresent()) {
            x = x.replaceFormula(subReplacement.get());
            modified = true;
        }
        return modified?Optional.of(x):Optional.empty();
    }

    @Override
    public Optional<Expr> visit(ExprUnary x) throws Err {
        Optional<Expr> subReplacement = x.sub.accept(this);
        boolean modified = false;
        if (subReplacement.isPresent()) {
            x = x.mutateExpression(subReplacement.get());
            modified = true;
        }
        return modified?Optional.of(x):Optional.empty();
    }

    @Override
    public Optional<Expr> visit(ExprVar x) throws Err {
        if (fixTrue)
            return Optional.empty();
        if (target == null || replacement == null)
            throw new IllegalStateException("This fixer is not fixing true but does not have both target and replacement expressions set to a non null value");
        if (x.label.compareTo(target.label) == 0)
            return Optional.of(replacement);
        return Optional.empty();
    }

    @Override
    public Optional<Expr> visit(Sig x) throws Err {
        return Optional.empty();
    }

    @Override
    public Optional<Expr> visit(Sig.Field x) throws Err {
        return Optional.empty();
    }

    private boolean isTrueConstant(Expr x) {
        if (x instanceof ExprConstant) {
            return ((ExprConstant)x).op.equals(ExprConstant.Op.TRUE);
        }
        return false;
    }

}

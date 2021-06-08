package ar.edu.unrc.dc.mutation.mutantLab.testGeneration;

import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.*;

import java.util.Optional;

public class FactsFixer extends VisitReturn<Optional<Expr>> {

    private final ExprVar target;
    private final Sig replacement;

    public FactsFixer(ExprVar expr, Sig replacement) {
        this.target = expr;
        this.replacement = replacement;
    }

    @Override
    public Optional<Expr> visitThis(Expr x) throws Err {
        return x.accept(this);
    }

    @Override
    public Optional<Expr> visit(ExprBinary x) throws Err {
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
        return modified?Optional.of(x):Optional.empty();
    }

    @Override
    public Optional<Expr> visit(ExprList x) throws Err {
        Optional<Expr> argReplacement;
        boolean modified = false;
        for (Expr e : x.args) {
            argReplacement = e.accept(this);
            if (argReplacement.isPresent()) {
                x = x.replaceArg(e, argReplacement.get());
                modified = true;
            }
        }
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

}

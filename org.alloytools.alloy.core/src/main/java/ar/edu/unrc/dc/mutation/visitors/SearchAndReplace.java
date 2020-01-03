package edu.mit.csail.sdg.ast;

import java.util.Optional;

import edu.mit.csail.sdg.alloy4.Err;

public class VisitSearchAndReplace extends VisitReturn<Optional<Expr>> {

    /**
     * position of expression to search
     */
    private Expr target;
    private Expr replacement;


    public VisitSearchAndReplace(Expr expr, Expr replacement) {
        this.target = expr;
        this.replacement = replacement;
    }

    @Override
    public Optional<Expr> visit(ExprBinary x) throws Err {
        Optional<Expr> res = null;
        if (x.getID() == this.target.getID()) {
            return replace(x, replacement);
        } else {
            res = x.left.accept(this);
            if (res != null)
                return res;
            res = x.right.accept(this);
            if (res != null)
                return res;
        }
        return Optional.empty();
    }

    @Override
    public Optional<Expr> visit(ExprList x) throws Err {
        Optional<Expr> res = null;
        if (x.getID() == this.target.getID()) {
            return replace(x, replacement);
        } else {
            for (Expr e : x.args) {
                res = e.accept(this);
                if (res != null)
                    return res;
            }
        }

        return Optional.empty();
    }

    @Override
    public Optional<Expr> visit(ExprCall x) throws Err {
        if (x.getID() == this.target.getID()) {
            return replace(x, replacement);
        }
        return Optional.empty();
    }

    @Override
    public Optional<Expr> visit(ExprConstant x) throws Err {
        if (x.getID() == this.target.getID()) {
            return replace(x, replacement);
        }
        return Optional.empty();
    }

    @Override
    public Optional<Expr> visit(ExprITE x) throws Err {
        Optional<Expr> res = null;
        if (x.getID() == this.target.getID()) {
            return replace(x, replacement);
        } else {
            res = x.cond.accept(this);
            if (res != null)
                return res;
            res = x.left.accept(this);
            if (res != null)
                return res;
            res = x.right.accept(this);
            if (res != null)
                return res;
        }
        return Optional.empty();
    }

    @Override
    public Optional<Expr> visit(ExprLet x) throws Err {
        if (x.getID() == this.target.getID()) {
            return replace(x, replacement);
        } else
            return x.sub.accept(this);

    }

    @Override
    public Optional<Expr> visit(ExprQt x) throws Err {
        if (x.getID() == this.target.getID()) {
            return replace(x, replacement);
        } else
            return x.sub.accept(this);
    }

    @Override
    public Optional<Expr> visit(ExprUnary x) throws Err {
        if (x.getID() == this.target.getID()) {
            return replace(x, replacement);
        } else
            return x.sub.accept(this);
    }

    @Override
    public Optional<Expr> visit(ExprVar x) throws Err {
        if (x.getID() == this.target.getID()) {
            return replace(x, replacement);
        }
        return Optional.empty();
    }

    @Override
    public Optional<Expr> visit(Sig x) throws Err {
        Optional<Expr> res = null;
        if (x.getID() == this.target.getID()) {
            return replace(x, replacement);
        } else {
            for (Sig.Field f : x.getFields()) {
                res = f.accept(this);
                if (res != null)
                    return res;
            }
        }
        return Optional.empty();
    }

    @Override
    public Optional<Expr> visit(Sig.Field x) throws Err {
        if (x.getID() == this.target.getID()) {
            return replace(x, replacement);
        }
        return Optional.empty();
    }

    private Optional<Expr> replace(Expr original, Expr replacement) {
        Browsable originalParent = original.getBrowsableParent();
        if (originalParent != null) {
            if (originalParent instanceof ExprBinary) {
                //replacement should be either the left or the right side
                ExprBinary oParentAsBinary = (ExprBinary) originalParent;
                if (oParentAsBinary.left.getID() == original.getID()) {
                    return Optional.of(oParentAsBinary.mutateLeft(replacement));
                } else if (oParentAsBinary.right.getID() == original.getID()) {
                    return Optional.of(oParentAsBinary.mutateRight(replacement));
                } else {
                    return Optional.empty();
                }
            } else if (originalParent instanceof ExprUnary) {
                ExprUnary oParentAsUnary = (ExprUnary) originalParent;
                //replacement should be the subexpression
                if (oParentAsUnary.sub.getID() == original.getID()) {
                    return Optional.of(oParentAsUnary.mutateExpression(replacement));
                } else {
                    return Optional.empty();
                }
            } else {
                //for now we only have to deal with binary and unary expressions
                //TODO: update as this change
                Optional.empty();
            }
        }
        throw new IllegalStateException("If we are here then the original expression must have a parent");
    }
}

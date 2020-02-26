package ar.edu.unrc.dc.mutation.visitors;

import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.*;

import java.util.Optional;

public class SearchAndReplace extends VisitReturn<Optional<Expr>> {

    /**
     * position of expression to search
     */
    private Expr target;
    private Expr replacement;

    private Expr initialExpression;

    public SearchAndReplace(Expr expr, Expr replacement) {
        this.target = expr;
        this.replacement = replacement;
    }

    @Override
    public Optional<Expr> visitThis(Expr x) throws Err {
        initialExpression = x;
        return x.accept(this);
    }

    @Override
    public Optional<Expr> visit(ExprBinary x) throws Err {
        Optional<Expr> res;
        if (x.getID() == this.target.getID()) {
            return replace(x, replacement);
        } else {
            res = x.left.accept(this);
            if (res.isPresent())
                return res;
            res = x.right.accept(this);
            if (res.isPresent())
                return res;
        }
        return Optional.empty();
    }

    @Override
    public Optional<Expr> visit(ExprList x) throws Err {
        Optional<Expr> res;
        if (x.getID() == this.target.getID()) {
            return replace(x, replacement);
        } else {
            for (Expr e : x.args) {
                res = e.accept(this);
                if (res.isPresent())
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
        for (Expr arg : x.args) {
            Optional<Expr> res = arg.accept(this);
            if (res.isPresent())
                return res;
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
        Optional<Expr> res;
        if (x.getID() == this.target.getID()) {
            return replace(x, replacement);
        } else {
            res = x.cond.accept(this);
            if (res.isPresent())
                return res;
            res = x.left.accept(this);
            if (res.isPresent())
                return res;
            res = x.right.accept(this);
            if (res.isPresent())
                return res;
        }
        return Optional.empty();
    }

    @Override
    public Optional<Expr> visit(ExprLet x) throws Err {
        if (x.getID() == this.target.getID()) {
            return replace(x, replacement);
        }
        Optional<Expr> res = x.var.accept(this);
        if (res.isPresent())
            return res;
        res = x.expr.accept(this);
        if (res.isPresent())
            return res;
        res = x.sub.accept(this);
        return res;
    }

    @Override
    public Optional<Expr> visit(ExprQt x) throws Err {
        if (x.getID() == this.target.getID()) {
            return replace(x, replacement);
        }
        for (Decl d : x.decls) {
            for (Expr var : d.names) {
                Optional<Expr> res = var.accept(this);
                if (res.isPresent())
                    return res;
            }
            Optional<Expr> res = d.expr.accept(this);
            if (res.isPresent())
                return res;
        }
        return x.sub.accept(this);
    }

    @Override
    public Optional<Expr> visit(ExprUnary x) throws Err {
        if (x.getID() == this.target.getID()) {
            return replace(x, replacement);
        }
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
        Optional<Expr> res;
        if (x.getID() == this.target.getID()) {
            return replace(x, replacement);
        }
        for (Sig.Field f : x.getFields()) {
            res = f.accept(this);
            if (res.isPresent())
                return res;
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
        if (original.getID() == initialExpression.getID())
            return Optional.of(replacement);
        Browsable originalParent = original.getBrowsableParent();
        Expr modifiedParent = null;
        if (originalParent != null) {
            if (originalParent instanceof ExprBinary) {
                //replacement should be either the left or the right side
                ExprBinary oParentAsBinary = (ExprBinary) originalParent;
                if (oParentAsBinary.left.getID() == original.getID()) {
                    modifiedParent = oParentAsBinary.mutateLeft(replacement);
                } else if (oParentAsBinary.right.getID() == original.getID()) {
                    modifiedParent = oParentAsBinary.mutateRight(replacement);
                }
            } else if (originalParent instanceof ExprUnary) {
                ExprUnary oParentAsUnary = (ExprUnary) originalParent;
                //replacement should be the subexpression
                if (oParentAsUnary.sub.getID() == original.getID()) {
                    modifiedParent = oParentAsUnary.mutateExpression(replacement);
                }
            } else if (originalParent instanceof ExprQt) {
                ExprQt oParentAsQt = (ExprQt) originalParent;
                //check if replacement is in a bound expression
                for (Decl d : oParentAsQt.decls) {
                    if (d.expr.getID() == original.getID()) {
                        modifiedParent = oParentAsQt.replaceBoundForDecl(d, replacement);
                        break;
                    }
                }
                //check if replacement is in formula
                if (oParentAsQt.sub.getID() == original.getID())
                    modifiedParent = oParentAsQt.replaceFormula(replacement);
            } else if (originalParent instanceof  ExprList) {
                ExprList oParentAsList = (ExprList) originalParent;
                //replacement should be an arg, not the operator
                for (Expr arg : oParentAsList.args) {
                    if (arg.getID() == original.getID()) {
                        modifiedParent = oParentAsList.replaceArg(original, replacement);
                        break;
                    }
                }
            } else {
                //for now we only have to deal with binary and unary expressions
                //TODO: update as this change
                return Optional.empty();
            }
            if (modifiedParent != null) {
                target = (Expr) originalParent;
                this.replacement = modifiedParent;
                return visitThis(initialExpression);
            }
            return Optional.empty();
        }
        throw new IllegalStateException("If we are here then the original expression must have a parent");
    }
}

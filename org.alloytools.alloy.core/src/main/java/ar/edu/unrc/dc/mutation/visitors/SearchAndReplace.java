package ar.edu.unrc.dc.mutation.visitors;

import java.util.Optional;

import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.Browsable;
import edu.mit.csail.sdg.ast.Decl;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.ExprBinary;
import edu.mit.csail.sdg.ast.ExprCall;
import edu.mit.csail.sdg.ast.ExprConstant;
import edu.mit.csail.sdg.ast.ExprITE;
import edu.mit.csail.sdg.ast.ExprLet;
import edu.mit.csail.sdg.ast.ExprList;
import edu.mit.csail.sdg.ast.ExprQt;
import edu.mit.csail.sdg.ast.ExprUnary;
import edu.mit.csail.sdg.ast.ExprVar;
import edu.mit.csail.sdg.ast.Sig;
import edu.mit.csail.sdg.ast.VisitReturn;

public class SearchAndReplace extends VisitReturn<Optional<Expr>> {

    /**
     * position of expression to search
     */
    private Expr target;
    private Expr replacement;


    public SearchAndReplace(Expr expr, Expr replacement) {
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
        Optional<Expr> res = null;
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
        } else {
            Optional<Expr> res = x.var.accept(this);
            if (res.isPresent())
                return res;
        }
        return x.sub.accept(this);
    }

    @Override
    public Optional<Expr> visit(ExprQt x) throws Err {
        if (x.getID() == this.target.getID()) {
            return replace(x, replacement);
        } else {
            for (Decl d : x.decls) {
                for (Expr var : d.names) {
                    Optional<Expr> res = var.accept(this);
                    if (res.isPresent())
                        return res;
                }
            }
        }
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
                if (res.isPresent())
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

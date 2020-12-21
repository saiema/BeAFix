package ar.edu.unrc.dc.mutation.visitors;

import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.*;

public class SearchCall extends VisitReturn<Boolean> {

    /**
     * position of expression to search
     */
    private Func target;


    public SearchCall(Func expr) {
        this.target = expr;
    }

    public void setTarget(Func newTarget) {
        target = newTarget;
    }

    @Override
    public Boolean visit(ExprBinary x) throws Err {
        if (x.left.accept(this))
            return Boolean.TRUE;
        if (x.right.accept(this))
            return Boolean.TRUE;
        return Boolean.FALSE;
    }

    @Override
    public Boolean visit(ExprList x) throws Err {
        for (Expr e : x.args) {
            if (e.accept(this))
                return Boolean.TRUE;
        }
        return Boolean.FALSE;
    }

    @Override
    public Boolean visit(ExprCall x) throws Err {
        if (Browsable.equals(x.fun, target))
            return Boolean.TRUE;
        return Boolean.FALSE;
    }

    @Override
    public Boolean visit(ExprConstant x) throws Err {
        return Boolean.FALSE;
    }

    @Override
    public Boolean visit(ExprITE x) throws Err {
        if (x.cond.accept(this))
            return Boolean.TRUE;
        if (x.left.accept(this))
            return Boolean.TRUE;
        if (x.right.accept(this))
            return Boolean.TRUE;
        return Boolean.FALSE;
    }

    @Override
    public Boolean visit(ExprLet x) throws Err {
        if (x.var.accept(this))
            return Boolean.TRUE;
        if (x.expr.accept(this))
            return Boolean.TRUE;
        if (x.sub.accept(this))
            return Boolean.TRUE;
        return Boolean.FALSE;
    }

    @Override
    public Boolean visit(ExprQt x) throws Err {
        for (Decl d : x.decls) {
            if (d.expr.accept(this))
                return Boolean.TRUE;
        }
        return x.sub.accept(this);
    }

    @Override
    public Boolean visit(ExprUnary x) throws Err {
        return x.sub.accept(this);
    }

    @Override
    public Boolean visit(ExprVar x) throws Err {
        return Boolean.FALSE;
    }

    @Override
    public Boolean visit(Sig x) throws Err {
        return Boolean.FALSE;
    }

    @Override
    public Boolean visit(Sig.Field x) throws Err {
        return Boolean.FALSE;
    }
}

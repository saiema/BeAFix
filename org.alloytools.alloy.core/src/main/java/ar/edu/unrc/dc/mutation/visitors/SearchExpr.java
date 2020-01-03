package edu.mit.csail.sdg.ast;

import edu.mit.csail.sdg.alloy4.Err;

public class VisitSearchExpr extends VisitReturn<Boolean> {

    /**
     * position of expression to search
     */
    private Expr target;


    public VisitSearchExpr(Expr expr) {
        this.target = expr;
    }

    @Override
    public Boolean visit(ExprBinary x) throws Err {
        Boolean res = null;
        if (x.getID() == this.target.getID()) {
            return Boolean.TRUE;
        } else {
            res = x.left.accept(this);
            if (res != null)
                return res;
            res = x.right.accept(this);
            if (res != null)
                return res;
        }
        return Boolean.FALSE;
    }

    @Override
    public Boolean visit(ExprList x) throws Err {
        Boolean res = null;
        if (x.getID() == this.target.getID()) {
            return Boolean.TRUE;
        } else {
            for (Expr e : x.args) {
                res = e.accept(this);
                if (res != null)
                    return res;
            }
        }

        return Boolean.FALSE;
    }

    @Override
    public Boolean visit(ExprCall x) throws Err {
        if (x.getID() == this.target.getID()) {
            return Boolean.TRUE;
        }
        return Boolean.FALSE;
    }

    @Override
    public Boolean visit(ExprConstant x) throws Err {
        if (x.getID() == this.target.getID()) {
            return Boolean.TRUE;
        }
        return Boolean.FALSE;
    }

    @Override
    public Boolean visit(ExprITE x) throws Err {
        Boolean res = null;
        if (x.getID() == this.target.getID()) {
            return Boolean.TRUE;
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
        return Boolean.FALSE;
    }

    @Override
    public Boolean visit(ExprLet x) throws Err {
        if (x.getID() == this.target.getID()) {
            return Boolean.TRUE;
        } else
            return x.sub.accept(this);

    }

    @Override
    public Boolean visit(ExprQt x) throws Err {
        if (x.getID() == this.target.getID()) {
            return Boolean.TRUE;
        } else
            return x.sub.accept(this);
    }

    @Override
    public Boolean visit(ExprUnary x) throws Err {
        if (x.getID() == this.target.getID()) {
            return Boolean.TRUE;
        } else
            return x.sub.accept(this);
    }

    @Override
    public Boolean visit(ExprVar x) throws Err {
        if (x.getID() == this.target.getID()) {
            return Boolean.TRUE;
        }
        return Boolean.FALSE;
    }

    @Override
    public Boolean visit(Sig x) throws Err {
        Boolean res = null;
        if (x.getID() == this.target.getID()) {
            return Boolean.TRUE;
        } else {
            for (Sig.Field f : x.getFields()) {
                res = f.accept(this);
                if (res != null)
                    return res;
            }
        }
        return Boolean.FALSE;
    }

    @Override
    public Boolean visit(Sig.Field x) throws Err {
        if (x.getID() == this.target.getID()) {
            return Boolean.TRUE;
        }
        return Boolean.FALSE;
    }
}

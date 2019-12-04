package edu.mit.csail.sdg.ast;

import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.Pos;

public class VisitSearchExpByPos extends VisitReturn<Expr>{

    /**
     * position of expression to search
     */
    private Pos pos;


    public VisitSearchExpByPos(Pos pos) {
        this.pos = pos;
    }

    @Override
    public Expr visit(ExprBinary x) throws Err {
        Expr res = null;
        if (x.pos()==this.pos) {
            return x;
        }
        else {
            res = x.left.accept(this);
            if (res != null) return res;
            res = x.right.accept(this);
            if (res != null) return res;
        }
        return null;
    }

    @Override
    public Expr visit(ExprList x) throws Err {
        Expr res = null;
        if (x.pos()==this.pos) {
            return x;
        }
        else {
            for (Expr e: x.args) {
                res = e.accept(this);
                if (res != null) return res;
            }
        }
        
        return null;
    }

    @Override
    public Expr visit(ExprCall x) throws Err {
        if (x.pos()==this.pos) {
            return x;
        }
        return null;
    }

    @Override
    public Expr visit(ExprConstant x) throws Err {
        if (x.pos()==this.pos) {
            return x;
        }
        return null;
    }

    @Override
    public Expr visit(ExprITE x) throws Err {
        Expr res = null;
        if (x.pos()==this.pos) {
            return x;
        }
        else {
            res =x.cond.accept(this);
            if (res != null) return res;
            res = x.left.accept(this);
            if (res != null) return res;
            res = x.right.accept(this);
            if (res != null) return res;
        }
        return null;
    }

    @Override
    public Expr visit(ExprLet x) throws Err {
        if (x.pos()==this.pos) {
            return x;
        }
        else return x.sub.accept(this);

    }

    @Override
    public Expr visit(ExprQt x) throws Err {
        if (x.pos()==this.pos) {
            return x;
        }
        else return x.sub.accept(this);
    }

    @Override
    public Expr visit(ExprUnary x) throws Err {
        if (x.pos()==this.pos) {
            return x;
        }
        else return x.sub.accept(this);
    }

    @Override
    public Expr visit(ExprVar x) throws Err {
        if (x.pos()==this.pos) {
            return x;
        }
        return null;
    }

    @Override
    public Expr visit(Sig x) throws Err {
        Expr res = null;
        if (x.pos()==this.pos) {
            return x;
        }
        else {
            for ( Sig.Field f: x.getFields()) {
                res = f.accept(this);
                if (res != null) return res;
            }
        }
        return null;
    }

    @Override
    public Expr visit(Sig.Field x) throws Err {
        if (x.pos()==this.pos) {
            return x;
        }
        return null;
    }
}

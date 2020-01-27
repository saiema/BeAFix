package ar.edu.unrc.dc.mutation.visitors;

import edu.mit.csail.sdg.alloy4.Err;
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

public class SearchExpr extends VisitReturn<Boolean> {

    /**
     * position of expression to search
     */
    private Expr target;


    public SearchExpr(Expr expr) {
        this.target = expr;
    }

    @Override
    public Boolean visit(ExprBinary x) throws Err {
        if (match(x, this.target)) {
            return Boolean.TRUE;
        } else {
            if (x.left.accept(this))
                return Boolean.TRUE;
            if (x.right.accept(this))
                return Boolean.TRUE;
        }
        return Boolean.FALSE;
    }

    @Override
    public Boolean visit(ExprList x) throws Err {
        if (match(x, this.target)) {
            return Boolean.TRUE;
        } else {
            for (Expr e : x.args) {
                if (e.accept(this))
                    return Boolean.TRUE;
            }
        }
        return Boolean.FALSE;
    }

    @Override
    public Boolean visit(ExprCall x) throws Err {
        if (match(x, this.target)) {
            return Boolean.TRUE;
        }
        for (Expr arg : x.args) {
            if (arg.accept(this))
                return Boolean.TRUE;
        }
        return Boolean.FALSE;
    }

    @Override
    public Boolean visit(ExprConstant x) throws Err {
        if (match(x, this.target)) {
            return Boolean.TRUE;
        }
        return Boolean.FALSE;
    }

    @Override
    public Boolean visit(ExprITE x) throws Err {
        if (match(x, this.target)) {
            return Boolean.TRUE;
        } else {
            if (x.cond.accept(this))
                return Boolean.TRUE;
            if (x.left.accept(this))
                return Boolean.TRUE;
            if (x.right.accept(this))
                return Boolean.TRUE;
        }
        return Boolean.FALSE;
    }

    @Override
    public Boolean visit(ExprLet x) throws Err {
        if (match(x, this.target)) {
            return Boolean.TRUE;
        } else {
            if (x.var.accept(this))
                return Boolean.TRUE;
            if (x.expr.accept(this))
                return Boolean.TRUE;
            if (x.sub.accept(this))
                return Boolean.TRUE;
        }
        return Boolean.FALSE;
    }

    @Override
    public Boolean visit(ExprQt x) throws Err {
        if (match(x, this.target)) {
            return Boolean.TRUE;
        } else {
            for (Decl d : x.decls) {
                for (Expr var : d.names) {
                    if (var.accept(this)) {
                        return Boolean.TRUE;
                    }
                }
                if (d.expr.accept(this))
                    return Boolean.TRUE;
            }
        }
        return x.sub.accept(this);
    }

    @Override
    public Boolean visit(ExprUnary x) throws Err {
        if (match(x, this.target)) {
            return Boolean.TRUE;
        } else
            return x.sub.accept(this);
    }

    @Override
    public Boolean visit(ExprVar x) throws Err {
        if (match(x, this.target)) {
            return Boolean.TRUE;
        }
        return Boolean.FALSE;
    }

    @Override
    public Boolean visit(Sig x) throws Err {
        if (match(x, this.target)) {
            return Boolean.TRUE;
        } else {
            for (Sig.Field f : x.getFields()) {
                if (f.accept(this))
                    return Boolean.TRUE;
            }
        }
        return Boolean.FALSE;
    }

    @Override
    public Boolean visit(Sig.Field x) throws Err {
        if (match(x, this.target)) {
            return Boolean.TRUE;
        }
        return Boolean.FALSE;
    }

    protected boolean match(Expr a, Expr b) {
        return a.getID() == b.getID();
    }
}

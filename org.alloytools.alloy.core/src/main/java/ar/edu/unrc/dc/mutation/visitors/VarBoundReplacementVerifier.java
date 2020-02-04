package ar.edu.unrc.dc.mutation.visitors;

import ar.edu.unrc.dc.mutation.util.TypeChecking;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.*;

public class VarBoundReplacementVerifier extends VisitReturn<Boolean> {

    private Expr target;
    private Type replacementType;
    private boolean strictCheck;


    public VarBoundReplacementVerifier(Expr target, Type replacementType) {
        this(target, replacementType, true);
    }

    public VarBoundReplacementVerifier(Expr target, Type replacementType, boolean strictCheck) {
        this.target = target;
        this.replacementType = replacementType;
        this.strictCheck = strictCheck;
    }

    @Override
    public Boolean visit(ExprBinary x) throws Err {
        Boolean res = null;
        if (match(x.left, this.target)) {
            return TypeChecking.checkBinary(x.right, replacementType, x.op, true, strictCheck);
        } else if (match(x.right, this.target)) {
            return TypeChecking.checkBinary(x.left, replacementType, x.op, false, strictCheck);
        }
        if (x.left.accept(this) && x.right.accept(this))
            return Boolean.TRUE;
        return Boolean.FALSE;
    }

    @Override
    public Boolean visit(ExprList x) throws Err {
        for (Expr e : x.args) {
            if (match(e, this.target)) {
                if (!TypeChecking.checkExprList(e, x, replacementType))
                    return Boolean.FALSE;
            } else {
                if (!e.accept(this))
                    return Boolean.FALSE;
            }
        }
        return Boolean.TRUE;
    }

    @Override
    public Boolean visit(ExprCall x) throws Err {
        for (Expr e : x.args) {
            if (match(e, this.target)) {
                if (!TypeChecking.checkExprCallArgument(e, x, replacementType, strictCheck));
                    return Boolean.FALSE;
            } else {
                if (!e.accept(this))
                    return Boolean.FALSE;
            }
        }
        return Boolean.TRUE;
    }

    @Override
    public Boolean visit(ExprConstant x) throws Err {
        return Boolean.TRUE;
    }

    @Override
    public Boolean visit(ExprITE x) throws Err {
        Boolean res = null;
        if (match(x.cond, this.target)) {
            return this.replacementType.is_bool;
        }
        if (!x.cond.accept(this))
            return Boolean.FALSE;
        if (!x.left.accept(this))
            return Boolean.FALSE;
        if (!x.right.accept(this))
            return Boolean.FALSE;
        return Boolean.TRUE;
    }

    @Override
    public Boolean visit(ExprLet x) throws Err {
        if (match(x.var, this.target)) //The variable we are checking has been masked
            return Boolean.TRUE;
        SearchExpr searcher = new SearchExpr(this.target) {

            @Override
            protected boolean match(Expr a, Expr b) {
                return a.toString().compareTo(b.toString()) == 0;
            }

        };
        if (searcher.visitThis(x.expr)) {
            //We altered the let variable type, we must continue our check with this new variable
            VarBoundReplacementVerifier verifier = new VarBoundReplacementVerifier(x.var, replacementType, strictCheck);
            return verifier.visitThis(x.sub);
        }
        return x.sub.accept(this);
    }

    @Override
    public Boolean visit(ExprQt x) throws Err {
        SearchExpr searcher = new SearchExpr(this.target) {

            @Override
            protected boolean match(Expr a, Expr b) {
                return a.toString().compareTo(b.toString()) == 0;
            }

        };
        for (Decl d : x.decls) {
            for (Expr var : d.names) {
                if (match(var, this.target))
                    return Boolean.TRUE; //The variable we are checking has been masked
            }
            if (searcher.visitThis(d.expr)) {
                //We altered the let variable type, we must continue our check with all variables in this Decl
                for (Expr var : d.names) {
                    VarBoundReplacementVerifier verifier = new VarBoundReplacementVerifier(var, replacementType, strictCheck);
                    if (!verifier.visitThis(x.sub))
                        return Boolean.FALSE;
                }
                return Boolean.TRUE;
            } else {
                if (!d.expr.accept(this))
                    return Boolean.FALSE;
            }
        }
        return x.sub.accept(this);
    }

    @Override
    public Boolean visit(ExprUnary x) throws Err {
        if (match(x.sub, this.target)) {
            return TypeChecking.checkUnary(x.sub, replacementType, x.op, strictCheck);
        } else
            return x.sub.accept(this);
    }

    @Override
    public Boolean visit(ExprVar x) throws Err {
        return Boolean.TRUE;
    }

    @Override
    public Boolean visit(Sig x) throws Err {
        return Boolean.TRUE;
    }

    @Override
    public Boolean visit(Sig.Field x) throws Err {
        return Boolean.TRUE;
    }

    private boolean match(Expr a, Expr b) {
        return a.toString().compareTo(b.toString()) == 0;
    }

}

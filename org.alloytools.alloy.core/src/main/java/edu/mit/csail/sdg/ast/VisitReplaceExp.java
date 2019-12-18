package edu.mit.csail.sdg.ast;

import edu.mit.csail.sdg.alloy4.Err;

import java.util.ArrayList;
import java.util.List;

public class VisitReplaceExp extends VisitReturn<Expr>{

    /**
     *  expression to replace
     */
    private Expr originExpr;

    private Expr newExpr;


    public VisitReplaceExp(Expr originExpr, Expr newExpr) {
        this.originExpr = originExpr; this.newExpr=newExpr;
    }

    @Override
    public Expr visit(ExprBinary x) throws Err {
        if (x==originExpr)
            return newExpr;
        else {
            Expr left = x.left.accept(this);
            Expr right = x.right.accept(this);
            return x.op.make(x.pos, x.closingBracket, left, right);
        }
    }

    @Override
    public Expr visit(ExprList x) throws Err {
        if (x==originExpr) return newExpr;
        List<Expr> newsArgs = new ArrayList<Expr>();
        for (Expr e : x.args) {
            newsArgs.add(e.accept(this));
        }
        return ExprList.make(x.pos, x.closingBracket, x.op, newsArgs);
    }

    @Override
    public Expr visit(ExprCall x) throws Err {
        if (x==originExpr)
            return newExpr;
        else
            return x;

    }

    @Override
    public Expr visit(ExprConstant x) throws Err {
        if (x==originExpr)
            return newExpr;
        else
            return x;
    }

    @Override
    public Expr visit(ExprITE x) throws Err {
        if (x==originExpr)
            return newExpr;
        else {
            Expr newCond = x.cond.accept(this);
            Expr newLeft = x.left.accept(this);
            Expr newRight = x.right.accept(this);
            return  ExprITE.make(x.pos, newCond, newLeft, newRight);
        }
    }

    @Override
    public Expr visit(ExprLet x) throws Err {
        if (x==originExpr)
            return newExpr;
        else {
            //Expr newLetBody = x.sub.accept(this);
            return x;

        }
    }

    @Override
    public Expr visit(ExprQt x) throws Err {
        if (x==originExpr)
            return newExpr;
        else {
            return x.op.make(x.pos, x.closingBracket, x.decls, x.sub.accept(this));
        }
    }

    @Override
    public Expr visit(ExprUnary x) throws Err {
        if (x==originExpr)
            return newExpr;
        else {
            return x.op.make(x.pos(),x.sub.accept(this));
        }
    }

    @Override
    public Expr visit(ExprVar x) throws Err {
        if (x==originExpr)
            return newExpr;
        else
            return x;
    }

    @Override
    public Expr visit(Sig x) throws Err {
        if (x==originExpr)
            return newExpr;
        else
            return x;
    }

    @Override
    public Expr visit(Sig.Field x) throws Err {
        return x;
    }
}

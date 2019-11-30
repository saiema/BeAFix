package ar.edu.unrc.dc.mutation;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.ExprBinary;
import edu.mit.csail.sdg.ast.ExprBinary.Op;
import edu.mit.csail.sdg.ast.ExprCall;
import edu.mit.csail.sdg.ast.ExprConstant;
import edu.mit.csail.sdg.ast.ExprITE;
import edu.mit.csail.sdg.ast.ExprLet;
import edu.mit.csail.sdg.ast.ExprList;
import edu.mit.csail.sdg.ast.ExprQt;
import edu.mit.csail.sdg.ast.ExprUnary;
import edu.mit.csail.sdg.ast.ExprVar;
import edu.mit.csail.sdg.ast.Sig;
import edu.mit.csail.sdg.ast.Sig.Field;
import edu.mit.csail.sdg.ast.VisitReturn;


public abstract class Mutator extends VisitReturn<List<Expr>> {

    protected static final List<Expr> EMPTY          = new LinkedList<>();
    protected static final List<Op>   RELATIONAL_OPS = Arrays.asList(Op.EQUALS, Op.GT, Op.GTE, Op.LT, Op.LTE, Op.NOT_EQUALS, Op.NOT_GT, Op.NOT_GTE, Op.NOT_LT, Op.NOT_LTE);


    public List<Expr> mutate(Expr e) {
        return this.visitThis(e);
    }

    @Override
    public List<Expr> visit(Sig x) throws Err {
        return EMPTY;
    }

    @Override
    public List<Expr> visit(Field x) throws Err {
        return EMPTY;
    }

    @Override
    public List<Expr> visit(ExprBinary x) throws Err {
        return EMPTY;
    }

    @Override
    public List<Expr> visit(ExprList x) throws Err {
        return EMPTY;
    }

    @Override
    public List<Expr> visit(ExprCall x) throws Err {
        return EMPTY;
    }

    @Override
    public List<Expr> visit(ExprConstant x) throws Err {
        return EMPTY;
    }

    @Override
    public List<Expr> visit(ExprITE x) throws Err {
        return EMPTY;
    }

    @Override
    public List<Expr> visit(ExprLet x) throws Err {
        return EMPTY;
    }

    @Override
    public List<Expr> visit(ExprQt x) throws Err {
        return EMPTY;
    }

    @Override
    public List<Expr> visit(ExprUnary x) throws Err {
        return EMPTY;
    }

    @Override
    public List<Expr> visit(ExprVar x) throws Err {
        return EMPTY;
    }

}

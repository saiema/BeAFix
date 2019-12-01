package ar.edu.unrc.dc.mutation;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

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


public abstract class Mutator extends VisitReturn<Optional<List<Mutation>>> {

    protected static final Optional<List<Mutation>> EMPTY                 = Optional.empty();
    protected static final List<Op>                 RELATIONAL_OPS        = Arrays.asList(Op.EQUALS, Op.GT, Op.GTE, Op.LT, Op.LTE, Op.NOT_EQUALS, Op.NOT_GT, Op.NOT_GTE, Op.NOT_LT, Op.NOT_LTE);
    protected static final List<Op>                 CONDITIONAL_OPS       = Arrays.asList(Op.AND, Op.OR, Op.IMPLIES, Op.IFF);
    protected static final List<Op>                 ARITHMETIC_BINARY_OPS = Arrays.asList(Op.DIV, Op.MUL, Op.REM, Op.IPLUS, Op.IMINUS);
    protected static final List<ExprUnary.Op>       RELATIONAL_UNARY_OPS  = Arrays.asList(ExprUnary.Op.CLOSURE, ExprUnary.Op.RCLOSURE, ExprUnary.Op.TRANSPOSE);

    public Optional<List<Mutation>> mutate(Expr e) {
        return this.visitThis(e);
    }

    //UTILITIES

    protected boolean isRelationalExpression(Expr e) {
        if (!(e instanceof ExprBinary))
            return false;
        return RELATIONAL_OPS.contains(((ExprBinary) e).op);
    }

    protected boolean isConditionalExpression(Expr e) {
        if (!(e instanceof ExprBinary))
            return false;
        return CONDITIONAL_OPS.contains(((ExprBinary) e).op);
    }

    //for the moment only binary expressions are considered
    protected boolean isArithmeticExpression(Expr e) {
        if (!(e instanceof ExprBinary))
            return false;
        return ARITHMETIC_BINARY_OPS.contains(((ExprBinary) e).op);
    }

    protected boolean isUnaryRelationalExpression(Expr e) {
        if (!(e instanceof ExprUnary))
            return false;
        return RELATIONAL_UNARY_OPS.contains(((ExprUnary) e).op);
    }

    //DEFAULT VISIT IMPLEMENTATION

    @Override
    public Optional<List<Mutation>> visit(Sig x) throws Err {
        return EMPTY;
    }

    @Override
    public Optional<List<Mutation>> visit(Field x) throws Err {
        return EMPTY;
    }

    @Override
    public Optional<List<Mutation>> visit(ExprBinary x) throws Err {
        return EMPTY;
    }

    @Override
    public Optional<List<Mutation>> visit(ExprList x) throws Err {
        return EMPTY;
    }

    @Override
    public Optional<List<Mutation>> visit(ExprCall x) throws Err {
        return EMPTY;
    }

    @Override
    public Optional<List<Mutation>> visit(ExprConstant x) throws Err {
        return EMPTY;
    }

    @Override
    public Optional<List<Mutation>> visit(ExprITE x) throws Err {
        return EMPTY;
    }

    @Override
    public Optional<List<Mutation>> visit(ExprLet x) throws Err {
        return EMPTY;
    }

    @Override
    public Optional<List<Mutation>> visit(ExprQt x) throws Err {
        return EMPTY;
    }

    @Override
    public Optional<List<Mutation>> visit(ExprUnary x) throws Err {
        return EMPTY;
    }

    @Override
    public Optional<List<Mutation>> visit(ExprVar x) throws Err {
        return EMPTY;
    }

}

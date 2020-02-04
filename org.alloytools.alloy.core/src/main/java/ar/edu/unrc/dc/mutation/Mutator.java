package ar.edu.unrc.dc.mutation;

import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.*;
import edu.mit.csail.sdg.ast.ExprBinary.Op;
import edu.mit.csail.sdg.ast.Sig.Field;
import edu.mit.csail.sdg.parser.CompModule;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;


public abstract class Mutator extends VisitReturn<Optional<List<Mutation>>> {

    protected static final Optional<List<Mutation>> EMPTY                 = Optional.empty();
    protected static final List<Op>                 RELATIONAL_OPS        = Arrays.asList(Op.EQUALS, Op.GT, Op.GTE, Op.LT, Op.LTE, Op.NOT_EQUALS, Op.NOT_GT, Op.NOT_GTE, Op.NOT_LT, Op.NOT_LTE);
    protected static final List<Op>                 CONDITIONAL_OPS       = Arrays.asList(Op.AND, Op.OR, Op.IMPLIES, Op.IFF);
    protected static final List<Op>                 ARITHMETIC_BINARY_OPS = Arrays.asList(Op.DIV, Op.MUL, Op.REM, Op.IPLUS, Op.IMINUS);
    protected static final List<ExprUnary.Op>       RELATIONAL_UNARY_OPS  = Arrays.asList(ExprUnary.Op.CLOSURE, ExprUnary.Op.RCLOSURE, ExprUnary.Op.TRANSPOSE);
    protected static final List<Op>                 SET_OPERATORS         = Arrays.asList(Op.JOIN, Op.PLUS, Op.MINUS, Op.INTERSECT, Op.IN, Op.PLUSPLUS);
    protected static final List<ExprUnary.Op>       MULTIPLICITY_OPERATORS = Arrays.asList(ExprUnary.Op.NO, ExprUnary.Op.SOME, ExprUnary.Op.LONE, ExprUnary.Op.ONE);
    //Comprehension is not yet considered
    protected static final List<ExprQt.Op>          QUANTIFIER_OPERATORS  = Arrays.asList(ExprQt.Op.ALL, ExprQt.Op.LONE, ExprQt.Op.NO, ExprQt.Op.ONE, ExprQt.Op.SOME);

    protected CompModule                            context;

    protected Mutator(CompModule context) {
        this.context = context;
    }

    public Optional<List<Mutation>> getMutations(Expr e) {
        return this.visitThis(e);
    }

    protected abstract Ops whoiam();

    //UTILITIES

    protected final boolean isRelationalExpression(Expr e) {
        if (!(e instanceof ExprBinary))
            return false;
        return RELATIONAL_OPS.contains(((ExprBinary) e).op);
    }

    protected final boolean isConditionalExpression(Expr e) {
        if (!(e instanceof ExprBinary))
            return false;
        return CONDITIONAL_OPS.contains(((ExprBinary) e).op);
    }

    //for the moment only binary expressions are considered
    protected final boolean isArithmeticBinaryExpression(Expr e) {
        if (!(e instanceof ExprBinary))
            return false;
        return ARITHMETIC_BINARY_OPS.contains(((ExprBinary) e).op);
    }

    protected final boolean isUnaryRelationalExpression(Expr e) {
        if (!(e instanceof ExprUnary))
            return false;
        return RELATIONAL_UNARY_OPS.contains(((ExprUnary) e).op);
    }

    protected final boolean isSetBinaryExpression(Expr e) {
        if (!(e instanceof ExprBinary))
            return false;
        return SET_OPERATORS.contains(((ExprBinary) e).op);
    }

    protected final boolean isMultiplicityExpression(Expr e) {
        if (!(e instanceof ExprUnary))
            return false;
        return MULTIPLICITY_OPERATORS.contains(((ExprUnary) e).op);
    }

    protected boolean isMemberOfBinaryExpression(ExprBinary binary, Expr expr) {
        return (binary.left.getID() == expr.getID()) || (binary.right.getID() == expr.getID());
    }

    //DEFAULT VISIT IMPLEMENTATION

    @Override
    public Optional<List<Mutation>> visit(Sig x) throws Err {
        //TODO: need help with this
        return EMPTY;
    }

    @Override
    public Optional<List<Mutation>> visit(Field x) throws Err {
        //TODO: should visit the decl
        return EMPTY;
    }

    @Override
    public Optional<List<Mutation>> visit(ExprBinary x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        Optional<List<Mutation>> leftMutations = x.left.accept(this);
        leftMutations.ifPresent(mutations::addAll);
        Optional<List<Mutation>> rightMutations = x.right.accept(this);
        rightMutations.ifPresent(mutations::addAll);
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return EMPTY;
    }

    @Override
    public Optional<List<Mutation>> visit(ExprList x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        if (x.args != null) {
            for (Expr e : x.args) {
                Optional<List<Mutation>> argMutations = e.accept(this);
                argMutations.ifPresent(mutations::addAll);
            }
        }
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return EMPTY;
    }

    @Override
    public Optional<List<Mutation>> visit(ExprCall x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        if (x.args != null) {
            for (Expr e : x.args) {
                Optional<List<Mutation>> argMutations = e.accept(this);
                argMutations.ifPresent(mutations::addAll);
            }
        }
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return EMPTY;
    }

    @Override
    public Optional<List<Mutation>> visit(ExprConstant x) throws Err {
        //this expressions are TRUE, FALSE, and numbers
        return EMPTY;
    }

    @Override
    public Optional<List<Mutation>> visit(ExprITE x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        if (x.cond != null) {
            Optional<List<Mutation>> condMutations = x.cond.accept(this);
            condMutations.ifPresent(mutations::addAll);
        }
        if (x.left != null) {
            Optional<List<Mutation>> thenMutations = x.left.accept(this);
            thenMutations.ifPresent(mutations::addAll);
        }
        if (x.right != null) {
            Optional<List<Mutation>> elseMutations = x.right.accept(this);
            elseMutations.ifPresent(mutations::addAll);
        }
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return EMPTY;
    }

    @Override
    public Optional<List<Mutation>> visit(ExprLet x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        if (x.var != null) {
            Optional<List<Mutation>> letVarMutations = x.var.accept(this);
            letVarMutations.ifPresent(mutations::addAll);
        }
        if (x.expr != null) {
            Optional<List<Mutation>> letVarBoundedExprMutations = x.expr.accept(this);
            letVarBoundedExprMutations.ifPresent(mutations::addAll);
        }
        if (x.sub != null) {
            Optional<List<Mutation>> exprMutations = x.sub.accept(this);
            exprMutations.ifPresent(mutations::addAll);
        }
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return EMPTY;
    }

    @Override
    public Optional<List<Mutation>> visit(ExprQt x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        Optional<List<Mutation>> formulaMutations = x.sub.accept(this);
        formulaMutations.ifPresent(mutations::addAll);
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return EMPTY;
    }

    @Override
    public Optional<List<Mutation>> visit(ExprUnary x) throws Err {
        Expr sub = x.sub;
        if (sub != null)
            return sub.accept(this);
        return EMPTY;
    }

    @Override
    public Optional<List<Mutation>> visit(ExprVar x) throws Err {
        return EMPTY;
    }

}

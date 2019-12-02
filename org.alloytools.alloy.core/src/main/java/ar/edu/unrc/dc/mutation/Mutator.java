package ar.edu.unrc.dc.mutation;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.Decl;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.ExprBinary;
import edu.mit.csail.sdg.ast.ExprBinary.Op;
import edu.mit.csail.sdg.ast.ExprCall;
import edu.mit.csail.sdg.ast.ExprConstant;
import edu.mit.csail.sdg.ast.ExprHasName;
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

    public Optional<List<Mutation>> getMutations(Expr e) {
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
        if (x.left != null) {
            Optional<List<Mutation>> leftMutations = x.left.accept(this);
            if (leftMutations.isPresent())
                mutations.addAll(leftMutations.get());
        }
        if (x.right != null) {
            Optional<List<Mutation>> rightMutations = x.right.accept(this);
            if (rightMutations.isPresent())
                mutations.addAll(rightMutations.get());
        }
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
                if (argMutations.isPresent())
                    mutations.addAll(argMutations.get());
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
                if (argMutations.isPresent())
                    mutations.addAll(argMutations.get());
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
            if (condMutations.isPresent())
                mutations.addAll(condMutations.get());
        }
        if (x.left != null) {
            Optional<List<Mutation>> thenMutations = x.left.accept(this);
            if (thenMutations.isPresent())
                mutations.addAll(thenMutations.get());
        }
        if (x.right != null) {
            Optional<List<Mutation>> elseMutations = x.right.accept(this);
            if (elseMutations.isPresent())
                mutations.addAll(elseMutations.get());
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
            if (letVarMutations.isPresent())
                mutations.addAll(letVarMutations.get());
        }
        if (x.expr != null) {
            Optional<List<Mutation>> letVarBoundedExprMutations = x.expr.accept(this);
            if (letVarBoundedExprMutations.isPresent())
                mutations.addAll(letVarBoundedExprMutations.get());
        }
        if (x.sub != null) {
            Optional<List<Mutation>> exprMutations = x.sub.accept(this);
            if (exprMutations.isPresent())
                mutations.addAll(exprMutations.get());
        }
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return EMPTY;
    }

    @Override
    public Optional<List<Mutation>> visit(ExprQt x) throws Err {
        List<Decl> vars = x.decls;
        List<Mutation> declMutations = new LinkedList<>();
        if (vars != null) {
            for (Decl d : vars) {
                //only mutate names for now
                //TODO: maybe add bounds later
                for (ExprHasName n : d.names) {
                    Optional<List<Mutation>> nMutations = n.accept(this);
                    if (nMutations.isPresent())
                        declMutations.addAll(nMutations.get());
                }
            }
        }

        Expr formula = x.sub;
        if (formula != null)
            if (declMutations.isEmpty())
                return formula.accept(this);
            else {
                Optional<List<Mutation>> formulaMutations = formula.accept(this);
                if (formulaMutations.isPresent()) {
                    declMutations.addAll(formulaMutations.get());
                }
                return Optional.of(declMutations);
            }
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

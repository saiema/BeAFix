package ar.edu.unrc.dc.mutation;

import ar.edu.unrc.dc.mutation.MutationConfiguration.ConfigKey;
import ar.edu.unrc.dc.mutation.mutantLab.Candidate;
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

    protected static final List<Op>                 RELATIONAL_OPS        = Arrays.asList(Op.EQUALS, Op.GT, Op.GTE, Op.LT, Op.LTE, Op.NOT_EQUALS, Op.NOT_GT, Op.NOT_GTE, Op.NOT_LT, Op.NOT_LTE);
    protected static final List<Op>                 CONDITIONAL_OPS       = Arrays.asList(Op.AND, Op.OR, Op.IMPLIES, Op.IFF);
    protected static final List<Op>                 ARITHMETIC_BINARY_OPS = Arrays.asList(Op.DIV, Op.MUL, Op.REM, Op.IPLUS, Op.IMINUS);
    protected static final List<ExprUnary.Op>       RELATIONAL_UNARY_OPS  = Arrays.asList(ExprUnary.Op.CLOSURE, ExprUnary.Op.RCLOSURE, ExprUnary.Op.TRANSPOSE);
    protected static final List<Op>                 SET_OPERATORS         = Arrays.asList(Op.JOIN, Op.PLUS, Op.MINUS, Op.INTERSECT, Op.IN, Op.PLUSPLUS);
    protected static final List<ExprUnary.Op>       MULTIPLICITY_OPERATORS = Arrays.asList(ExprUnary.Op.NO, ExprUnary.Op.SOME, ExprUnary.Op.LONE, ExprUnary.Op.ONE);
    //Comprehension is not yet considered
    protected static final List<ExprQt.Op>          QUANTIFIER_OPERATORS  = Arrays.asList(ExprQt.Op.ALL, ExprQt.Op.LONE, ExprQt.Op.NO, ExprQt.Op.ONE, ExprQt.Op.SOME);

    protected CompModule                            context;
    private Candidate                               candidate;
    private boolean                                 useMutGenLimit;

    protected Mutator(CompModule context) {
        this.context = context;
        useMutGenLimit = false;
    }

    public Optional<List<Mutation>> getMutationsFrom(Expr e, Candidate candidate) {
        if (candidate == null)
            throw new IllegalArgumentException("candidate can't be null");
        this.candidate = candidate;
        Optional<List<Mutation>> mutations = visitThis(e);
        candidate.clearMutatedStatus();
        return mutations;
    }
    public Optional<List<Mutation>> getMutations(Expr e) {
        return visitThis(e);
    }

    public Optional<List<Mutation>> getMutations() {
        useMutGenLimit = true;
        List<Mutation> mutations = new LinkedList<>();
        context.getAllFunc().forEach(f -> {
            visitThis(f.getBody()).ifPresent(mutations::addAll);
        });
        context.getAllAssertions().forEach(namedAssertion -> {
            visitThis(namedAssertion.b).ifPresent(mutations::addAll);
        });
        context.getAllFacts().forEach(namedFact -> {
            visitThis(namedFact.b).ifPresent(mutations::addAll);
        });
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return Optional.empty();
    }

    @Override
    public Optional<List<Mutation>> visitThis(Expr x) throws Err {
        if (candidate != null) {
            Optional<Expr> mutant = candidate.getMutatedExpr(x);
            mutant.ifPresent(expr -> {
                candidate.markAsAlreadyMutated(x);
                visitThis(mutant.get());
            });
        }
        return x.accept(this);
    }

    protected abstract Ops whoiam();

    protected boolean mutGenLimitCheck(Expr x) {
        return !useMutGenLimit || x.canBeMutated();
    }

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

    protected final boolean isArithmeticUnaryExpression(Expr x) {
        if (!(x instanceof ExprUnary))
            return false;
        ExprUnary xAsUnary = (ExprUnary) x;
        switch (xAsUnary.op) {
            case CARDINALITY :
            case CAST2INT :
            case CAST2SIGINT :
                return true;
            default :
                return false;
        }
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
        return Optional.empty();
    }

    @Override
    public Optional<List<Mutation>> visit(Field x) throws Err {
        //TODO: should visit the decl
        return Optional.empty();
    }

    @Override
    public Optional<List<Mutation>> visit(ExprBinary x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        Optional<List<Mutation>> leftMutations = visitThis(x.left);
        leftMutations.ifPresent(mutations::addAll);
        Optional<List<Mutation>> rightMutations = visitThis(x.right);
        rightMutations.ifPresent(mutations::addAll);
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return Optional.empty();
    }

    @Override
    public Optional<List<Mutation>> visit(ExprList x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        if (x.args != null) {
            for (Expr e : x.args) {
                Optional<List<Mutation>> argMutations = visitThis(e);
                argMutations.ifPresent(mutations::addAll);
            }
        }
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return Optional.empty();
    }

    @Override
    public Optional<List<Mutation>> visit(ExprCall x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        if (x.args != null) {
            for (Expr e : x.args) {
                Optional<List<Mutation>> argMutations = visitThis(e);
                argMutations.ifPresent(mutations::addAll);
            }
        }
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return Optional.empty();
    }

    @Override
    public Optional<List<Mutation>> visit(ExprConstant x) throws Err {
        //this expressions are TRUE, FALSE, and numbers
        return Optional.empty();
    }

    @Override
    public Optional<List<Mutation>> visit(ExprITE x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        if (x.cond != null) {
            Optional<List<Mutation>> condMutations = visitThis(x.cond);
            condMutations.ifPresent(mutations::addAll);
        }
        if (x.left != null) {
            Optional<List<Mutation>> thenMutations = visitThis(x.left);
            thenMutations.ifPresent(mutations::addAll);
        }
        if (x.right != null) {
            Optional<List<Mutation>> elseMutations = visitThis(x.right);
            elseMutations.ifPresent(mutations::addAll);
        }
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return Optional.empty();
    }

    @Override
    public Optional<List<Mutation>> visit(ExprLet x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
//        Optional<List<Mutation>> letVarMutations = x.var.accept(this);
//        letVarMutations.ifPresent(mutations::addAll);
        if (allowBoundMutationByAnyOperator()) {
            Optional<List<Mutation>> letVarBoundedExprMutations = visitThis(x.expr);
            letVarBoundedExprMutations.ifPresent(mutations::addAll);
        }
        Optional<List<Mutation>> exprMutations = visitThis(x.sub);
        exprMutations.ifPresent(mutations::addAll);
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return Optional.empty();
    }

    @Override
    public Optional<List<Mutation>> visit(ExprQt x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        if (allowBoundMutationByAnyOperator()) {
            for (Decl d : x.decls) {
                Optional<List<Mutation>> boundMutations = visitThis(d.expr);
                boundMutations.ifPresent(mutations::addAll);
            }
        }
        Optional<List<Mutation>> formulaMutations = visitThis(x.sub);
        formulaMutations.ifPresent(mutations::addAll);
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return Optional.empty();
    }

    @Override
    public Optional<List<Mutation>> visit(ExprUnary x) throws Err {
        Expr sub = x.sub;
        if (sub != null)
            return visitThis(sub);
        return Optional.empty();
    }

    @Override
    public Optional<List<Mutation>> visit(ExprVar x) throws Err {
        return Optional.empty();
    }

    //CONFIGURATION METHODS

    private boolean allowBoundMutationByAnyOperator() {
        Optional<Object> configValue = MutationConfiguration.getInstance().getConfigValue(ConfigKey.MUTATION_BOUND_MUTATION_BY_ANY_OPERATOR);
        return configValue.map(o -> (Boolean) o).orElse((Boolean) ConfigKey.MUTATION_BOUND_MUTATION_BY_ANY_OPERATOR.defaultValue());
    }

}

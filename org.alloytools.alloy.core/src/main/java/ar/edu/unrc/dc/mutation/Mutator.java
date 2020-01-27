package ar.edu.unrc.dc.mutation;

import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.SafeList;
import edu.mit.csail.sdg.ast.*;
import edu.mit.csail.sdg.ast.ExprBinary.Op;
import edu.mit.csail.sdg.ast.Sig.Field;
import edu.mit.csail.sdg.ast.Sig.PrimSig;
import edu.mit.csail.sdg.ast.Type.ProductType;
import edu.mit.csail.sdg.parser.CompModule;

import java.util.*;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicInteger;

import static ar.edu.unrc.dc.mutation.Cheats.cheatedClone;


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

    /**
     * Obtains the function that contains the given expression
     *
     * @param x : the expression
     * @return {@link Optional#empty()} if the {@code expression} is not contained
     *         in a function, or an {@code Optional} containing the function
     */
    protected final Optional<Func> getContainerFunc(Expr x) {
        Browsable current = x;
        while (current != null && !(current instanceof Func)) {
            current = current.getBrowsableParent();
        }
        if (current == null)
            return Optional.empty();
        return Optional.of((Func) current);
    }

    /**
     * Obtains compatible variables or signature fields to replace a given
     * expression
     *
     * @param x : the expression
     * @return an {@code Optional} list of expressions {@code Expr} or
     *         {@link Optional#empty()} if none is found
     */
    protected final Optional<List<Expr>> getCompatibleVariablesFor(Expr x, boolean strictChecking) {
        Type xtype = x.type();
        List<Expr> compatibleVariables = new LinkedList<>();
        Map<String,Expr> funcVariablesFound = new HashMap<>();
        Map<String,Expr> variablesFound = new HashMap<>();
        Optional<Func> containerFunc = getContainerFunc(x);
        if (containerFunc.isPresent()) {
            for (Decl arg : containerFunc.get().decls) {
                Type argType = arg.expr.type();
                if (compatibleVariablesChecker(x, argType, strictChecking)) {
                    for (ExprHasName var : arg.names) {
                        funcVariablesFound.put(cleanLabelFromThis(var.label), var);
                    }
                }
            }
        }
        SafeList<Sig> sigs = this.context.getAllSigs();
        List<Decl> sigDecls = new LinkedList<>();
        for (Sig s : sigs) {
            Type stype = s.type();
            if (compatibleVariablesChecker(x, stype, strictChecking)) {
                String label = cleanLabelFromThis(s.label);
                if (!funcVariablesFound.containsKey(label)) { //a funcs argument will hide all other fields
                    variablesFound.put(label, s);
                }
            }
            for (Decl d : s.getFieldDecls()) {
                Type dtype = appendTypes(stype, d.expr.type());
                if (compatibleVariablesChecker(x, dtype, strictChecking)) {
                    for (Expr var : d.names) {
                        String label;
                        if (var instanceof Field) {
                            label = cleanLabelFromThis(((Field) var).label);
                        } else if (var instanceof ExprHasName) {
                            label = cleanLabelFromThis(((ExprHasName) var).label);
                        } else {
                            throw new IllegalStateException("While searching for sig " + s.label + " fields, found " + var.toString() + " which is not a Field nor an ExprHasName, but instead a " + var.getClass().getCanonicalName());
                        }
                        if (!funcVariablesFound.containsKey(label)) {
                            variablesFound.put(label, var);
                        }
                    }
                }

            }
        }
        compatibleVariables.addAll(variablesFound.values());
        compatibleVariables.addAll(funcVariablesFound.values());
        if (!compatibleVariables.isEmpty())
            return Optional.of(compatibleVariables);
        return Optional.empty();
    }

    protected final Type getType(Expr e) {
        //        if (e instanceof Field) {
        //            Field eAsField = (Field) e;
        //            return appendTypes(eAsField.sig.type(), e.type());
        //        }
        return e.type();
    }

    protected final Type appendTypes(Type atype, Type btype) {
        if (atype.is_bool || atype.is_int())
            return Type.EMPTY;
        if (btype.is_bool || btype.is_int())
            return Type.EMPTY;
        List<ProductType> rtypes = new LinkedList<>();
        AtomicInteger arities = new AtomicInteger(0);
        Iterator<ProductType> atypesIt = atype.iterator();
        Iterator<ProductType> btypesIt = btype.iterator();
        atypesIt.forEachRemaining(at -> {
            rtypes.add(at);
            arities.addAndGet(at.arity());
        });
        btypesIt.forEachRemaining(bt -> {
            rtypes.add(bt);
            arities.addAndGet(bt.arity());
        });
        List<PrimSig> primSigs = new LinkedList<>();
        rtypes.forEach(prodType -> {
            Collections.addAll(primSigs, prodType.getAll());
        });
        List<ProductType> rProductTypes = Collections.singletonList(new ProductType(primSigs.toArray(new PrimSig[primSigs.size()])));
        ConstList<ProductType> rtypesConstList = ConstList.make(rProductTypes);
        return new Type(false, rtypesConstList, arities.get());
    }

    protected boolean isMemberOfBinaryExpression(ExprBinary binary, Expr expr) {
        return (binary.left.getID() == expr.getID()) || (binary.right.getID() == expr.getID());
    }

    protected boolean compatibleVariablesChecker(Expr toReplace, Type replacementType, boolean strictTypeChecking) {
        return Mutator.compatibleVariablesChecker(toReplace.type(), replacementType, strictTypeChecking);
    }

    public static boolean compatibleVariablesChecker(Type toReplaceType, Type replacementType, boolean strictTypeChecking) {
        if (strictTypeChecking)
            return toReplaceType.equals(replacementType);
        Optional<Sig> toReplaceFirst = getFirst(toReplaceType);
        Optional<Sig> replacementFirst = getFirst(replacementType);
        Optional<Sig> toReplaceLast = getLast(toReplaceType);
        Optional<Sig> replacementLast = getLast(replacementType);
        if (!toReplaceFirst.isPresent() || !replacementFirst.isPresent())
            return false;
        if (!toReplaceLast.isPresent() || !replacementLast.isPresent())
            return false;
        return compatibleTypes(toReplaceFirst.get(), replacementFirst.get()) && compatibleTypes(toReplaceLast.get(), replacementLast.get());
    }

    //This method should use a cache
    protected Optional<List<Expr>> getAllTypes(int min, int max) throws CheatingIsBadMkay {
        List<Expr> result = new LinkedList<>();
        SafeList<Sig> sigs = this.context.getAllSigs();
        List<Expr> sigsList = new LinkedList<>();
        Map<Expr,Type> fields = new HashMap<>();
        for (Sig s : sigs) {
            Type stype = s.type();
            sigsList.add(s);
            for (Decl d : s.getFieldDecls()) {
                Type dtype = appendTypes(stype, d.expr.type());
                for (Expr var : d.names) {
                    fields.put(var, dtype);
                }
            }
        }
        combineSigsAndFields(sigsList, fields, result, min, max);
        if (!result.isEmpty()) {
            return Optional.of(result);
        }
        return Optional.empty();
    }

    private void combineSigsAndFields(List<Expr> sigs, Map<Expr,Type> fields, List<Expr> output, int min, int max) throws CheatingIsBadMkay {
        boolean modified = true;
        List<Map<Expr,Type>> combinations = new LinkedList<>();
        Map<Expr,Type> firstGeneration = new HashMap<>();
        for (Expr s : sigs) {
            Expr sclone = cheatedClone(s);
            Type stype = s.type();
            firstGeneration.put(sclone, stype);
        }
        firstGeneration.putAll(fields);
        if (min == 1)
            output.addAll(firstGeneration.keySet());
        combinations.add(firstGeneration);
        int lastGeneratedGen = 1;
        while (modified && lastGeneratedGen < max) {
            Map<Expr,Type> from = combinations.get(lastGeneratedGen - 1);
            Map<Expr,Type> newGeneration = new HashMap<>();
            modified = false;
            for (Entry<Expr,Type> comb : from.entrySet()) {
                Type ctype = comb.getValue();
                Expr combExpr = comb.getKey();
                for (Expr s : sigs) {
                    Type sType = s.type();
                    Type joinType = ctype.join(sType);
                    if (!emptyOrNone(joinType)) {
                        Expr combExprClone = cheatedClone(combExpr, ctype);
                        Expr sclone = cheatedClone(s);
                        Expr newCombination = ExprBinary.Op.JOIN.make(combExpr.pos, s.closingBracket, combExprClone, sclone);
                        Type newCombinationType = getType(newCombination);
                        newGeneration.put(newCombination, getType(newCombination));
                        modified = true;
                    }
                }
                for (Entry<Expr,Type> field : fields.entrySet()) {
                    Type ftype = field.getValue();
                    Expr f = field.getKey();
                    Type joinType = ctype.join(ftype);
                    if (!emptyOrNone(joinType)) {
                        Expr combExprClone = cheatedClone(combExpr, ctype);
                        Expr fclone = cheatedClone(f, ftype);
                        Expr newCombination = ExprBinary.Op.JOIN.make(combExpr.pos, f.closingBracket, combExprClone, fclone);
                        newGeneration.put(newCombination, getType(newCombination));
                        modified = true;
                    }
                }
            }
            if (modified) {
                lastGeneratedGen++;
                combinations.add(newGeneration);
                if (lastGeneratedGen >= min) {
                    output.addAll(newGeneration.keySet());
                }
            }
        }
    }

    private static Optional<Sig> getFirst(Type t) {
        if (t.arity() < 1)
            return Optional.empty();
        Iterator<ProductType> it = t.iterator();
        ProductType first = it.hasNext() ? it.next() : null;
        if (first == null)
            return Optional.empty();
        PrimSig[] types = first.getAll();
        if (types.length == 0 || types[0] == null)
            return Optional.empty();
        return Optional.of(types[0]);
    }

    private static Optional<Sig> getLast(Type t) {
        if (t.arity() < 1)
            return Optional.empty();
        Iterator<ProductType> it = t.iterator();
        ProductType last = null;
        while (it.hasNext()) {
            last = it.next();
        }
        if (last == null)
            return Optional.empty();
        PrimSig[] types = last.getAll();
        if (types.length == 0 || types[types.length - 1] == null)
            return Optional.empty();
        return Optional.of(types[types.length - 1]);
    }

    private static boolean compatibleTypes(Sig a, Sig b) {
        return a.isSameOrDescendentOf(b) || b.isSameOrDescendentOf(a);
    }

    protected final boolean emptyOrNone(Type joinedType) {
        if (joinedType.toString().equals(Type.EMPTY.toString()))
            return true;
        for (ProductType productType : joinedType) {
            if (productType.isEmpty())
                return true;
        }
        return false;
    }

    protected final String cleanLabelFromThis(String label) {
        if (label.startsWith("this/"))
            return label.substring(5);
        return label;
    }

    /**
     * Evaluates whether an expression is part of a definition (Signature field,
     * predicate parameters, etc)
     *
     * @param x : the expression
     * @return {@code true} iff {@code x} is part of a definition
     */
    protected boolean belongsToDefinition(Expr x) {
        //if an expression belongs to a definition is must be either a function argument (belongs to Func#decls)
        throw new UnsupportedOperationException("Not implemented yet");
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

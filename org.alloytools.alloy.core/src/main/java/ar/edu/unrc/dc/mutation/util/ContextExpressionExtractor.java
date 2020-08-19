package ar.edu.unrc.dc.mutation.util;

import ar.edu.unrc.dc.mutation.CheatingIsBadMkay;
import ar.edu.unrc.dc.mutation.visitors.SearchExpr;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.SafeList;
import edu.mit.csail.sdg.ast.*;
import edu.mit.csail.sdg.ast.Sig.PrimSig;
import edu.mit.csail.sdg.ast.Type.ProductType;
import edu.mit.csail.sdg.parser.CompModule;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

import static ar.edu.unrc.dc.mutation.Cheats.cheatedClone;

public final class ContextExpressionExtractor {

    private static CompModule context;

    public synchronized static void initialize(CompModule context) {
        if (ContextExpressionExtractor.context != null)
            throw new IllegalStateException("ContextExpressionExtractor already initialized, use reInitialize if you want to change the associated CompModule");
        if (context == null)
            throw new IllegalArgumentException("context can't be null");
        ContextExpressionExtractor.context = context;
    }

    public synchronized static void reInitialize(CompModule context) {
        ContextExpressionExtractor.context = null;
        initialize(context);
    }

    public synchronized static void clear() {
        ContextExpressionExtractor.context = null;
    }

    private synchronized static boolean validateInstance() {
        return context != null;
    }

    public synchronized static Optional<List<Expr>> getAllVariablesFor(Expr target) throws CheatingIsBadMkay {
        return getVariables(target, false, false);
    }

    public synchronized static Optional<List<Expr>> getCompatibleVariablesFor(Expr target, boolean strictTypeCheck) throws CheatingIsBadMkay {
        return getVariables(target, true, strictTypeCheck);
    }

    private synchronized static Optional<List<Expr>> getVariables(Expr target, boolean typeCheck, boolean strictTypeCheck) throws CheatingIsBadMkay {
        List<Expr> clonedVars = new LinkedList<>();
        Optional<List<Expr>> vars = getCompatibleVariablesFor(target, typeCheck, strictTypeCheck);
        if (vars.isPresent()) {
            for (Expr x : vars.get()) {
                Expr varClone = cheatedClone(x);
                varClone.newID();
                clonedVars.add(varClone);
            }
        }
        return clonedVars.isEmpty()?Optional.empty():Optional.of(clonedVars);
    }

    private synchronized static Optional<List<Expr>> getCompatibleVariablesFor(Expr target, boolean typeCheck, boolean strictTypeCheck) throws CheatingIsBadMkay {
        Set<Expr> result = new TreeSet<>((Comparator.comparing(Expr::toString)));
        Optional<Func> containerFunc = getContainerFunc(target);
        if (containerFunc.isPresent()) {
            for (Decl formalArg : containerFunc.get().decls) {
                Type argsType = formalArg.expr.type();
                if (!typeCheck || TypeChecking.canReplace(target, argsType, strictTypeCheck))
                    result.addAll(formalArg.names);
            }
        }
        Optional<List<Expr>> localVariables = getLocalVariables(target);
        if (localVariables.isPresent()) {
            for (Expr localVar : localVariables.get()) {
                Type localVarType = localVar.type();
                if (!typeCheck || TypeChecking.canReplace(target, localVarType, strictTypeCheck)) {
                    result.add(localVar);
                }
            }
        }
        Optional<List<Expr>> sigsAndDecls = getSigsAndDecls();
        if (sigsAndDecls.isPresent()) {
            for (Expr sigOrField : sigsAndDecls.get()) {
                Type sigOrFieldType = sigOrField.type();
                if (!typeCheck || TypeChecking.canReplace(target, sigOrFieldType, strictTypeCheck)) {
                    result.add(sigOrField);
                }
            }
        }
        List<Expr> resultAsList = new LinkedList<>(result);
        return resultAsList.isEmpty()?Optional.empty():Optional.of(resultAsList);
    }


    /**
     * Obtains the function that contains the given expression
     *
     * @param x : the expression
     * @return {@link Optional#empty()} if the {@code expression} is not contained
     *         in a function, or an {@code Optional} containing the function
     */
    public synchronized static Optional<Func> getContainerFunc(Expr x) {
        if (!validateInstance())
            throw new IllegalStateException("The method initialize must be run once before running any other method");
        Browsable current = x;
        while (current != null && !(current instanceof Func)) {
            current = current.getBrowsableParent();
        }
        Optional<Func> result;
        if (current == null)
            result = Optional.empty();
        else
            result = Optional.of((Func)current);
        return result;
    }

    public synchronized static Optional<List<Expr>> getLocalVariables(Expr x) throws CheatingIsBadMkay {
        if (!validateInstance())
            throw new IllegalStateException("The method initialize must be run once before running any other method");
        Set<Expr> localVariablesFound = new TreeSet<>(Comparator.comparing(Expr::toString));
        Expr mayorExpr = TypeChecking.getMayorExpression(x, context);
        Browsable current = x;
        Browsable parent = current.getBrowsableParent();
        SearchExpr searcher = new SearchExpr((Expr) current);
        while (current != mayorExpr) {
            if (parent instanceof ExprQt) {
                ExprQt parentAsQt = (ExprQt) parent;
                if (searcher.visitThis(parentAsQt.sub)) { //current is inside the formula of a quantified expression
                    for (Decl d : parentAsQt.decls) {
                        //will only add var if there is not already a variable with the same name
                        for (Expr var : d.names) {
                            Expr varClone = cheatedClone(var);
                            varClone.newID();
                            localVariablesFound.add(varClone);
                        }
                    }
                }
            } else if (parent instanceof ExprLet) {
                ExprLet parentAsLet = (ExprLet) parent;
                if (searcher.visitThis(parentAsLet.sub)) { //current is inside the body of the let expression
                    Expr varClone = cheatedClone(parentAsLet.var);
                    varClone.newID();
                    localVariablesFound.add(varClone);
                }
            }
            current = parent;
            parent = current.getBrowsableParent();
            searcher = new SearchExpr((Expr) current);
        }
        Optional<Func> containerFunc = getContainerFunc(x);
        if (containerFunc.isPresent()) {
            for (Decl d : containerFunc.get().decls) {
                for (Expr var : d.names) {
                    Expr varClone = cheatedClone(var);
                    varClone.newID();
                    localVariablesFound.add(varClone);
                }
            }
        }
        Optional<List<Expr>> result;
        if (localVariablesFound.isEmpty()) {
            result = Optional.empty();
        } else {
            result = Optional.of(new LinkedList<>(localVariablesFound));
        }
        return result;
    }

    public synchronized static Optional<List<Expr>> getSigsAndDecls() throws CheatingIsBadMkay {
        if (!validateInstance())
            throw new IllegalStateException("The method initialize must be run once before running any other method");
        List<Expr> collectedSigsAndDecls = new LinkedList<>();
        SafeList<Sig> sigs = context.getAllSigs();
        for (Sig s : sigs) {
            if (s.isVariabilizationTestRelatedSig())
                continue;
            Sig sClone = (Sig) cheatedClone(s);
            sClone.newID();
            collectedSigsAndDecls.add(sClone);
            for (Decl d : s.getFieldDecls()) {
                for (Expr f : d.names) {
                    Expr fClone = cheatedClone(f);
                    fClone.newID();
                    collectedSigsAndDecls.add(fClone);
                }
            }
        }
        return collectedSigsAndDecls.isEmpty()?Optional.empty():Optional.of(collectedSigsAndDecls);
    }

    public synchronized static Optional<List<Expr>> getCombinedSigAndDecls(int min, int max) throws CheatingIsBadMkay {
        if (!validateInstance())
            throw new IllegalStateException("The method initialize must be run once before running any other method");
        List<Expr> result = new LinkedList<>();
        SafeList<Sig> sigs = context.getAllSigs();
        List<Expr> sigsList = new LinkedList<>();
        Map<Expr,Type> fields = new HashMap<>();
        for (Sig s : sigs) {
            if (s.isVariabilizationTestRelatedSig())
                continue;
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
        return result.isEmpty()?Optional.empty():Optional.of(result);
    }

    private synchronized static void combineSigsAndFields(List<Expr> sigs, Map<Expr,Type> fields, List<Expr> output, int min, int max) throws CheatingIsBadMkay {
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
            for (Map.Entry<Expr,Type> comb : from.entrySet()) {
                Type ctype = comb.getValue();
                Expr combExpr = comb.getKey();
                for (Expr s : sigs) {
                    Type sType = s.type();
                    Type joinType = ctype.join(sType);
                    if (!TypeChecking.emptyOrNone(joinType)) {
                        Expr combExprClone = cheatedClone(combExpr, ctype);
                        Expr sclone = cheatedClone(s);
                        Expr newCombination = ExprBinary.Op.JOIN.make(combExpr.pos, s.closingBracket, combExprClone, sclone);
                        newGeneration.put(newCombination, TypeChecking.getType(newCombination));
                        modified = true;
                    }
                }
                for (Map.Entry<Expr,Type> field : fields.entrySet()) {
                    Type ftype = field.getValue();
                    Expr f = field.getKey();
                    Type joinType = ctype.join(ftype);
                    if (!TypeChecking.emptyOrNone(joinType)) {
                        Expr combExprClone = cheatedClone(combExpr, ctype);
                        Expr fclone = cheatedClone(f, ftype);
                        Expr newCombination = ExprBinary.Op.JOIN.make(combExpr.pos, f.closingBracket, combExprClone, fclone);
                        newGeneration.put(newCombination, TypeChecking.getType(newCombination));
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

    private synchronized static Type appendTypes(Type atype, Type btype) {
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
        rtypes.forEach(prodType -> Collections.addAll(primSigs, prodType.getAll()));
        List<ProductType> rProductTypes = Collections.singletonList(new ProductType(primSigs.toArray(new PrimSig[primSigs.size()])));
        ConstList<ProductType> rtypesConstList = ConstList.make(rProductTypes);
        return new Type(false, rtypesConstList, arities.get());
    }


}

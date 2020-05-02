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
    private static Map<Integer, Optional<Func>> functions;
    private static Map<Integer, Optional<List<Expr>>> localVariables;
    private static Map<Integer, Optional<List<Expr>>> compatibleVariablesFor;
    private static Map<Integer, Optional<List<Expr>>> allVariablesFor;
    private static Optional<List<Expr>> sigsAndDecls;
    private static Optional<List<Expr>> combinedSigAndDecls;

    public synchronized static void initialize(CompModule context) {
        if (ContextExpressionExtractor.context != null)
            throw new IllegalStateException("ContextExpressionExtractor already initialized, use reInitialize if you want to change the associated CompModule");
        if (context == null)
            throw new IllegalArgumentException("context can't be null");
        ContextExpressionExtractor.context = context;
        functions = new HashMap<>();
        localVariables = new HashMap<>();
        compatibleVariablesFor = new HashMap<>();
        allVariablesFor = new HashMap<>();
        sigsAndDecls = null;
        combinedSigAndDecls = null;
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
        List<Expr> clonedVars = new LinkedList<>();
        Optional<List<Expr>> vars = getCompatibleVariablesFor(target, false, false);
        if (vars.isPresent()) {
            for (Expr x : vars.get()) {
                clonedVars.add(cheatedClone(x));
            }
        }
        return clonedVars.isEmpty()?Optional.empty():Optional.of(clonedVars);
    }

    public synchronized static Optional<List<Expr>> getCompatibleVariablesFor(Expr target, boolean strictTypeCheck) throws CheatingIsBadMkay {
        List<Expr> clonedVars = new LinkedList<>();
        Optional<List<Expr>> vars = getCompatibleVariablesFor(target, true, strictTypeCheck);
        if (vars.isPresent()) {
            for (Expr x : vars.get()) {
                clonedVars.add(cheatedClone(x));
            }
        }
        return clonedVars.isEmpty()?Optional.empty():Optional.of(clonedVars);
    }

    private synchronized static Optional<List<Expr>> getCompatibleVariablesFor(Expr target, boolean typeCheck, boolean strictTypeCheck) {
        if (!typeCheck && allVariablesFor.containsKey(target.getID()))
            return allVariablesFor.get(target.getID());
        if (typeCheck && compatibleVariablesFor.containsKey(target.getID()))
            return compatibleVariablesFor.get(target.getID());
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
        if (typeCheck)
            compatibleVariablesFor.put(target.getID(), result.isEmpty()?Optional.empty():Optional.of(resultAsList));
        else
            allVariablesFor.put(target.getID(), result.isEmpty()?Optional.empty():Optional.of(resultAsList));
        return typeCheck?compatibleVariablesFor.get(target.getID()):allVariablesFor.get(target.getID());
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
        if (functions.containsKey(x.getID())) {
            return functions.get(x.getID());
        }
        Set<Integer> visitedIds = new TreeSet<>();
        Browsable current = x;
        while (current != null && !(current instanceof Func)) {
            visitedIds.add(current.getID());
            current = current.getBrowsableParent();
            if (current != null && functions.containsKey(current.getID()))
                return functions.get(current.getID());
        }
        Optional<Func> result;
        if (current == null)
            result = Optional.empty();
        else
            result = Optional.of((Func)current);
        for (Integer id : visitedIds) {
            functions.put(id, result);
        }
        return result;
    }

    public synchronized static Optional<List<Expr>> getLocalVariables(Expr x) {
        if (!validateInstance())
            throw new IllegalStateException("The method initialize must be run once before running any other method");
        if (localVariables.containsKey(x.getID())) {
            return localVariables.get(x.getID());
        }
        Set<Expr> localVariablesFound = new TreeSet<>(Comparator.comparing(Expr::toString));
        Set<Integer> visitedIds = new TreeSet<>();
        Expr mayorExpr = TypeChecking.getMayorExpression(x);
        Browsable current = x;
        Browsable parent = current.getBrowsableParent();
        SearchExpr searcher = new SearchExpr((Expr) current);
        visitedIds.add(current.getID());
        while (current != mayorExpr) {
            if (parent instanceof ExprQt) {
                ExprQt parentAsQt = (ExprQt) parent;
                if (searcher.visitThis(parentAsQt.sub)) { //current is inside the formula of a quantified expression
                    visitedIds.add(current.getID());
                    for (Decl d : parentAsQt.decls) {
                        //will only add var if there is not already a variable with the same name
                        localVariablesFound.addAll(d.names);
                    }
                }
            } else if (parent instanceof ExprLet) {
                ExprLet parentAsLet = (ExprLet) parent;
                if (searcher.visitThis(parentAsLet.sub)) { //current is inside the body of the let expression
                    visitedIds.add(current.getID());
                    localVariablesFound.add(parentAsLet.var);
                }
            }
            current = parent;
            parent = current.getBrowsableParent();
            searcher = new SearchExpr((Expr) current);
        }
        Optional<Func> containerFunc = getContainerFunc(x);
        containerFunc.ifPresent(f -> f.decls.forEach(d -> localVariablesFound.addAll(d.names)));
        Optional<List<Expr>> result;
        if (localVariablesFound.isEmpty()) {
            result = Optional.empty();
        } else {
            result = Optional.of(new LinkedList<>(localVariablesFound));
        }
        for (Integer id : visitedIds)
            localVariables.put(id, result);
        return result;
    }

    public synchronized static Optional<List<Expr>> getSigsAndDecls() {
        if (!validateInstance())
            throw new IllegalStateException("The method initialize must be run once before running any other method");
        if (sigsAndDecls == null) {
            List<Expr> collectedSigsAndDecls = new LinkedList<>();
            SafeList<Sig> sigs = context.getAllSigs();
            for (Sig s : sigs) {
                if (s.isVariabilizationTestRelatedSig())
                    continue;
                collectedSigsAndDecls.add(s);
                for (Decl d : s.getFieldDecls()) {
                    collectedSigsAndDecls.addAll(d.names);
                }
            }
            if (collectedSigsAndDecls.isEmpty()) {
                sigsAndDecls = Optional.empty();
            } else {
                sigsAndDecls = Optional.of(collectedSigsAndDecls);
            }
        }
        return sigsAndDecls;
    }

    public synchronized static Optional<List<Expr>> getCombinedSigAndDecls(int min, int max) throws CheatingIsBadMkay {
        if (!validateInstance())
            throw new IllegalStateException("The method initialize must be run once before running any other method");
        if (combinedSigAndDecls != null)
            return combinedSigAndDecls;
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
        if (!result.isEmpty()) {
            combinedSigAndDecls = Optional.of(result);
        } else {
            combinedSigAndDecls = Optional.empty();
        }
        return combinedSigAndDecls;
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

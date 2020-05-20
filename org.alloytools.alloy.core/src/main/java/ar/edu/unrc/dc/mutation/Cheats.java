package ar.edu.unrc.dc.mutation;

import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.ast.*;
import edu.mit.csail.sdg.parser.CompModule;

import java.lang.reflect.Field;
import java.util.*;
import java.util.Map.Entry;

/**
 * When Alloy won't comply use cheats.
 * <p>
 * <b>Use this class at your own risk</b>
 */
public final class Cheats {

    public static void changeExpressionType(Expr expr, Type newType) throws CheatingIsBadMkay {
        Field typeField;
        try {
            typeField = getField(Expr.class, "type");
            if (typeField == null) {
                throw new CheatingIsBadMkay("Field type for class Expr was not found");
            }
            boolean oldAccessibleStatus = setAccessibleStatus(typeField, true);
            typeField.set(expr, newType);
            setAccessibleStatus(typeField, oldAccessibleStatus);
        } catch (SecurityException | IllegalArgumentException | IllegalAccessException e) {
            throw new CheatingIsBadMkay("Your cheats didn't work", e);
        }
    }

    public static Expr cheatedClone(Expr x) throws CheatingIsBadMkay {
        Type originalType = x.type();
        return cheatedClone(x, originalType);
    }

    public static Expr cheatedClone(Expr x, Type t) throws CheatingIsBadMkay {
        Expr clone = (Expr) x.clone();
        Cheats.changeExpressionType(clone, t);
        return clone;
    }

    public static void changeBinaryLeftField(ExprBinary original, Expr leftReplacement) throws CheatingIsBadMkay {
        Field leftField = getField(ExprBinary.class, "left");
        if (leftField == null)
            throw new CheatingIsBadMkay("Couldn't find left field");
        try {
            boolean oldAccessibleStatus = setAccessibleStatus(leftField, true);
            leftField.set(original, leftReplacement);
            leftReplacement.setBrowsableParent(original);
            setAccessibleStatus(leftField, oldAccessibleStatus);
        } catch (IllegalAccessException e) {
            throw new CheatingIsBadMkay("There was a problem while trying to change left field", e);
        }
    }

    public static void changeBinaryRightField(ExprBinary original, Expr rightReplacement) throws CheatingIsBadMkay {
        Field rightField = getField(ExprBinary.class, "right");
        if (rightField == null)
            throw new CheatingIsBadMkay("Couldn't find right field");
        try {
            boolean oldAccessibleStatus = setAccessibleStatus(rightField, true);
            rightField.set(original, rightReplacement);
            rightReplacement.setBrowsableParent(original);
            setAccessibleStatus(rightField, oldAccessibleStatus);
        } catch (IllegalAccessException e) {
            throw new CheatingIsBadMkay("There was a problem while trying to change right field", e);
        }
    }

    public static void changeUnarySubField(ExprUnary original, Expr sub) throws CheatingIsBadMkay {
        Field subField = getField(ExprUnary.class, "sub");
        if (subField == null)
            throw new CheatingIsBadMkay("Couldn't find sub field");
        try {
            boolean oldAccessibleStatus = setAccessibleStatus(subField, true);
            subField.set(original, sub);
            sub.setBrowsableParent(original);
            setAccessibleStatus(subField, oldAccessibleStatus);
        } catch (IllegalAccessException e) {
            throw new CheatingIsBadMkay("There was a problem while trying to change sub field", e);
        }
    }

    public static void changeQtFormulaField(ExprQt original, Expr formulaReplacement) throws CheatingIsBadMkay {
        Field subField = getField(ExprQt.class, "sub");
        if (subField == null)
            throw new CheatingIsBadMkay("Couldn't find sub field");
        try {
            boolean oldAccessibleStatus = setAccessibleStatus(subField, true);
            subField.set(original, formulaReplacement);
            formulaReplacement.setBrowsableParent(original);
            setAccessibleStatus(subField, oldAccessibleStatus);
        } catch (IllegalAccessException e) {
            throw new CheatingIsBadMkay("There was a problem while trying to change sub field", e);
        }
    }

    public static void changeQtBoundFieldFor(ExprQt original, Expr boundReplacement, Decl target) throws CheatingIsBadMkay {
        boolean targetFound = false;
        for (Decl d : original.decls) {
            if (d.equals(target)) {
                Field boundField = getField(Decl.class, "expr");
                if (boundField == null)
                    throw new CheatingIsBadMkay("Couldn't find expr field for target decl");
                try {
                    targetFound = true;
                    boolean oldAccessibleStatus = setAccessibleStatus(boundField, true);
                    boundField.set(target, boundReplacement);
                    boundReplacement.setBrowsableParent(original);
                    setAccessibleStatus(boundField, oldAccessibleStatus);
                } catch (IllegalAccessException e) {
                    throw new CheatingIsBadMkay("There was a problem while trying to change expr field for target decl", e);
                }
            }
        }
        if (!targetFound)
            throw new CheatingIsBadMkay("Couldn't find target decl in quantified expression");
    }

    public static void changeQtVar(ExprQt original, ExprHasName target, ExprVar replacement) throws CheatingIsBadMkay {
        boolean targetFound = false;
        for (Decl d : original.decls) {
            if (hasVar(d, target)) {
                targetFound = true;
                List<ExprHasName> varsCopy = new LinkedList<>();
                for (ExprHasName dvar : d.names) {
                    if (dvar.getID() == target.getID()) {
                        varsCopy.add(replacement);
                        replacement.setBrowsableParent(original);
                    } else {
                        varsCopy.add(dvar);
                    }
                }
                Field namesField = getField(Decl.class, "names");
                if (namesField == null)
                    throw new CheatingIsBadMkay("Couldn't find names field for current decl");
                try {
                    boolean oldAccessibleStatus = setAccessibleStatus(namesField, true);
                    namesField.set(d, ConstList.make(varsCopy));
                    setAccessibleStatus(namesField, oldAccessibleStatus);
                } catch (IllegalAccessException e) {
                    throw new CheatingIsBadMkay("There was a problem while trying to change names field for current decl", e);
                }
            }
        }
        if (!targetFound)
            throw new CheatingIsBadMkay("Couldn't find target var in quantified expression");
    }

    private static boolean hasVar(Decl d, ExprHasName var) {
        for (Expr dvar : d.names) {
            if (dvar.getID() == var.getID())
                return true;
        }
        return false;
    }

    public static void changeLetExpr(ExprLet original, Expr replacement) throws CheatingIsBadMkay {
        Field exprField = getField(ExprLet.class, "expr");
        if (exprField == null)
            throw new CheatingIsBadMkay("Couldn't find expr field");
        try {
            boolean oldAccessibleStatus = setAccessibleStatus(exprField, true);
            exprField.set(original, replacement);
            replacement.setBrowsableParent(original);
            setAccessibleStatus(exprField, oldAccessibleStatus);
        } catch (IllegalAccessException e) {
            throw new CheatingIsBadMkay("There was a problem while trying to change expr field", e);
        }
    }

    public static void changeLetSub(ExprLet original, Expr replacement) throws CheatingIsBadMkay {
        Field subField = getField(ExprLet.class, "sub");
        if (subField == null)
            throw new CheatingIsBadMkay("Couldn't find sub field");
        try {
            boolean oldAccessibleStatus = setAccessibleStatus(subField, true);
            subField.set(original, replacement);
            replacement.setBrowsableParent(original);
            setAccessibleStatus(subField, oldAccessibleStatus);
        } catch (IllegalAccessException e) {
            throw new CheatingIsBadMkay("There was a problem while trying to change sub field", e);
        }
    }

    public static void changeLetVar(ExprLet original, ExprVar replacement) throws CheatingIsBadMkay {
        Field varField = getField(ExprLet.class, "var");
        if (varField == null)
            throw new CheatingIsBadMkay("Couldn't find var field");
        try {
            boolean oldAccessibleStatus = setAccessibleStatus(varField, true);
            varField.set(original, replacement);
            replacement.setBrowsableParent(original);
            setAccessibleStatus(varField, oldAccessibleStatus);
        } catch (IllegalAccessException e) {
            throw new CheatingIsBadMkay("There was a problem while trying to change var field", e);
        }
    }

    public static void changeListElement(ExprList original, Expr target, Expr replacement) throws CheatingIsBadMkay {
        boolean targetFound = false;
        List<Expr> newArgs = new LinkedList<>();
        for (Expr arg : original.args) {
            if (arg.getID() == target.getID()) {
                targetFound = true;
                newArgs.add(replacement);
            } else
                newArgs.add(arg);
        }
        if (!targetFound)
            throw new CheatingIsBadMkay("Couldn't find target arg in list expression");
        ConstList<Expr> newArgsConstList = ConstList.make(newArgs);
        Field argsField = getField(ExprList.class, "args");
        if (argsField == null)
            throw new CheatingIsBadMkay("Couldn't find args field");
        try {
            boolean oldAccessibleStatus = setAccessibleStatus(argsField, true);
            argsField.set(original, newArgsConstList);
            replacement.setBrowsableParent(original);
            setAccessibleStatus(argsField, oldAccessibleStatus);
        } catch (IllegalAccessException e) {
            throw new CheatingIsBadMkay("There was a problem while trying to change expr field for target decl", e);
        }
    }

    public static void changeFuncBody(Func original, Expr body) throws CheatingIsBadMkay {
        Field bodyField = getField(Func.class, "body");
        if (bodyField == null)
            throw new CheatingIsBadMkay("Couldn't find body field");
        try {
            boolean oldAccessibleStatus = setAccessibleStatus(bodyField, true);
            bodyField.set(original, body);
            body.setBrowsableParent(original);
            setAccessibleStatus(bodyField, oldAccessibleStatus);
        } catch (IllegalAccessException e) {
            throw new CheatingIsBadMkay("There was a problem while trying to change function's body field", e);
        }
    }

    public static void changeCallArgument(ExprCall original, Expr target, Expr replacement) throws CheatingIsBadMkay {
        boolean targetFound = false;
        List<Expr> newArgs = new LinkedList<>();
        for (Expr arg : original.args) {
            if (arg.getID() == target.getID()) {
                targetFound = true;
                newArgs.add(replacement);
            } else
                newArgs.add(arg);
        }
        if (!targetFound)
            throw new CheatingIsBadMkay("Couldn't find target arg in call arguments");
        ConstList<Expr> newArgsConstList = ConstList.make(newArgs);
        Field argsField = getField(ExprCall.class, "args");
        if (argsField == null)
            throw new CheatingIsBadMkay("Couldn't find args field");
        try {
            boolean oldAccessibleStatus = setAccessibleStatus(argsField, true);
            argsField.set(original, newArgsConstList);
            replacement.setBrowsableParent(original);
            setAccessibleStatus(argsField, oldAccessibleStatus);
        } catch (IllegalAccessException e) {
            throw new CheatingIsBadMkay("There was a problem while trying to change expr field for target argument", e);
        }
    }

    public static void changeITECondition(ExprITE original, Expr replacement) throws CheatingIsBadMkay {
        Field condField = getField(ExprITE.class, "cond");
        if (condField == null)
            throw new CheatingIsBadMkay("Couldn't find cond field");
        try {
            boolean oldAccessibleStatus = setAccessibleStatus(condField, true);
            condField.set(original, replacement);
            replacement.setBrowsableParent(original);
            setAccessibleStatus(condField, oldAccessibleStatus);
        } catch (IllegalAccessException e) {
            throw new CheatingIsBadMkay("There was a problem while trying to change ITE cond field", e);
        }
    }

    public static void changeITEThen(ExprITE original, Expr replacement) throws CheatingIsBadMkay {
        Field leftField = getField(ExprITE.class, "left");
        if (leftField == null)
            throw new CheatingIsBadMkay("Couldn't find left field");
        try {
            boolean oldAccessibleStatus = setAccessibleStatus(leftField, true);
            leftField.set(original, replacement);
            replacement.setBrowsableParent(original);
            setAccessibleStatus(leftField, oldAccessibleStatus);
        } catch (IllegalAccessException e) {
            throw new CheatingIsBadMkay("There was a problem while trying to change ITE left field", e);
        }
    }

    public static void changeITETElse(ExprITE original, Expr replacement) throws CheatingIsBadMkay {
        Field rightField = getField(ExprITE.class, "right");
        if (rightField == null)
            throw new CheatingIsBadMkay("Couldn't find right field");
        try {
            boolean oldAccessibleStatus = setAccessibleStatus(rightField, true);
            rightField.set(original, replacement);
            replacement.setBrowsableParent(original);
            setAccessibleStatus(rightField, oldAccessibleStatus);
        } catch (IllegalAccessException e) {
            throw new CheatingIsBadMkay("There was a problem while trying to change ITE right field", e);
        }
    }

    public static void changeAssertion(Expr target, Expr replacement, CompModule inAst) throws CheatingIsBadMkay {
        for (Pair<String, Expr> assertion : inAst.getAllAssertions()) {
            if (assertion.b.getID() == target.getID()) {
                changeAssertion(assertion.a, replacement, inAst);
                return;
            }
        }
        throw new CheatingIsBadMkay("Couldn't find targeted assertion");
    }

    public static void changeAssertion(String targetAssertionsName, Expr replacement, CompModule inAst) throws CheatingIsBadMkay {
        Field assertsField = getField(CompModule.class, "asserts");
        if (assertsField == null)
            throw new CheatingIsBadMkay("Couldn't find asserts field");
        try {
            boolean oldAccessibleStatus = setAccessibleStatus(assertsField, true);
            Map<String, Expr> asserts = (Map<String, Expr>) assertsField.get(inAst);
            boolean targetFound = false;
            for (Entry<String, Expr> a : asserts.entrySet()) {
                if (a.getKey().compareTo(targetAssertionsName) == 0) {
                    targetFound = true;
                    replacement.setID(a.getValue().getID());
                    break;
                }
            }
            setAccessibleStatus(assertsField, oldAccessibleStatus);
            if (!targetFound)
                throw new CheatingIsBadMkay("Couldn't find targeted assertion");
            asserts.put(targetAssertionsName, replacement);
        } catch (IllegalAccessException e) {
            throw new CheatingIsBadMkay("An error ocurred while trying to access asserts field");
        }
    }

    public static void changeFact(String targetFactName, Expr replacement, CompModule inAst) throws CheatingIsBadMkay {
        //private final List<Pair<String,Expr>>     facts       = new ArrayList<Pair<String,Expr>>();
        Field factsField = getField(CompModule.class, "facts");
        if (factsField == null)
            throw new CheatingIsBadMkay("Couldn't find facts field");
        try {
            boolean oldAccessibleStatus = setAccessibleStatus(factsField, true);
            List<Pair<String, Expr>> facts = (List<Pair<String, Expr>>) factsField.get(inAst);
            List<Pair<String, Expr>> modifiedFacts = new LinkedList<>();
            boolean targetFound = false;
            for (Pair<String, Expr> fact : facts) {
                if (fact.a.compareTo(targetFactName) == 0) {
                    targetFound = true;
                    replacement.setID(fact.b.getID());
                    modifiedFacts.add(new Pair<String, Expr>(targetFactName, replacement));
                } else {
                    modifiedFacts.add(fact);
                }
            }
            if (!targetFound)
                throw new CheatingIsBadMkay("Couldn't find targeted fact");
            factsField.set(inAst, modifiedFacts);
            setAccessibleStatus(factsField, oldAccessibleStatus);
        } catch (IllegalAccessException e) {
            throw new CheatingIsBadMkay("An error ocurred while trying to access facts field");
        }
    }

    public static void addSigToModule(CompModule module, Sig sig) throws CheatingIsBadMkay {
        addSigToModule_sigsField(module, sig);
        addSigToModule_old2fieldsField(module, sig);
        addSigToModule_sig2moduleField(module, sig);
        addSigToModule_old2appendedfactsField(module, sig);
    }

    private static void addSigToModule_sigsField(CompModule module, Sig sig) throws CheatingIsBadMkay {
        Field sigsField = getField(CompModule.class, "sigs");
        if (sigsField == null)
            throw new CheatingIsBadMkay("Couldn't find sigs field");
        try {
            boolean oldAccessibleStatus = setAccessibleStatus(sigsField, true);
            Map<String, Sig> sigs = (Map<String, Sig>) sigsField.get(module);
            if (sigs.containsKey(sig.label)) {
                setAccessibleStatus(sigsField, oldAccessibleStatus);
                throw new IllegalAccessException("Sig to add (" + sig.label + ") already exists in module");
            }
            sigs.put(sig.label, sig);
            setAccessibleStatus(sigsField, oldAccessibleStatus);
        } catch (IllegalAccessException e) {
            throw new CheatingIsBadMkay("An error ocurred while trying to access sigs field");
        }
    }

    private static void addSigToModule_old2fieldsField(CompModule module, Sig sig) throws CheatingIsBadMkay {
        Field old2fieldsField = getField(CompModule.class, "old2fields");
        if (old2fieldsField == null)
            throw new CheatingIsBadMkay("Couldn't find old2fields field");
        try {
            boolean oldAccessibleStatus = setAccessibleStatus(old2fieldsField, true);
            LinkedHashMap<Sig, List<Decl>> old2fields = (LinkedHashMap<Sig, List<Decl>>) old2fieldsField.get(module);
            if (old2fields.containsKey(sig)) {
                setAccessibleStatus(old2fieldsField, oldAccessibleStatus);
                throw new IllegalAccessException("Sig to add (" + sig.label + ") already exists in module");
            }
            old2fields.put(sig, sig.getFieldDecls().makeCopy());
            setAccessibleStatus(old2fieldsField, oldAccessibleStatus);
        } catch (IllegalAccessException e) {
            throw new CheatingIsBadMkay("An error ocurred while trying to access old2fields field");
        }
    }

    private static void addSigToModule_sig2moduleField(CompModule module, Sig sig) throws CheatingIsBadMkay {
        Field sig2moduleField = getField(CompModule.class, "sig2module");
        if (sig2moduleField == null)
            throw new CheatingIsBadMkay("Couldn't find sig2module field");
        try {
            boolean oldAccessibleStatus = setAccessibleStatus(sig2moduleField, true);
            HashMap<Sig, CompModule> sig2module = (HashMap<Sig, CompModule>) sig2moduleField.get(module);
            if (sig2module.containsKey(sig)) {
                setAccessibleStatus(sig2moduleField, oldAccessibleStatus);
                throw new IllegalAccessException("Sig to add (" + sig.label + ") already exists in module");
            }
            sig2module.put(sig, module);
            setAccessibleStatus(sig2moduleField, oldAccessibleStatus);
        } catch (IllegalAccessException e) {
            throw new CheatingIsBadMkay("An error ocurred while trying to access sig2module field");
        }
    }

    private static void addSigToModule_old2appendedfactsField(CompModule module, Sig sig) throws CheatingIsBadMkay {
        Field old2appendedfactsField = getField(CompModule.class, "old2appendedfacts");
        if (old2appendedfactsField == null)
            throw new CheatingIsBadMkay("Couldn't find old2appendedfacts field");
        try {
            boolean oldAccessibleStatus = setAccessibleStatus(old2appendedfactsField, true);
            LinkedHashMap<Sig,Expr> old2appendedfacts = (LinkedHashMap<Sig,Expr>) old2appendedfactsField.get(module);
            if (old2appendedfacts.containsKey(sig)) {
                setAccessibleStatus(old2appendedfactsField, oldAccessibleStatus);
                throw new IllegalAccessException("Sig to add (" + sig.label + ") already exists in module");
            }
            old2appendedfacts.put(sig, ExprConstant.TRUE);
            setAccessibleStatus(old2appendedfactsField, oldAccessibleStatus);
        } catch (IllegalAccessException e) {
            throw new CheatingIsBadMkay("An error ocurred while trying to access old2appendedfacts field");
        }
    }

    public static void removeSigFromModule(CompModule module, Sig sig) throws CheatingIsBadMkay {
        removeSigFromModule_sigsField(module, sig);
        removeSigFromModule_old2fieldsField(module, sig);
        removeSigFromModule_sig2moduleField(module, sig);
        removeSigFromModule_old2appendedfactsField(module, sig);
    }

    private static void removeSigFromModule_sigsField(CompModule module, Sig sig) throws CheatingIsBadMkay {
        Field sigsField = getField(CompModule.class, "sigs");
        if (sigsField == null)
            throw new CheatingIsBadMkay("Couldn't find sigs field");
        try {
            boolean oldAccessibleStatus = setAccessibleStatus(sigsField, true);
            Map<String, Sig> sigs = (Map<String, Sig>) sigsField.get(module);
            if (!sigs.containsKey(sig.label)) {
                setAccessibleStatus(sigsField, oldAccessibleStatus);
                throw new IllegalAccessException("Sig to remove (" + sig.label + ") doesn't exists in module");
            }
            sigs.remove(sig.label);
            setAccessibleStatus(sigsField, oldAccessibleStatus);
        } catch (IllegalAccessException e) {
            throw new CheatingIsBadMkay("An error ocurred while trying to access sigs field");
        }
    }

    private static void removeSigFromModule_old2fieldsField(CompModule module, Sig sig) throws CheatingIsBadMkay {
        Field old2fieldsField = getField(CompModule.class, "old2fields");
        if (old2fieldsField == null)
            throw new CheatingIsBadMkay("Couldn't find old2fields field");
        try {
            boolean oldAccessibleStatus = setAccessibleStatus(old2fieldsField, true);
            LinkedHashMap<Sig,List<Decl>> old2fields = (LinkedHashMap<Sig,List<Decl>>) old2fieldsField.get(module);
            if (!old2fields.containsKey(sig)) {
                setAccessibleStatus(old2fieldsField, oldAccessibleStatus);
                throw new IllegalAccessException("Sig to remove (" + sig.label + ") doesn't exists in module");
            }
            old2fields.remove(sig);
            setAccessibleStatus(old2fieldsField, oldAccessibleStatus);
        } catch (IllegalAccessException e) {
            throw new CheatingIsBadMkay("An error ocurred while trying to access old2fields field");
        }
    }

    private static void removeSigFromModule_sig2moduleField(CompModule module, Sig sig) throws CheatingIsBadMkay {
        Field sigToModuleField = getField(CompModule.class, "sig2module");
        if (sigToModuleField == null)
            throw new CheatingIsBadMkay("Couldn't find sig2module field");
        try {
            boolean oldAccessibleStatus = setAccessibleStatus(sigToModuleField, true);
            HashMap<Sig,CompModule> sigToModule = (HashMap<Sig,CompModule>) sigToModuleField.get(module);
            if (!sigToModule.containsKey(sig)) {
                setAccessibleStatus(sigToModuleField, oldAccessibleStatus);
                throw new IllegalAccessException("Sig to remove (" + sig.label + ") doesn't exists in module");
            }
            sigToModule.remove(sig);
            setAccessibleStatus(sigToModuleField, oldAccessibleStatus);
        } catch (IllegalAccessException e) {
            throw new CheatingIsBadMkay("An error ocurred while trying to access sig2module field");
        }
    }

    private static void removeSigFromModule_old2appendedfactsField(CompModule module, Sig sig) throws CheatingIsBadMkay {
        Field old2appendedfactsField = getField(CompModule.class, "old2appendedfacts");
        if (old2appendedfactsField == null)
            throw new CheatingIsBadMkay("Couldn't find old2appendedfacts field");
        try {
            boolean oldAccessibleStatus = setAccessibleStatus(old2appendedfactsField, true);
            LinkedHashMap<Sig,Expr> old2appendedfacts = (LinkedHashMap<Sig,Expr>) old2appendedfactsField.get(module);
            if (!old2appendedfacts.containsKey(sig)) {
                setAccessibleStatus(old2appendedfactsField, oldAccessibleStatus);
                throw new IllegalAccessException("Sig to remove (" + sig.label + ") doesn't exists in module");
            }
            old2appendedfacts.remove(sig);
            setAccessibleStatus(old2appendedfactsField, oldAccessibleStatus);
        } catch (IllegalAccessException e) {
            throw new CheatingIsBadMkay("An error ocurred while trying to access old2appendedfacts field");
        }
    }

    public static void addFunctionToModule(CompModule module, Func function) throws CheatingIsBadMkay {
        Field funcsField = getField(CompModule.class, "funcs");
        if (funcsField == null)
            throw new CheatingIsBadMkay("Couldn't find funcs field");
        try {
            boolean oldAccessibleStatus = setAccessibleStatus(funcsField, true);
            Map<String, ArrayList<Func>> funcs = (Map<String,ArrayList<Func>>) funcsField.get(module);
            if (funcs.containsKey(function.label) && funcs.get(function.label).stream().anyMatch(f -> f.getID() == function.getID())) {
                setAccessibleStatus(funcsField, oldAccessibleStatus);
                throw new IllegalAccessException("Function to add (" + function.label + ") already exists in module");
            } else {
                ArrayList<Func> functionsList;
                if (funcs.containsKey(function.label))
                    functionsList = funcs.get(function.label);
                else {
                    functionsList = new ArrayList<>();
                    funcs.put(function.label, functionsList);
                }
                functionsList.add(function);
            }
            setAccessibleStatus(funcsField, oldAccessibleStatus);
        } catch (IllegalAccessException e) {
            throw new CheatingIsBadMkay("An error ocurred while trying to access funcs field");
        }
    }

    public static void removeFunctionFromModule(CompModule module, Func function) throws CheatingIsBadMkay {
        Field funcsField = getField(CompModule.class, "funcs");
        if (funcsField == null)
            throw new CheatingIsBadMkay("Couldn't find funcs field");
        try {
            boolean oldAccessibleStatus = setAccessibleStatus(funcsField, true);
            Map<String, ArrayList<Func>> funcs = (Map<String,ArrayList<Func>>) funcsField.get(module);
            if (!funcs.containsKey(function.label) || funcs.get(function.label).stream().noneMatch(f -> f.getID() == function.getID())) {
                setAccessibleStatus(funcsField, oldAccessibleStatus);
                throw new IllegalAccessException("Function to remove (" + function.label + ") doesn't exists in module");
            } else {
                ArrayList<Func> functionsList = funcs.get(function.label);
                int i = 0;
                boolean found = false;
                for (Func f : functionsList) {
                    if (f.getID() == function.getID()) {
                        found = true;
                        break;
                    }
                    i++;
                }
                if (!found)
                    throw new CheatingIsBadMkay("Function to remove should be present but no function with the same ID exists");
                else
                    functionsList.remove(i);
            }
            setAccessibleStatus(funcsField, oldAccessibleStatus);
        } catch (IllegalAccessException e) {
            throw new CheatingIsBadMkay("An error ocurred while trying to access funcs field");
        }
    }

    public static void copySigComponents(Sig original, Sig clone) throws CheatingIsBadMkay {
        Field factsField = getField(Sig.class, "facts");
        if (factsField == null)
            throw new CheatingIsBadMkay("Couldn't find facts field");
        Field fieldsField = getField(Sig.class, "fields");
        if (fieldsField == null)
            throw new CheatingIsBadMkay("Couldn't find fields field");
        Field realFieldsField = getField(Sig.class, "realFields");
        if (realFieldsField == null)
            throw new CheatingIsBadMkay("Couldn't find realFields field");
        copyFieldValue(original, clone, factsField);
        copyFieldValue(original, clone, fieldsField);
        copyFieldValue(original, clone, realFieldsField);
    }

    private static void copyFieldValue(Object original, Object target, Field field) throws CheatingIsBadMkay {
        boolean oldAccessibleStatus = setAccessibleStatus(field, true);
        try {
            field.set(target, field.get(original));
            setAccessibleStatus(field, oldAccessibleStatus);
        } catch (IllegalAccessException e) {
            throw new CheatingIsBadMkay("An error ocurred while trying to access " + field.getName() + " field");
        }
    }

    private static boolean setAccessibleStatus(Field f, boolean newValue) {
        boolean oldValue = f.isAccessible();
        f.setAccessible(newValue);
        return oldValue;
    }

    private static Field getField(Class< ? > from, String field) {
        for (Field f : from.getDeclaredFields()) {
            if (f.getName().compareTo(field) == 0)
                return f;
        }
        return null;
    }

}

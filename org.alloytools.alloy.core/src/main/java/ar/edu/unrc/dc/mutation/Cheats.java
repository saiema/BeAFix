package ar.edu.unrc.dc.mutation;

import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.ast.*;
import edu.mit.csail.sdg.parser.CompModule;

import java.lang.reflect.Field;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
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

    public static void changeBinaryLeftField(ExprBinary original, Expr left) throws CheatingIsBadMkay {
        Field leftField = getField(ExprBinary.class, "left");
        if (leftField == null)
            throw new CheatingIsBadMkay("Couldn't find left field");
        try {
            boolean oldAccessibleStatus = setAccessibleStatus(leftField, true);
            leftField.set(original, left);
            left.setBrowsableParent(original);
            setAccessibleStatus(leftField, oldAccessibleStatus);
        } catch (IllegalAccessException e) {
            throw new CheatingIsBadMkay("There was a problem while trying to change left field", e);
        }
    }

    public static void changeBinaryRightField(ExprBinary original, Expr right) throws CheatingIsBadMkay {
        Field rightField = getField(ExprBinary.class, "right");
        if (rightField == null)
            throw new CheatingIsBadMkay("Couldn't find right field");
        try {
            boolean oldAccessibleStatus = setAccessibleStatus(rightField, true);
            rightField.set(original, right);
            right.setBrowsableParent(original);
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

    public static void changeQtFormulaField(ExprQt original, Expr formula) throws CheatingIsBadMkay {
        Field subField = getField(ExprQt.class, "sub");
        if (subField == null)
            throw new CheatingIsBadMkay("Couldn't find sub field");
        try {
            boolean oldAccessibleStatus = setAccessibleStatus(subField, true);
            subField.set(original, formula);
            formula.setBrowsableParent(original);
            setAccessibleStatus(subField, oldAccessibleStatus);
        } catch (IllegalAccessException e) {
            throw new CheatingIsBadMkay("There was a problem while trying to change sub field", e);
        }
    }

    public static void changeQtBoundFieldFor(ExprQt original, Expr bound, Decl target) throws CheatingIsBadMkay {
        boolean targetFound = false;
        for (Decl d : original.decls) {
            if (d.equals(target)) {
                Field boundField = getField(Decl.class, "expr");
                if (boundField == null)
                    throw new CheatingIsBadMkay("Couldn't find expr field for target decl");
                try {
                    targetFound = true;
                    boolean oldAccessibleStatus = setAccessibleStatus(boundField, true);
                    boundField.set(target, bound);
                    bound.setBrowsableParent(original);
                    setAccessibleStatus(boundField, oldAccessibleStatus);
                } catch (IllegalAccessException e) {
                    throw new CheatingIsBadMkay("There was a problem while trying to change expr field for target decl", e);
                }
            }
        }
        if (!targetFound)
            throw new CheatingIsBadMkay("Couldn't find target decl in quantified expression");
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

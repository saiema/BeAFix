package ar.edu.unrc.dc.mutation;

import java.lang.reflect.Field;

import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.Type;

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
            typeField.setAccessible(true);
            typeField.set(expr, newType);
            typeField.setAccessible(false);
        } catch (SecurityException | IllegalArgumentException | IllegalAccessException e) {
            throw new CheatingIsBadMkay("Your cheats didn't work", e);
        }
    }

    public static Expr cheatedClone(Expr x) throws CheatingIsBadMkay {
        Type originalType = x.type();
        return cheatedClone(x, originalType);
    }

    public static Expr cheatedClone(Expr x, Type t) throws CheatingIsBadMkay {
        Type originalType = t;
        Expr clone = (Expr) x.clone();
        Cheats.changeExpressionType(clone, originalType);
        return clone;
    }

    private static Field getField(Class< ? > from, String field) {
        Field result = null;
        for (Field f : from.getDeclaredFields()) {
            if (f.getName().compareTo(field) == 0)
                return f;
        }
        return result;
    }

}

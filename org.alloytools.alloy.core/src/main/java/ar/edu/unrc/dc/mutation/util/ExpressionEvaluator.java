package ar.edu.unrc.dc.mutation.util;

import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.translator.A4Solution;

import java.util.Optional;

public class ExpressionEvaluator {

    public static Optional<Boolean> evaluateFormula(A4Solution instance, Expr x) {
        Object res = instance.eval(x);
        if (res instanceof Boolean) {
            return Optional.of((Boolean) res);
        }
        return Optional.empty();
    }

}

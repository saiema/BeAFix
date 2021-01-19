package ar.edu.unrc.dc.mutation.util;

import ar.edu.unrc.dc.mutation.mutantLab.testGeneration.VariableExchanger;
import ar.edu.unrc.dc.mutation.mutantLab.testGeneration.VariableMapping;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.translator.A4Solution;

import java.util.Optional;

public class ExpressionEvaluator {

    public static Optional<Boolean> evaluateFormula(A4Solution instance, Expr x, VariableMapping variableMapping) {
        VariableExchanger variableExchanger = new VariableExchanger(variableMapping);
        Expr expressionWithInternalVariables = variableExchanger.replaceVariables((Expr) x.clone());
        Object res = instance.eval(expressionWithInternalVariables);
        if (res instanceof Boolean) {
            return Optional.of((Boolean) res);
        }
        return Optional.empty();
    }

}

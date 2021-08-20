package ar.edu.unrc.dc.mutation.mutantLab.testGeneration;

import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.ExprConstant;
import edu.mit.csail.sdg.translator.A4Solution;
import edu.mit.csail.sdg.translator.A4TupleSet;
import kodkod.instance.Tuple;

import java.util.LinkedList;
import java.util.List;
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

    public static Optional<Boolean> evaluateFormula(A4Solution instance, Expr x) {
        Object res = instance.eval(x);
        if (res instanceof Boolean) {
            return Optional.of((Boolean) res);
        }
        return Optional.empty();
    }

    public static Optional<ExprConstant> evaluateIntFormula(A4Solution instance, Expr x, VariableMapping variableMapping) {
        VariableExchanger variableExchanger = new VariableExchanger(variableMapping);
        Expr expressionWithInternalVariables = variableExchanger.replaceVariables((Expr) x.clone());
        Object res = instance.eval(expressionWithInternalVariables);
        if (res instanceof String) {
            try {
                int value = Integer.parseInt((String) res);
                Expr constant = ExprConstant.makeNUMBER(value);
                if (constant.errors != null && !constant.errors.isEmpty())
                    return Optional.empty();
                return Optional.of((ExprConstant) constant);
            } catch (NumberFormatException e) {
                return Optional.empty();
            }
        }
        return Optional.empty();
    }

    public static Optional<Expr> evaluateComplexFormula(A4Solution instance, Expr x, VariableMapping variableMapping) {
        VariableExchanger variableExchanger = new VariableExchanger(variableMapping);
        Expr expressionWithInternalVariables = variableExchanger.replaceVariables((Expr) x.clone());
        Object res = instance.eval(expressionWithInternalVariables);
        if (res instanceof A4TupleSet) {
            A4TupleSet resAsA4TupleSet = (A4TupleSet) res;
            List<Expr> values = new LinkedList<>();
            for (Tuple tuple : resAsA4TupleSet.debugGetKodkodTupleset()) {
                Expr tupleAsExpression = TestGeneratorHelper.tupleToExpr(tuple, variableMapping.signatureValues(), instance);
                values.add(tupleAsExpression);
            }
            Expr complexResult;
            if (values.isEmpty()) {
                complexResult = ExprConstant.Op.EMPTYNESS.make(null, 0);
            } else {
                complexResult = TestGeneratorHelper.exprListToSet(values);
            }
            return Optional.of(complexResult);
        }
        return Optional.empty();
    }

    public static Optional<? extends Expr> evaluateNonBooleanFormula(A4Solution instance, Expr x, VariableMapping variableMapping) {
        if (isNonBooleanExpression(x)) {
            return isIntExpression(x)?evaluateIntFormula(instance, x, variableMapping):evaluateComplexFormula(instance, x, variableMapping);
        } else {
            throw new IllegalArgumentException("Expression x is a boolean expression, use #evaluateFormula instead");
        }
    }

    public static boolean isNonBooleanExpression(Expr x) {
        return !x.type().is_bool;
    }

    public static boolean isIntExpression(Expr x) {
        return x.type().is_int() || x.type().is_small_int();
    }

}

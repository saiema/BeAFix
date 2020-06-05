package ar.edu.unrc.dc.mutation.mutantLab.testGeneration;

import edu.mit.csail.sdg.ast.*;
import edu.mit.csail.sdg.ast.Sig.Field;

import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

public class ExtractedProperty {

    private final List<ExprVar> variables;
    private final Expr property;

    public static ExtractedProperty createProperty(Expr property, List<ExprVar> variables) {
        return new ExtractedProperty(property, variables);
    }

    public static ExtractedProperty createBinaryProperty(ExtractedProperty a, ExtractedProperty b, ExprBinary.Op op) {
        Expr binaryPropertyExpression = op.make(null, null, a.property, b.property);
        if (binaryPropertyExpression.errors != null && !binaryPropertyExpression.errors.isEmpty())
            throw new IllegalArgumentException(
                    "Bad expression generated (" +
                    a.property.toString() + " " + op.toString() + " " + b.property.toString() +
                    ") : " +
                    binaryPropertyExpression.errors.stream().map(Throwable::toString).collect(Collectors.joining(","))
            );
        List<ExprVar> binaryPropertyVariables = joinVariables(a.variables, b.variables);
        return new ExtractedProperty(binaryPropertyExpression, binaryPropertyVariables);
    }

    public static ExtractedProperty createListProperty(List<ExtractedProperty> props, ExprList.Op op) {
        List<Expr> propertiesExpressions = props.stream().map(ExtractedProperty::getProperty).collect(Collectors.toList());
        Expr listPropertyExpression = ExprList.make(null, null, op, propertiesExpressions);
        if (listPropertyExpression.errors != null && !listPropertyExpression.errors.isEmpty())
            throw new IllegalArgumentException("Bad expression generated (" +
                    propertiesExpressions.stream().map(Expr::toString).collect(Collectors.joining(",")) +
                    ") : " +
                    listPropertyExpression.errors.stream().map(Throwable::toString).collect(Collectors.joining(","))
            );
        List<ExprVar> listPropertyVariables = new LinkedList<>();
        for (ExtractedProperty prop : props) {
            List<ExprVar> qVars = prop.getVariables();
            listPropertyVariables = joinVariables(listPropertyVariables, qVars);
        }
        return new ExtractedProperty(listPropertyExpression, listPropertyVariables);
    }

    public static ExtractedProperty createITEProperty(ExtractedProperty cond, ExtractedProperty left, ExtractedProperty right) {
        Expr ite = ExprITE.make(null, cond.property, left.property, right.property);
        if (ite.errors != null && !ite.errors.isEmpty())
            throw new IllegalArgumentException("Bad expression generated (" +
                    "ITE [ " + cond.property.toString() + " " +
                    "THEN " + left.property.toString() + " " +
                    "ELSE " + right.property.toString() + "]) :" +
                    ite.errors.stream().map(Throwable::toString).collect(Collectors.joining(","))
            );
        List<ExprVar> iteVars = cond.variables;
        iteVars = joinVariables(iteVars, left.variables);
        iteVars = joinVariables(iteVars, right.variables);
        return new ExtractedProperty(ite, iteVars);
    }

    public static ExtractedProperty createLetProperty(ExtractedProperty bound, ExtractedProperty formula, ExprVar declaredVariable) {
            Expr let = ExprLet.make(null, declaredVariable, bound.property, formula.property);
            if (let.errors != null && !let.errors.isEmpty())
                throw new IllegalArgumentException("Bad expression generated (" +
                        "LET [ " + declaredVariable.label + " " +
                        " = " + bound.property.toString() + " " +
                        " | " + formula.property.toString() + "]) : " +
                        let.errors.stream().map(Throwable::toString).collect(Collectors.joining(","))
                );
            List<ExprVar> letVars = new LinkedList<>();
            for (ExprVar v : joinVariables(bound.variables, formula.variables)) {
                if (!v.equals(declaredVariable))
                    letVars.add(v);
            }
            return new ExtractedProperty(let, letVars);
    }

    public static ExtractedProperty createUnaryProperty(ExtractedProperty sub, ExprUnary.Op op) {
        Expr unary = op.make(null, sub.property);
        if (unary.errors != null && !unary.errors.isEmpty())
            throw new IllegalArgumentException("Bad expression generated (" +
                    " " + op.toString() +
                    " " + sub.property.toString() + ") : " +
                    unary.errors.stream().map(Throwable::toString).collect(Collectors.joining(","))
            );
        return new ExtractedProperty(unary, sub.variables);
    }

    public static ExtractedProperty createVarProperty(ExprVar var) {
        List<ExprVar> vars = new LinkedList<>();
        vars.add(var);
        return new ExtractedProperty(var, vars);
    }

    public static ExtractedProperty createSigProperty(Sig sig) {
        return new ExtractedProperty(sig, null);
    }

    public static ExtractedProperty createFieldProperty(Field field) {
        return new ExtractedProperty(field, null);
    }

    public static ExtractedProperty createConstantProperty(Expr constant) {
        return new ExtractedProperty(constant, null);
    }

    public static ExtractedProperty mergeFirstWithSecondVariables(ExtractedProperty a, ExtractedProperty b) {
        return new ExtractedProperty(a.property, joinVariables(a.variables, b.variables));
    }

    private ExtractedProperty(Expr property, List<ExprVar> variables) {
        if (property == null)
            throw new IllegalArgumentException("null property");
        if (variables == null)
            this.variables = new LinkedList<>();
        else
            this.variables = variables;
        this.property = (Expr) property.clone();
    }

    public List<ExprVar> getVariables() {
        return variables;
    }

    public Expr getProperty() {
        return property;
    }

    public boolean declaresVariable(ExprVar var) {
        for (ExprVar qVar : variables) {
            if (qVar.equals(var))
                return true;
        }
        return false;
    }

    @Override
    public String toString() {
        return "Property : " + property.toString() +
                "\n" +
                "Variables : " + variables.stream().map(Expr::toString).collect(Collectors.joining(",")) +
                "\n";
    }

    //PRIVATE FUNCTIONS
    private static List<ExprVar> joinVariables(List<ExprVar> a, List<ExprVar> b) {
        if (b.isEmpty())
            return a;
        if (a.isEmpty())
            return b;
        List<ExprVar> join = new LinkedList<>(a);
        for (ExprVar bVar : b) {
            if (a.stream().noneMatch(aVar -> aVar.equals(bVar)))
                join.add(bVar);
        }
        return join;
    }

}

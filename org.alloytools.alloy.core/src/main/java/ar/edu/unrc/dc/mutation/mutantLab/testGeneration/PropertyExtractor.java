package ar.edu.unrc.dc.mutation.mutantLab.testGeneration;

import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.*;

import java.util.LinkedList;
import java.util.List;

public class PropertyExtractor extends VisitReturn<ExtractedProperty> {

    @Override
    public ExtractedProperty visit(ExprBinary x) throws Err {
        ExtractedProperty left = visitThis(x.left);
        ExtractedProperty right = visitThis(x.right);
        return ExtractedProperty.createBinaryProperty(left, right, x.op);
    }

    @Override
    public ExtractedProperty visit(ExprList x) throws Err {
        List<ExtractedProperty> args = new LinkedList<>();
        for (Expr arg : x.args) {
            args.add(visitThis(arg));
        }
        return ExtractedProperty.createListProperty(args, x.op);
    }

    @Override
    public ExtractedProperty visit(ExprCall x) throws Err {
        List<ExprVar> vars = new LinkedList<>();
        List<Expr> nonVars = new LinkedList<>();
        for (Expr arg : x.args) {
            if (arg instanceof ExprVar)
                vars.add((ExprVar) arg);
            else
                nonVars.add(arg);
        }
        ExtractedProperty callProperty = ExtractedProperty.createProperty(x, vars);
        for (Expr arg : nonVars) {
            ExtractedProperty argProp = visitThis(arg);
            callProperty = ExtractedProperty.mergeFirstWithSecondVariables(callProperty, argProp);
        }
        return callProperty;
    }

    @Override
    public ExtractedProperty visit(ExprConstant x) throws Err {
        return ExtractedProperty.createConstantProperty(x);
    }

    @Override
    public ExtractedProperty visit(ExprITE x) throws Err {
        ExtractedProperty condProperty = visitThis(x.cond);
        ExtractedProperty leftProperty = visitThis(x.left);
        ExtractedProperty rightProperty = visitThis(x.right);
        return ExtractedProperty.createITEProperty(condProperty, leftProperty, rightProperty);
    }

    @Override
    public ExtractedProperty visit(ExprLet x) throws Err {
        ExtractedProperty boundProperty = visitThis(x.expr);
        ExtractedProperty formulaProperty = visitThis(x.sub);
        return ExtractedProperty.createLetProperty(boundProperty, formulaProperty, x.var);
    }

    @Override
    public ExtractedProperty visit(ExprQt x) throws Err {
        return visitThis(x.sub);
    }

    @Override
    public ExtractedProperty visit(ExprUnary x) throws Err {
        ExtractedProperty subProperty = visitThis(x.sub);
        return ExtractedProperty.createUnaryProperty(subProperty, x.op);
    }

    @Override
    public ExtractedProperty visit(ExprVar x) throws Err {
        return ExtractedProperty.createVarProperty(x);
    }

    @Override
    public ExtractedProperty visit(Sig x) throws Err {
        return ExtractedProperty.createSigProperty(x);
    }

    @Override
    public ExtractedProperty visit(Sig.Field x) throws Err {
        return ExtractedProperty.createFieldProperty(x);
    }
}

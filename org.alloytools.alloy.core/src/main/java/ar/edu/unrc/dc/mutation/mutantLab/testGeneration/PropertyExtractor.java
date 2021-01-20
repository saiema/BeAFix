package ar.edu.unrc.dc.mutation.mutantLab.testGeneration;

import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.*;

import java.util.LinkedList;
import java.util.List;

public class PropertyExtractor extends VisitReturn<ExtractedProperty> {

    private boolean validState = false;
    private boolean firstVisit = true;
    private boolean visitingPred = false;
    private boolean visitingAssert = false;
    private boolean extractQt = false;

    public ExtractedProperty extractFromPredicate(Func f) throws Err {
        if (f == null)
            throw new IllegalArgumentException("null argument");
        if (!f.isPred)
            throw new IllegalArgumentException("Func " + f.label + " is not a predicate");
        validState = true;
        firstVisit = true;
        visitingPred = true;
        visitingAssert = false;
        extractQt = false;
        return visitThis(f.getBody());
    }

    public ExtractedProperty extractFromAssertion(Expr a) throws Err {
        if (a == null)
            throw new IllegalArgumentException("null argument");
        validState = true;
        firstVisit = true;
        visitingPred = false;
        visitingAssert = true;
        extractQt = false;
        return visitThis(a);
    }

    public ExtractedProperty extractFromQt(ExprQt qt) throws Err {
        if (qt == null)
            throw new IllegalArgumentException("null argument");
        validState = true;
        firstVisit = true;
        visitingPred = false;
        visitingAssert = false;
        extractQt = true;
        return visitThis(qt);
    }

    @Override
    public ExtractedProperty visitThis(Expr x) throws Err {
        checkState();
        return super.visitThis(x);
    }

    @Override
    public ExtractedProperty visit(ExprBinary x) throws Err {
        firstVisit = false;
        ExtractedProperty left = visitThis(x.left);
        ExtractedProperty right = visitThis(x.right);
        return ExtractedProperty.createBinaryProperty(left, right, x.op);
    }

    @Override
    public ExtractedProperty visit(ExprList x) throws Err {
        firstVisit = false;
        List<ExtractedProperty> args = new LinkedList<>();
        for (Expr arg : x.args) {
            args.add(visitThis(arg));
        }
        return ExtractedProperty.createListProperty(args, x.op);
    }

    @Override
    public ExtractedProperty visit(ExprCall x) throws Err {
        firstVisit = false;
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
        firstVisit = false;
        return ExtractedProperty.createConstantProperty(x);
    }

    @Override
    public ExtractedProperty visit(ExprITE x) throws Err {
        firstVisit = false;
        ExtractedProperty condProperty = visitThis(x.cond);
        ExtractedProperty leftProperty = visitThis(x.left);
        ExtractedProperty rightProperty = visitThis(x.right);
        return ExtractedProperty.createITEProperty(condProperty, leftProperty, rightProperty);
    }

    @Override
    public ExtractedProperty visit(ExprLet x) throws Err {
        firstVisit = false;
        ExtractedProperty boundProperty = visitThis(x.expr);
        ExtractedProperty formulaProperty = visitThis(x.sub);
        return ExtractedProperty.createLetProperty(boundProperty, formulaProperty, x.var);
    }

    @Override
    public ExtractedProperty visit(ExprQt x) throws Err {
        boolean wasFirstVisit = firstVisit;
        firstVisit = false;
        boolean allQt = x.op.equals(ExprQt.Op.ALL);
        boolean someQt = x.op.equals(ExprQt.Op.SOME);
        if (extractQt && wasFirstVisit && (allQt || someQt)) {
            return visitThis(x.sub);
        } else if (visitingPred && wasFirstVisit && someQt)
            return visitThis(x.sub);
        else if (visitingAssert && wasFirstVisit && allQt) {
            return visitThis(x.sub);
        } else {
            ExtractedProperty formulaProperty = visitThis(x.sub);
            return ExtractedProperty.createQTProperty(x, formulaProperty);
        }
    }

    @Override
    public ExtractedProperty visit(ExprUnary x) throws Err {
        if (x.op.equals(ExprUnary.Op.NOOP))
            return visitThis(x.sub);
        firstVisit = false;
        ExtractedProperty subProperty = visitThis(x.sub);
        return ExtractedProperty.createUnaryProperty(subProperty, x.op);
    }

    @Override
    public ExtractedProperty visit(ExprVar x) throws Err {
        firstVisit = false;
        return ExtractedProperty.createVarProperty(x);
    }

    @Override
    public ExtractedProperty visit(Sig x) throws Err {
        firstVisit = false;
        return ExtractedProperty.createSigProperty(x);
    }

    @Override
    public ExtractedProperty visit(Sig.Field x) throws Err {
        firstVisit = false;
        return ExtractedProperty.createFieldProperty(x);
    }

    private void checkState() {
        int visitTypes = 0;
        if (visitingAssert)
            visitTypes++;
        if (visitingPred)
            visitTypes++;
        if (extractQt)
            visitTypes++;
        if (!validState)
            throw new IllegalStateException("visit didn't started from either #extractFromPredicate(Func), #extractFromAssertion(Expr), or #extractFromQt(ExprQt) methods");
        if (visitTypes != 1)
            throw new IllegalStateException("Can only be visiting from one type of expression (predicate: " + visitingPred + "; assert: " + visitingAssert + "; quantifier: " + extractQt + ")");
    }

}

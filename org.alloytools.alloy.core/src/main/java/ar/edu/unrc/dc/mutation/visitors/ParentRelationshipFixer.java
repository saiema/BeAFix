package ar.edu.unrc.dc.mutation.visitors;

import ar.edu.unrc.dc.mutation.CheatingIsBadMkay;
import ar.edu.unrc.dc.mutation.Cheats;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.ast.*;
import edu.mit.csail.sdg.parser.CompModule;

public class ParentRelationshipFixer extends VisitReturn<Void> {

    private Browsable currentParent;
    private Expr varConstantReplacement;

    public ParentRelationshipFixer(Func f) {
        currentParent = f;
        varConstantReplacement = null;
    }

    public ParentRelationshipFixer(Expr a, CompModule context) {
        boolean found = false;
        for (Pair<String, Expr> namedAssertion : context.getAllAssertions()) {
            if (Browsable.equals(namedAssertion.b, a)) {
                found = true;
                break;
            }
        }
        if (!found) {
            for (Pair<String, Expr> namedFact : context.getAllFacts()) {
                if (Browsable.equals(namedFact.b, a)) {
                    found = true;
                    break;
                }
            }
        }
        if (!found)
            throw new IllegalArgumentException("The expression is neither an assertion nor a fact defined by the provided context");
        currentParent = a;
    }

    public void fixParentRelation() {
        if (currentParent instanceof Expr) {
            visitThis((Expr)currentParent);
        } else if (currentParent instanceof Func) {
            Func currentAsFunc = (Func) currentParent;
            visitThis(currentAsFunc.getBody());
            for (Decl arg : currentAsFunc.decls) {
                varConstantReplacement = null;
                visitThis(arg.expr);
                for (Expr argName : arg.names) {
                    varConstantReplacement = null;
                    visitThis(argName);
                }
            }
        } else {
            throw new IllegalStateException("currentParent is neither a Func or an Expr");
        }
    }


    @Override
    public Void visitThis(Expr x) throws Err {
        return super.visitThis(x);
    }

    private boolean isCurrentParent(Expr x) {
        return (currentParent instanceof Expr) && (x.getIDEnv() == currentParent.getIDEnv());
    }

    private void changeParentVisitAndRestore(Expr x, Browsable newParent) {
        Browsable oldParent = currentParent;
        currentParent = newParent;
        x.accept(this);
        currentParent = oldParent;
    }

    @Override
    public Void visit(ExprBinary x) throws Err {
        if (!isCurrentParent(x)) {
            x.setBrowsableParent(currentParent);
        }
        changeParentVisitAndRestore(x.left, isCurrentParent(x)?currentParent:x);
        if (varConstantReplacement != null) {
            try {
                Cheats.changeBinaryLeftField(x, varConstantReplacement);
            } catch (CheatingIsBadMkay e) {
                throw new Error("There was a problem while updating binary expression " + x.toString() + " left expression", e);
            }
            varConstantReplacement = null;
        }
        changeParentVisitAndRestore(x.right, isCurrentParent(x)?currentParent:x);
        if (varConstantReplacement != null) {
            try {
                Cheats.changeBinaryRightField(x, varConstantReplacement);
            } catch (CheatingIsBadMkay e) {
                throw new Error("There was a problem while updating binary expression " + x.toString() + " right expression", e);
            }
            varConstantReplacement = null;
        }
        return null;
    }

    @Override
    public Void visit(ExprList x) throws Err {
        if (!isCurrentParent(x)) {
            x.setBrowsableParent(currentParent);
        }
        for (Expr elem : x.args) {
            changeParentVisitAndRestore(elem, isCurrentParent(x)?currentParent:x);
            if (varConstantReplacement != null) {
                try {
                    Cheats.changeListElement(x, elem, varConstantReplacement);
                } catch (CheatingIsBadMkay e) {
                    throw new Error("There was a problem while updating list expression " + x.toString() + " element", e);
                }
                varConstantReplacement = null;
            }
        }
        return null;
    }

    @Override
    public Void visit(ExprCall x) throws Err {
        if (!isCurrentParent(x)) {
            x.setBrowsableParent(currentParent);
        }
        for (Expr arg : x.args) {
            changeParentVisitAndRestore(arg, isCurrentParent(x)?currentParent:x);
            if (varConstantReplacement != null) {
                try {
                    Cheats.changeCallArgument(x, arg, varConstantReplacement);
                } catch (CheatingIsBadMkay e) {
                    throw new Error("There was a problem while updating function call " + x.toString() + " argument", e);
                }
                varConstantReplacement = null;
            }
        }
        return null;
    }

    @Override
    public Void visit(ExprConstant x) throws Err {
        try {
            if (!isCurrentParent(x)) {
                ExprConstant constCopy = (ExprConstant) Cheats.cheatedClone(x);
                constCopy.newID();
                constCopy.newIDEnv();
                varConstantReplacement = constCopy;
                constCopy.setBrowsableParent(currentParent);
            }
        } catch (CheatingIsBadMkay e) {
            throw new Error("There was a problem while cloning variable " + x.toString(), e);
        }
        return null;
    }

    @Override
    public Void visit(ExprITE x) throws Err {
        if (!isCurrentParent(x)) {
            x.setBrowsableParent(currentParent);
        }
        changeParentVisitAndRestore(x.cond, isCurrentParent(x)?currentParent:x);
        if (varConstantReplacement != null) {
            try {
                Cheats.changeITECondition(x, varConstantReplacement);
            } catch (CheatingIsBadMkay e) {
                throw new Error("There was a problem while updating ITE condition " + x.toString(), e);
            }
            varConstantReplacement = null;
        }
        changeParentVisitAndRestore(x.left, isCurrentParent(x)?currentParent:x);
        if (varConstantReplacement != null) {
            try {
                Cheats.changeITEThen(x, varConstantReplacement);
            } catch (CheatingIsBadMkay e) {
                throw new Error("There was a problem while updating ITE then " + x.toString() + " expression", e);
            }
            varConstantReplacement = null;
        }
        changeParentVisitAndRestore(x.right, isCurrentParent(x)?currentParent:x);
        if (varConstantReplacement != null) {
            try {
                Cheats.changeITETElse(x, varConstantReplacement);
            } catch (CheatingIsBadMkay e) {
                throw new Error("There was a problem while updating ITE else " + x.toString() + " expression", e);
            }
            varConstantReplacement = null;
        }
        return null;
    }

    @Override
    public Void visit(ExprLet x) throws Err {
        if (!isCurrentParent(x)) {
            x.setBrowsableParent(currentParent);
        }
        changeParentVisitAndRestore(x.var, isCurrentParent(x)?currentParent:x);
        if (varConstantReplacement != null) {
            varConstantReplacement = null;
        }
        changeParentVisitAndRestore(x.expr, isCurrentParent(x)?currentParent:x);
        if (varConstantReplacement != null) {
            try {
                Cheats.changeLetExpr(x, varConstantReplacement);
            } catch (CheatingIsBadMkay e) {
                throw new Error("There was a problem while updating LET var bound " + x.toString() + " expression", e);
            }
            varConstantReplacement = null;
        }
        changeParentVisitAndRestore(x.sub, isCurrentParent(x)?currentParent:x);
        if (varConstantReplacement != null) {
            try {
                Cheats.changeLetSub(x, varConstantReplacement);
            } catch (CheatingIsBadMkay e) {
                throw new Error("There was a problem while updating LET sub " + x.toString() + " expression", e);
            }
            varConstantReplacement = null;
        }
        return null;
    }

    @Override
    public Void visit(ExprQt x) throws Err {
        if (!isCurrentParent(x)) {
            x.setBrowsableParent(currentParent);
        }
        for (Decl d : x.decls) {
            changeParentVisitAndRestore(d.expr, isCurrentParent(x)?currentParent:x);
            if (varConstantReplacement != null) {
                try {
                    Cheats.changeQtBoundFieldFor(x, varConstantReplacement, d);
                } catch (CheatingIsBadMkay e) {
                    throw new Error("There was a problem while updating QT bound " + x.toString() + " expression", e);
                }
                varConstantReplacement = null;
            }
            for (Expr dName : d.names) {
                changeParentVisitAndRestore(dName, isCurrentParent(x) ? currentParent : x);
                if (varConstantReplacement != null) {
                    varConstantReplacement = null;
                }
            }
        }
        changeParentVisitAndRestore(x.sub, isCurrentParent(x)?currentParent:x);
        if (varConstantReplacement != null) {
            try {
                Cheats.changeQtFormulaField(x, varConstantReplacement);
            } catch (CheatingIsBadMkay e) {
                throw new Error("There was a problem while updating QT formula " + x.toString() + " expression", e);
            }
            varConstantReplacement = null;
        }
        return null;
    }

    @Override
    public Void visit(ExprUnary x) throws Err {
        if (!isCurrentParent(x)) {
            x.setBrowsableParent(currentParent);
        }
        changeParentVisitAndRestore(x.sub, isCurrentParent(x)?currentParent:x);
        if (varConstantReplacement != null) {
            try {
                Cheats.changeUnarySubField(x, varConstantReplacement);
            } catch (CheatingIsBadMkay e) {
                throw new Error("There was a problem while updating Unary sub " + x.toString() + " expression", e);
            }
            varConstantReplacement = null;
        }
        return null;
    }

    @Override
    public Void visit(ExprVar x) throws Err {
        try {
            if (!isCurrentParent(x)) {
                ExprVar varCopy = (ExprVar) Cheats.cheatedClone(x);
                varCopy.newID();
                varCopy.newIDEnv();
                varConstantReplacement = varCopy;
                varCopy.setBrowsableParent(currentParent);
            }
        } catch (CheatingIsBadMkay e) {
            throw new Error("There was a problem while cloning variable " + x.toString(), e);
        }
        return null;
    }


    @Override
    public Void visit(Sig x) throws Err {
        if (!isCurrentParent(x)) {
            x.setBrowsableParent(currentParent);
        }
        return null;
    }

    @Override
    public Void visit(Sig.Field x) throws Err {
        if (!isCurrentParent(x)) {
            x.setBrowsableParent(currentParent);
        }
        return null;
    }
}

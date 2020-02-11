package ar.edu.unrc.dc.mutation.visitors;

import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.ast.*;
import edu.mit.csail.sdg.parser.CompModule;

public class ParentRelationshipFixer extends VisitReturn<Void> {

    private Browsable currentParent;

    public ParentRelationshipFixer(Func f) {
        currentParent = f;
    }

    public ParentRelationshipFixer(Expr a, CompModule context) {
        boolean found = false;
        for (Pair<String, Expr> namedAssertion : context.getAllAssertions()) {
            if (namedAssertion.b.equals(a)) {
                found = true;
                break;
            }
        }
        if (!found)
            throw new IllegalArgumentException("The expression is not an assertion defined by the provided context");
        currentParent = a;
    }

    public void fixParentRelation() {
        if (currentParent instanceof Expr) {
            visitThis((Expr)currentParent);
        } else if (currentParent instanceof Func) {
            Func currentAsFunc = (Func) currentParent;
            visitThis(currentAsFunc.getBody());
            for (Decl arg : currentAsFunc.decls) {
                visitThis(arg.expr);
                for (Expr argName : arg.names) {
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
        changeParentVisitAndRestore(x.right, isCurrentParent(x)?currentParent:x);
        return null;
    }

    @Override
    public Void visit(ExprList x) throws Err {
        if (!isCurrentParent(x)) {
            x.setBrowsableParent(currentParent);
        }
        for (Expr elem : x.args) {
            changeParentVisitAndRestore(elem, isCurrentParent(x)?currentParent:x);
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
        }
        return null;
    }

    @Override
    public Void visit(ExprConstant x) throws Err {
        if (!isCurrentParent(x)) {
            x.setBrowsableParent(currentParent);
        }
        return null;
    }

    @Override
    public Void visit(ExprITE x) throws Err {
        if (!isCurrentParent(x)) {
            x.setBrowsableParent(currentParent);
        }
        changeParentVisitAndRestore(x.cond, isCurrentParent(x)?currentParent:x);
        changeParentVisitAndRestore(x.left, isCurrentParent(x)?currentParent:x);
        changeParentVisitAndRestore(x.right, isCurrentParent(x)?currentParent:x);
        return null;
    }

    @Override
    public Void visit(ExprLet x) throws Err {
        if (!isCurrentParent(x)) {
            x.setBrowsableParent(currentParent);
        }
        changeParentVisitAndRestore(x.var, isCurrentParent(x)?currentParent:x);
        changeParentVisitAndRestore(x.expr, isCurrentParent(x)?currentParent:x);
        changeParentVisitAndRestore(x.sub, isCurrentParent(x)?currentParent:x);
        return null;
    }

    @Override
    public Void visit(ExprQt x) throws Err {
        if (!isCurrentParent(x)) {
            x.setBrowsableParent(currentParent);
        }
        for (Decl d : x.decls) {
            changeParentVisitAndRestore(d.expr, isCurrentParent(x)?currentParent:x);
            for (Expr dName : d.names)
                changeParentVisitAndRestore(dName, isCurrentParent(x)?currentParent:x);
        }
        changeParentVisitAndRestore(x.sub, isCurrentParent(x)?currentParent:x);
        return null;
    }

    @Override
    public Void visit(ExprUnary x) throws Err {
        if (!isCurrentParent(x)) {
            x.setBrowsableParent(currentParent);
        }
        changeParentVisitAndRestore(x.sub, isCurrentParent(x)?currentParent:x);
        return null;
    }

    @Override
    public Void visit(ExprVar x) throws Err {
        if (!isCurrentParent(x)) {
            x.setBrowsableParent(currentParent);
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

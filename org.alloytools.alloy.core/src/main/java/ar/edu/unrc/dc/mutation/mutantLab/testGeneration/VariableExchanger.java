package ar.edu.unrc.dc.mutation.mutantLab.testGeneration;

import ar.edu.unrc.dc.mutation.CheatingIsBadMkay;
import ar.edu.unrc.dc.mutation.Cheats;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.*;

public class VariableExchanger extends VisitReturn<Void> {

    private final VariableMapping variableMapping;

    public VariableExchanger(VariableMapping variableMapping) {
        this.variableMapping = variableMapping;
    }

    public Expr replaceVariables(Expr x) {
        visitThis(x);
        return x;
    }

    @Override
    public Void visit(ExprBinary x) throws Err {
        visitThis(x.left);
        visitThis(x.right);
        return null;
    }

    @Override
    public Void visit(ExprList x) throws Err {
        for (Expr arg : x.args) {
            visitThis(arg);
        }
        return null;
    }

    @Override
    public Void visit(ExprCall x) throws Err {
        for (Expr arg :  x.args) {
            visitThis(arg);
        }
        return null;
    }

    @Override
    public Void visit(ExprConstant x) throws Err {
        return null;
    }

    @Override
    public Void visit(ExprITE x) throws Err {
        visitThis(x.cond);
        visitThis(x.left);
        visitThis(x.right);
        return null;
    }

    @Override
    public Void visit(ExprLet x) throws Err {
        visitThis(x.expr);
        visitThis(x.sub);
        return null;
    }

    @Override
    public Void visit(ExprQt x) throws Err {
        for (Decl d : x.decls) {
            visitThis(d.expr);
        }
        visitThis(x.sub);
        return null;
    }

    @Override
    public Void visit(ExprUnary x) throws Err {
        visitThis(x.sub);
        return null;
    }

    @Override
    public Void visit(ExprVar x) throws Err {
        if (variableMapping.availableSkolem(x)) {
            ExprVar skolemVar = variableMapping.skolemVar(x);
            try {
                Cheats.impersonateVariable(skolemVar, x);
            } catch (CheatingIsBadMkay e) {
                throw new Error("An error occurred while trying to change variable " + x.toString() + " for skolem variable " + skolemVar, e);
            }

        }
        return null;
    }

    @Override
    public Void visit(Sig x) throws Err {
        return null;
    }

    @Override
    public Void visit(Sig.Field x) throws Err {
        return null;
    }
}

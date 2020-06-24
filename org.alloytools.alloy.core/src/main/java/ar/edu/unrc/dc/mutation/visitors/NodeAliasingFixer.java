package ar.edu.unrc.dc.mutation.visitors;

import ar.edu.unrc.dc.mutation.CheatingIsBadMkay;
import ar.edu.unrc.dc.mutation.Cheats;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.ast.*;
import edu.mit.csail.sdg.parser.CompModule;

public class NodeAliasingFixer extends VisitReturn<Void> {

    private Expr replacement = null;

    public void fixSigNodes(CompModule ast) {
        for (Sig s : ast.getAllSigs()) {
            s.getFacts().forEach(f -> f.accept(this));
        }
        for (Pair<String, Expr> fact : ast.getAllFacts()) {
            fact.b.accept(this);
        }
        for (Pair<String, Expr> assertion : ast.getAllAssertions()) {
            assertion.b.accept(this);
        }
        for (Func f : ast.getAllFunc()) {
            f.getBody().accept(this);
        }
        for (Command cmd : ast.getAllCommands()) {
            cmd.formula.accept(this);
        }
    }

    @Override
    public Void visit(ExprBinary x) throws Err {
        x.left.accept(this);
        if (replacement != null) {
            try {
                Cheats.changeBinaryLeftField(x, replacement);
                replacement = null;
            } catch (CheatingIsBadMkay e) {
                throw new Error("Error while replacing left expression " + x.left.toString() + " of " + x.toString(), e);
            }
        }
        x.right.accept(this);
        if (replacement != null) {
            try {
                Cheats.changeBinaryRightField(x, replacement);
                replacement = null;
            } catch (CheatingIsBadMkay e) {
                throw new Error("Error while replacing right expression " + x.right.toString() + " of " + x.toString(), e);
            }
        }
        return null;
    }

    @Override
    public Void visit(ExprList x) throws Err {
        for (Expr arg : x.args) {
            arg.accept(this);
            if (replacement != null) {
                try {
                    Cheats.changeListElement(x, arg, replacement);
                    replacement = null;
                } catch (CheatingIsBadMkay e) {
                    throw new Error("Error while replacing argument " + arg.toString() + " of " + x.toString(), e);
                }
            }
        }
        return null;
    }

    @Override
    public Void visit(ExprCall x) throws Err {
        for (Expr arg : x.args) {
            arg.accept(this);
            if (replacement != null) {
                try {
                    Cheats.changeCallArgument(x, arg, replacement);
                    replacement = null;
                } catch (CheatingIsBadMkay e) {
                    throw new Error("Error while replacing call argument " + arg.toString() + " of " + x.toString(), e);
                }
            }
        }
        return null;
    }

    @Override
    public Void visit(ExprConstant x) throws Err {
        return null;
    }

    @Override
    public Void visit(ExprITE x) throws Err {
        x.cond.accept(this);
        if (replacement != null) {
            try {
                Cheats.changeITECondition(x, replacement);
                replacement = null;
            } catch (CheatingIsBadMkay e) {
                throw new Error("Error while replacing condition " + x.cond.toString() + " of " + x.toString(), e);
            }
        }
        x.left.accept(this);
        if (replacement != null) {
            try {
                Cheats.changeITEThen(x, replacement);
                replacement = null;
            } catch (CheatingIsBadMkay e) {
                throw new Error("Error while replacing then expression " + x.left.toString() + " of " + x.toString(), e);
            }
        }
        x.right.accept(this);
        if (replacement != null) {
            try {
                Cheats.changeITETElse(x, replacement);
                replacement = null;
            } catch (CheatingIsBadMkay e) {
                throw new Error("Error while replacing else expression " + x.right.toString() + " of " + x.toString(), e);
            }
        }
        return null;
    }

    @Override
    public Void visit(ExprLet x) throws Err {
        x.expr.accept(this);
        if (replacement != null) {
            try {
                Cheats.changeLetExpr(x, replacement);
                replacement = null;
            } catch (CheatingIsBadMkay e) {
                throw new Error("Error while replacing bound expression " + x.expr.toString() + " of " + x.toString(), e);
            }
        }
        x.sub.accept(this);
        if (replacement != null) {
            try {
                Cheats.changeLetSub(x, replacement);
                replacement = null;
            } catch (CheatingIsBadMkay e) {
                throw new Error("Error while replacing formula " + x.sub.toString() + " of " + x.toString(), e);
            }
        }
        return null;
    }

    @Override
    public Void visit(ExprQt x) throws Err {
        for (Decl d : x.decls) {
            for (ExprHasName var : d.names) {
                var.accept(this);
                if (replacement != null && replacement instanceof ExprVar) {
                    try {
                        Cheats.changeQtVar(x, var, (ExprVar) replacement);
                        replacement = null;
                    } catch (CheatingIsBadMkay e) {
                        throw new Error("Error while replacing variable " + var.toString() + " of " + d.toString(), e);
                    }
                } else if (replacement != null)
                    throw new Error("Something went wrong, replacement should be a variable but is not");
            }
            d.expr.accept(this);
            if (replacement != null) {
                try {
                    Cheats.changeQtBoundFieldFor(x, replacement, d);
                    replacement = null;
                } catch (CheatingIsBadMkay e) {
                    throw new Error("Error while replacing variable bound expression " + d.toString() + " of " + x.toString(), e);
                }
            }
        }
        x.sub.accept(this);
        if (replacement != null) {
            try {
                Cheats.changeQtFormulaField(x, replacement);
                replacement = null;
            } catch (CheatingIsBadMkay e) {
                throw new Error("Error while replacing formula " + x.sub.toString() + " of " + x.toString(), e);
            }
        }
        return null;
    }

    @Override
    public Void visit(ExprUnary x) throws Err {
        x.sub.accept(this);
        if (replacement != null) {
            try {
                Cheats.changeUnarySubField(x, replacement);
                replacement = null;
            } catch (CheatingIsBadMkay e) {
                throw new Error("Error while replacing sub expression of " + x.toString(), e);
            }
        }
        return null;
    }

    @Override
    public Void visit(ExprVar x) throws Err {
        try {
            ExprVar newVar = (ExprVar) Cheats.cheatedClone(x);
            newVar.newID();
            newVar.newIDEnv();
            replacement = newVar;
        } catch (CheatingIsBadMkay e) {
            throw new Error("Error while cloning variable " + x.toString(), e);
        }
        return null;
    }

    @Override
    public Void visit(Sig x) throws Err {
        Sig cloneSig;
        try {
            cloneSig = (Sig) Cheats.cheatedClone(x);
            cloneSig.newIDEnv();
            cloneSig.newID();
            replacement = cloneSig;
        } catch (CheatingIsBadMkay e) {
            throw new Error("Error while cloning signature " + x.toString(), e);
        }
        return null;
    }

    @Override
    public Void visit(Sig.Field x) throws Err {
        Sig.Field cloneField;
        try {
            cloneField = (Sig.Field) Cheats.cheatedClone(x);
            cloneField.newID();
            cloneField.newIDEnv();
            replacement = cloneField;
        } catch (CheatingIsBadMkay e) {
            throw new Error("Error while cloning field " + x.toString(), e);
        }
        return null;
    }

}

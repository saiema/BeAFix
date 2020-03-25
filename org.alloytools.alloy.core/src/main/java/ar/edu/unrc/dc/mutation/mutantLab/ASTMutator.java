package ar.edu.unrc.dc.mutation.mutantLab;

import ar.edu.unrc.dc.mutation.CheatingIsBadMkay;
import ar.edu.unrc.dc.mutation.Cheats;
import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.Ops;
import ar.edu.unrc.dc.mutation.visitors.SearchAndReplace;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.ast.*;
import edu.mit.csail.sdg.parser.CompModule;

import java.util.List;
import java.util.Optional;
import java.util.Stack;

public class ASTMutator {

    private static ASTMutator instance;
    public synchronized static void startInstance(CompModule original) {
        if (instance != null)
            throw new IllegalStateException("Already instantiated");
        instance = new ASTMutator(original);
    }
    public synchronized static void destroyInstance() {
        instance = null;
    }
    public synchronized static ASTMutator getInstance() {
        if (instance == null)
            throw new IllegalStateException("You need to execute startInstance first");
        return instance;
    }

    private Stack<Mutation> unappliedMutations;
    private Stack<Mutation> appliedMutations;
    private CompModule ast;

    private ASTMutator(CompModule original) {
        if (original == null)
            throw new IllegalArgumentException("original ast can't be null");
        ast = original;
        appliedMutations = new Stack<>();
        unappliedMutations = new Stack<>();
    }

    public void pushNewMutation(Mutation m) {
        unappliedMutations.push(m);
    }

    public boolean applyMutations() {
        if (!appliedMutations.empty())
            throw new IllegalStateException("some mutations are already applied");
        while (!unappliedMutations.empty()) {
            if (!applyMutation(unappliedMutations.pop(), false))
                return false;
        }
        return true;
    }

    private boolean applyMutation(Mutation m, boolean undo) {
        Expr original = undo ? m.mutant() : m.original();
        Expr mutant = undo ? m.original() : m.mutant();
        Expr mayorExpr = undo ? original : m.getMayorAffectedExpression();
        SearchAndReplace replacer = new SearchAndReplace(original, mutant);
        Optional<Expr> mutatedExpr = replacer.visitThis(mayorExpr);
        if (mutatedExpr.isPresent()) {
            try {
                Optional<Expr> processedMutatedExpr = replace(mayorExpr, mutatedExpr);
                if (!undo && processedMutatedExpr.isPresent()) {
                    Mutation fullExprMutation = new Mutation(Ops.MULTI, mayorExpr, processedMutatedExpr.get());
                    appliedMutations.push(fullExprMutation);
                }
                return processedMutatedExpr.isPresent();
            } catch (CheatingIsBadMkay cheatingIsBadMkay) {
                cheatingIsBadMkay.printStackTrace();
                return false;
            }
        }
        return false;
    }

    private Optional<Expr> replace(Expr original, Optional<Expr> replacement) throws CheatingIsBadMkay {
        if (replacement.isPresent()) {
            Expr newExpression = replacement.get();
            Browsable initialExpressionParent = original.getBrowsableParent();
            if (initialExpressionParent == null) {
                return Optional.empty();
            }
            if (initialExpressionParent instanceof ExprBinary) {
                ExprBinary oParentAsBinary = (ExprBinary) initialExpressionParent;
                if (oParentAsBinary.left.getID() == original.getID()) {
                    Cheats.changeBinaryLeftField(oParentAsBinary, newExpression);
                } else if (oParentAsBinary.right.getID() == original.getID()) {
                    Cheats.changeBinaryRightField(oParentAsBinary, newExpression);
                } else {
                    return Optional.empty();
                }
            } else if (initialExpressionParent instanceof ExprUnary) {
                ExprUnary oParentAsUnary = (ExprUnary) initialExpressionParent;
                //replacement should be the subexpression
                if (oParentAsUnary.sub.getID() == original.getID()) {
                    Cheats.changeUnarySubField(oParentAsUnary, newExpression);
                } else {
                    return Optional.empty();
                }
            } else if (initialExpressionParent instanceof ExprQt) {
                ExprQt oParentAsQt = (ExprQt) initialExpressionParent;
                //check if replacement is in a bound expression
                boolean targetFound = false;
                for (Decl d : oParentAsQt.decls) {
                    if (d.expr.getID() == original.getID()) {
                        Cheats.changeQtBoundFieldFor(oParentAsQt, newExpression, d);
                        targetFound = true;
                        break;
                    }
                }
                //check if replacement is in formula
                if (oParentAsQt.sub.getID() == original.getID()) {
                    Cheats.changeQtFormulaField(oParentAsQt, newExpression);
                    targetFound = true;
                }
                if (!targetFound) {
                    return Optional.empty();
                }
            } else if (initialExpressionParent instanceof ExprList) {
                ExprList oParentAsList = (ExprList) initialExpressionParent;
                //replacement should be an arg, not the operator
                boolean targetFound = false;
                for (Expr arg : oParentAsList.args) {
                    if (arg.getID() == original.getID()) {
                        Cheats.changeListElement(oParentAsList, arg, newExpression);
                        targetFound = true;
                        break;
                    }
                }
                if (!targetFound) {
                    return Optional.empty();
                }
            } else if (initialExpressionParent instanceof Func) {
                Func oParentAsFunc = (Func) initialExpressionParent;
                //replacement should be the function's body
                if (oParentAsFunc.getBody().getID() == original.getID())
                    Cheats.changeFuncBody(oParentAsFunc, newExpression);
                else {
                    return Optional.empty();
                }
            } else if (initialExpressionParent instanceof ExprLet) {
                ExprLet oParentAsLet = (ExprLet) initialExpressionParent;
                if (oParentAsLet.expr.getID() == original.getID()) {
                    Cheats.changeLetExpr(oParentAsLet, newExpression);
                } else if (oParentAsLet.sub.getID() == original.getID()) {
                    Cheats.changeLetSub(oParentAsLet, newExpression);
                } else
                    return Optional.empty();
            } else if (initialExpressionParent instanceof Expr){
                //replacement should be an assertion's body
                boolean targetFound = false;
                for (Pair<String, Expr> assertion : ast.getAllAssertions()) {
                    if (assertion.b.getID() == original.getID()) {
                        Cheats.changeAssertion(assertion.a, newExpression, ast);
                        targetFound = true;
                        break;
                    }
                }
                if (!targetFound) {
                    return Optional.empty();
                }
            }
        }
        return replacement;
    }

    public List<Mutation> appliedMutations() {
        return appliedMutations;
    }

    public boolean undoMutations() {
        if (!unappliedMutations.empty())
            throw new IllegalStateException("not all mutations where applied");
        while (!appliedMutations.empty()) {
            if (!applyMutation(appliedMutations.pop(), true))
                return false;
        }
        return true;
    }

}

package ar.edu.unrc.dc.mutation.mutantLab;

import ar.edu.unrc.dc.mutation.CheatingIsBadMkay;
import ar.edu.unrc.dc.mutation.Cheats;
import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.Ops;
import ar.edu.unrc.dc.mutation.util.TypeChecking;
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

    private final Stack<Mutation> unappliedMutations;
    private final Stack<Mutation> appliedMutations;
    private final CompModule ast;

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

    //PRIVATE METHODS

    private boolean applyMutation(Mutation m, boolean undo) {
        Expr original = undo ? m.mutant() : m.original();
        Expr mutant = undo ? m.original() : m.mutant();
        Expr mayorExpr = undo ? original : m.getMayorAffectedExpression(ast);//m.getMayorAffectedExpression();
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
            } catch (CheatingIsBadMkay e) {
                throw new IllegalStateException("An exception occurred while applying mutation", e);
            }
        }
        return false;
    }

    private Optional<Expr> replace(Expr original, Optional<Expr> replacement) throws CheatingIsBadMkay {
        if (replacement.isPresent()) {
            Expr newExpression = replacement.get();
            Browsable initialExpressionParent = original.getBrowsableParent();
            if (initialExpressionParent == null || TypeChecking.isAssertionFactOrFunctionBody(original, ast)) {
                //first we should check if original is a fact
                for (Pair<String, Expr> fact : ast.getAllFacts()) {
                    if (fact.b.getID() == original.getID()) {
                        Cheats.changeFact(fact.a, newExpression, ast);
                        return replacement;
                    }
                }
                //then whe should check if original is in a predicate or function
                for (Func func : ast.getAllFunc()) {
                    if (func.getBody().getID() == original.getID()) {
                        Cheats.changeFuncBody(func, newExpression);
                        return replacement;
                    }
                }
                //finally we should check if original is in an assertion
                for (Pair<String, Expr> assertion : ast.getAllAssertions()) {
                    if (assertion.b.getID() == original.getID()) {
                        Cheats.changeAssertion(assertion.a, newExpression, ast);
                        return replacement;
                    }
                }
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
            } else if (initialExpressionParent instanceof ExprCall) {
                //replacement should be an argument of the call
                ExprCall oParentAsCall = (ExprCall) initialExpressionParent;
                boolean targetFound = false;
                for (Expr arg : oParentAsCall.args) {
                    if (arg.getID() == original.getID()) {
                        Cheats.changeCallArgument(oParentAsCall, arg, newExpression);
                        targetFound = true;
                        break;
                    }
                }
                if (!targetFound) {
                    return Optional.empty();
                }
            } else if (initialExpressionParent instanceof Expr){
                //replacement should be either an assertion's o fact's body
                boolean assertionFound = false;
                boolean factFound = false;
                for (Pair<String, Expr> assertion : ast.getAllAssertions()) {
                    if (assertion.b.getID() == original.getID()) {
                        Cheats.changeAssertion(assertion.a, newExpression, ast);
                        assertionFound = true;
                        break;
                    }
                }
                if (!assertionFound) {
                    for (Pair<String, Expr> fact : ast.getAllFacts()) {
                        if (fact.b.getID() == original.getID()) {
                            Cheats.changeFact(fact.a, newExpression, ast);
                            factFound = true;
                            break;
                        }
                    }
                }
                if (!assertionFound && !factFound) {
                    return Optional.empty();
                }
            }
        }
        return replacement;
    }

}

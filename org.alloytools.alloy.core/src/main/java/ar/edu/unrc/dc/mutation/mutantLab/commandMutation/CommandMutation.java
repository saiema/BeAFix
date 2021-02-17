package ar.edu.unrc.dc.mutation.mutantLab.commandMutation;

import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.ast.*;

import java.util.*;
import java.util.stream.Collectors;

/**
 * This class transform a check command that didn't generate any counterexamples to a run command that should generate some instances, using the following transformations:
 * <ul>
 *     <li>'all x | P[x]' will be transformed to 'some x | P[x]' and 'some x | not P[x]'</li>
 *     <li>'P' will be transformed to 'P' and 'not P'</li>
 *     <li>'no x | P[x]' will be transformed as if 'all x | not P[x]'</li>
 *     <li>'some x | P[x]' will be transformed to 'some x | not P[x]'</li>
 *     <li>On complex expressions, 'all' expressions will be transformed to 'some' and 'no' expressions will be first transformed to 'all' and then to 'some'</li>
 * </ul>
 * Tests generated from the original expression will be used in 'run ... expect 1' commands and
 * tests generated from the negation of the original expression will be used in 'run ... expect 0' commands.
 */
public class CommandMutation {

    public static Map<Command, Boolean> generateCommandMutations(Command from) throws CommandMutationException {
        if (from == null)
            throw new IllegalArgumentException("from command is null");
        if (!from.check)
            throw new IllegalArgumentException("from command is not a check command (not an assertion)");
        Map<Command, Boolean> commands = new HashMap<>();
        Expr facts = from.hasFacts()? from.getFacts():null;
        Expr formula = getOriginalAssertionFormula(from.hasFacts()?from.getFormulaWithoutFacts():from.formula);
        for (Pair<Expr, Boolean> mutation : generateMutations(formula)) {
            Expr newCommandFormula = facts == null?mutation.a:ExprBinary.Op.AND.make(null, null, (Expr) facts.clone(), mutation.a);
            Command newCommand = new Command(false, from.overall, from.bitwidth, from.maxseq, 1, newCommandFormula);
            commands.put(newCommand, mutation.b);
        }
        return commands;
    }

    private static Expr getOriginalAssertionFormula(Expr expr) {
        if (expr instanceof ExprUnary) {
            ExprUnary exprAsUnary = (ExprUnary) expr;
            if (exprAsUnary.op.equals(ExprUnary.Op.NOOP))
                return getOriginalAssertionFormula(exprAsUnary.sub);
            if (exprAsUnary.op.equals(ExprUnary.Op.NOT))
                return exprAsUnary.sub;
        }
        return expr;
    }

    private static List<Pair<Expr, Boolean>> generateMutations(Expr from) throws CommandMutationException {
        if (from instanceof ExprUnary && ((ExprUnary)from).op.equals(ExprUnary.Op.NOOP))
            return generateMutations(((ExprUnary)from).sub);
        if (from instanceof ExprQt) {
            ExprQt fromAsQt = (ExprQt) from;
            switch (fromAsQt.op) {
                case ALL: {
                    List<Decl> declsClonePositive = cloneDecls(fromAsQt.decls);
                    List<Decl> declsCloneNegative = cloneDecls(fromAsQt.decls);
                    Expr positive = ExprQt.Op.SOME.make(null, null, declsClonePositive, (Expr) fromAsQt.sub.clone());
                    checkExpr(positive);
                    Expr negative = ExprQt.Op.SOME.make(null, null, declsCloneNegative, (Expr) fromAsQt.sub.clone());//negateExpression(fromAsQt.sub));
                    checkExpr(negative);
                    return Arrays.asList(
                            new Pair<>(positive, true),
                            new Pair<>(negative, false)
                    );
                }
                case NO: {
                    List<Decl> declsClone = cloneDecls(fromAsQt.decls);
                    Expr newFrom = ExprQt.Op.ALL.make(null, null, declsClone, negateExpression(fromAsQt.sub));
                    checkExpr(newFrom);
                    return generateMutations(newFrom);
                }
                case SOME: {
                    List<Decl> declsClone = cloneDecls(fromAsQt.decls);
                    Expr someNegative = ExprQt.Op.SOME.make(null, null, declsClone, (Expr) fromAsQt.sub.clone());//negateExpression(fromAsQt.sub));
                    checkExpr(someNegative);
                    return Collections.singletonList(
                            new Pair<>(someNegative, false)
                    );
                }
                case LONE:
                case ONE:
                case SUM:
                case COMPREHENSION:
                    return Collections.emptyList(); //TODO: future work
            }
        } else if (from instanceof ExprUnary || from instanceof ExprBinary || from instanceof ExprList || from instanceof ExprCall) {
            Expr expr = transformComplex(from);
            return Arrays.asList(
                    new Pair<>((Expr) expr.clone(), true),
                    new Pair<>((Expr) expr.clone(), false)
            );
        } else if (from instanceof ExprLet) {
            ExprLet fromAsLet = (ExprLet) from;
            ExprVar clonedVar = (ExprVar) fromAsLet.var.clone();
            Expr boundClone = (Expr) fromAsLet.expr.clone();
            Expr negatedFormula = (Expr) fromAsLet.sub.clone(); //negateExpression(fromAsLet.sub);
            Expr negatedLet = ExprLet.make(null, clonedVar, boundClone, negatedFormula);
            checkExpr(negatedLet);
            return Collections.singletonList(
                    new Pair<>(negatedLet, false)
            );
        }
        return Collections.emptyList();
    }

    private static Expr transformComplex(Expr original) throws CommandMutationException {
        if (original instanceof ExprBinary) {
            Expr leftTransformed = transformComplex(((ExprBinary)original).left);
            Expr rightTransformed = transformComplex(((ExprBinary)original).right);
            Expr transformedBinary = ((ExprBinary) original).op.make(null, null, leftTransformed, rightTransformed);
            checkExpr(transformedBinary);
            return transformedBinary;

        } else if (original instanceof ExprList) {
            ExprList originalAsList = (ExprList) original;
            if (originalAsList.op.equals(ExprList.Op.AND) || originalAsList.op.equals(ExprList.Op.OR)) {
                List<Expr> transformedExpressions = new LinkedList<>();
                for (Expr expr : originalAsList.args) {
                    Expr transformedExpr = transformComplex(expr);
                    transformedExpressions.add(transformedExpr);
                }
                Expr transformedList = ExprList.make(null, null, originalAsList.op, transformedExpressions);
                checkExpr(transformedList);
                return transformedList;
            }
        } else if (original instanceof ExprLet) {
            ExprLet originalAsLet = (ExprLet) original;
            Expr transformedFormula = transformComplex(originalAsLet.sub);
            Expr transformedLet = ExprLet.make(null, (ExprVar) originalAsLet.var.clone(), (Expr) originalAsLet.expr.clone(), transformedFormula);
            checkExpr(transformedLet);
            return transformedLet;
        } else if (original instanceof ExprQt) {
            ExprQt originalAsQt = (ExprQt) original;
            if (originalAsQt.op.equals(ExprQt.Op.NO)) {
                return transformComplex(transformNo(originalAsQt));
            } else if (originalAsQt.op.equals(ExprQt.Op.ALL)) {
                Expr transformedQt = ExprQt.Op.SOME.make(null, null, cloneDecls(originalAsQt.decls), (Expr) originalAsQt.sub.clone());
                checkExpr(transformedQt);
                return transformedQt;
            }
        }
        return (Expr) original.clone();
    }

    private static Expr transformAll(ExprQt original) throws CommandMutationException {
        Expr positive = ExprQt.Op.SOME.make(null, null, cloneDecls(original.decls), (Expr) original.sub.clone());
        checkExpr(positive);
        return positive;
    }

    private static Expr transformNo(ExprQt original) throws CommandMutationException {
        Expr noAsAll = ExprQt.Op.ALL.make(null, null, cloneDecls(original.decls), negateExpression(original.sub));
        checkExpr(noAsAll);
        return transformAll((ExprQt) noAsAll);
    }

    private static Expr negateExpression(Expr original) throws CommandMutationException {
        Expr negatedExpression = ExprUnary.Op.NOT.make(null, (Expr) original.clone());
        checkExpr(negatedExpression);
        return negatedExpression;
    }

    private static List<Decl> cloneDecls(List<Decl> original) {
        List<Decl> declsClone = new LinkedList<>();
        for (Decl d : original) {
            List<ExprHasName> dnamesClone = new LinkedList<>();
            for (Expr dn : d.names)
                dnamesClone.add((ExprHasName) dn.clone());
            Expr exprClone = (Expr) d.expr.clone();
            Decl dclone = new Decl(d.isPrivate, d.disjoint, d.disjoint2, dnamesClone, exprClone);
            declsClone.add(dclone);
        }
        return declsClone;
    }

    private static void checkExpr(Expr toCheck) throws CommandMutationException {
        if (toCheck.errors != null && !toCheck.errors.isEmpty())
            throw new CommandMutationException("Errors found in new expression: \n\tExpression: " + toCheck.toString() + "\n\tErrors: " + toCheck.errors.stream().map(Throwable::toString).collect(Collectors.joining(",")) + "\n");
    }

}

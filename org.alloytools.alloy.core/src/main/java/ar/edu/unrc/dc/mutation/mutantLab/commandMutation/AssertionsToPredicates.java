package ar.edu.unrc.dc.mutation.mutantLab.commandMutation;

import ar.edu.unrc.dc.mutation.CheatingIsBadMkay;
import ar.edu.unrc.dc.mutation.Cheats;
import edu.mit.csail.sdg.ast.*;
import edu.mit.csail.sdg.parser.CompModule;

import java.util.*;
import java.util.stream.Collectors;

/**
 * This class transform a check command to a run command that should generate some instances, using the following transformations:
 * <ul>
 *     <li>'check all x | P[x]' will be transformed to 'run some x | P[x] expect 1'</li>
 *     <li>'check P' will be transformed to 'run P expect 1'</li>
 *     <li>'check no x | P[x]' will be transformed as if 'check all x | not P[x]'</li>
 *     <li>'check some x | P[x]' will be transformed to 'run some x | P[x] expect 1'</li>
 *     <li>On complex expressions, 'all' expressions will be transformed to 'some' and 'no' expressions will be first transformed to 'all' and then to 'some'</li>
 * </ul>
 */
public class AssertionsToPredicates {

    public static Optional<Collection<Command>> generatePredicateFromAssertion(Command from, CompModule context) throws CommandMutationException {
        if (from == null)
            throw new IllegalArgumentException("from command is null");
        if (!from.check)
            throw new IllegalArgumentException("from command is not a check command (not an assertion)");
        Expr facts = from.hasFacts()? from.getFacts():null;
        Expr formula = getOriginalAssertionFormula(from.hasFacts()?from.getFormulaWithoutFacts():from.formula);
        Optional<Collection<Expr>> mutations = transformToPredicateFormula(formula);
        Collection<Command> predicateCommands = new LinkedList<>();
        if (mutations.isPresent()) {
            for (Expr mutation : mutations.get()) {
                Expr newCommandFormula = facts == null ? mutation : ExprBinary.Op.AND.make(null, null, (Expr) facts.clone(), mutation);
                Func newPredicate = generatePredicate(newCommandFormula, from);
                Command newPredicateCommand = generatePredicateCommand(newPredicate);
                try {
                    Cheats.addFunctionToModule(context, newPredicate);
                } catch (CheatingIsBadMkay e) {
                    throw new Error("An error occurred while adding predicate to ast", e);
                }
                try {
                    Cheats.addCommand(newPredicateCommand, context);
                } catch (CheatingIsBadMkay e) {
                    throw new Error("An error occurred while adding command to ast", e);
                }
                predicateCommands.add(newPredicateCommand);
            }
        }
        if (predicateCommands.isEmpty())
            return Optional.empty();
        else
            return Optional.of(predicateCommands);
    }

    private static Func generatePredicate(Expr testFormula, Command fromAssertionsCmd) {
        String from = fromAssertionsCmd.nameExpr instanceof ExprVar?((ExprVar) fromAssertionsCmd.nameExpr).label:"NO_NAME";
        Random rng = new Random();
        String name = "predicate_from_assertion_"+from+"_" + rng.nextInt(100) + "_" + rng.nextInt(100) + "_" + rng.nextInt(100);
        Func predicate = new Func(null, name, null, null, testFormula);
        predicate.setGenerated();
        return predicate;
    }

    private static Command generatePredicateCommand(Func predicate) {
        ExprVar predName = ExprVar.make(null, predicate.label);
        predName.setReferenced(predicate);
        Expr formula = predicate.getBody();
        Command predicateCommand = new Command(null, predName, predicate.label, false, -1, -1, -1, 1, null, null, formula, null);
        predicateCommand.testType(Command.TestType.UNTRUSTED);
        return predicateCommand;
    }

    private static Expr getOriginalAssertionFormula(Expr expr) {
        if (expr instanceof ExprUnary) {
            ExprUnary exprAsUnary = (ExprUnary) expr;
            if (exprAsUnary.op.equals(ExprUnary.Op.NOOP))
                return getOriginalAssertionFormula(exprAsUnary.sub);
            if (exprAsUnary.op.equals(ExprUnary.Op.NOT))
                return exprAsUnary.sub;
        } else if (expr instanceof ExprBinary) {
            ExprBinary exprAsBinary = (ExprBinary) expr;
            if (isTrue(exprAsBinary.left))
                return getOriginalAssertionFormula(exprAsBinary.right);
        }
        return expr;
    }

    private static boolean isTrue(Expr expr) {
        if (expr instanceof ExprUnary && ((ExprUnary)expr).op.equals(ExprUnary.Op.NOOP))
            return isTrue(((ExprUnary)expr).sub);
        else
            return expr instanceof ExprConstant && ((ExprConstant)expr).op.equals(ExprConstant.Op.TRUE);
    }

    private static Optional<Collection<Expr>> transformToPredicateFormula(Expr from) throws CommandMutationException {
        if (from instanceof ExprUnary && ((ExprUnary)from).op.equals(ExprUnary.Op.NOOP))
            return transformToPredicateFormula(((ExprUnary)from).sub);
        if (from instanceof ExprQt) {
            ExprQt fromAsQt = (ExprQt) from;
            switch (fromAsQt.op) {
                case ALL:
                case SOME: {
                    List<Decl> declsClonePositive = cloneDecls(fromAsQt.decls);
                    Expr asSome = ExprQt.Op.SOME.make(null, null, declsClonePositive, (Expr) fromAsQt.sub.clone());
                    checkExpr(asSome);
                    return Optional.of(Collections.singletonList(asSome));
                }
                case NO: {
                    List<Decl> declsClone = cloneDecls(fromAsQt.decls);
                    Expr asSomeNegated = ExprQt.Op.SOME.make(null, null, declsClone, negateExpression(fromAsQt.sub));
                    checkExpr(asSomeNegated);
                    return Optional.of(Collections.singletonList(asSomeNegated));
                }
                case LONE:
                case ONE: {
                    List<Decl> declsClone = cloneDecls(fromAsQt.decls);
                    Expr noChange = fromAsQt.op.make(null, null, declsClone, (Expr) fromAsQt.sub.clone());
                    checkExpr(noChange);
                    return Optional.of(Collections.singletonList(noChange));
                }
                default: return Optional.empty(); //not yet supported
            }
        } else if (from instanceof ExprUnary) {
            ExprUnary fromAsUnary = (ExprUnary) from;
            if (!fromAsUnary.op.equals(ExprUnary.Op.NOT)) {
                Expr noChange = (Expr) fromAsUnary.clone();
                checkExpr(noChange);
                return Optional.of(Collections.singletonList(noChange));
            }
            Optional<Collection<Expr>> transformationsOfSub = transformToPredicateFormula(fromAsUnary.sub);
            Collection<Expr> transformations = new LinkedList<>();
            if (transformationsOfSub.isPresent()) {
                for (Expr subTransformation : transformationsOfSub.get()) {
                    Expr transformation = ExprUnary.Op.NOT.make(null, subTransformation);
                    checkExpr(transformation);
                    transformations.add(transformation);
                }
            }
            if (transformations.isEmpty())
                return Optional.empty();
            else
                return Optional.of(transformations);
        } else if (from instanceof ExprBinary) {
            ExprBinary fromAsBinary = (ExprBinary) from;
            switch (fromAsBinary.op) {
                case AND:
                case OR: {
                    Optional<Collection<Expr>> leftTransformations = transformToPredicateFormula(fromAsBinary.left);
                    Optional<Collection<Expr>> rightTransformations = transformToPredicateFormula(fromAsBinary.right);
                    if (!leftTransformations.isPresent() || !rightTransformations.isPresent())
                        return Optional.empty();
                    Collection<Expr> transformations = new LinkedList<>();
                    for (Expr leftTransformation : leftTransformations.get()) {
                        for (Expr rightTransformation : rightTransformations.get()) {
                            Expr transformedBinary = fromAsBinary.op.make(null, null, leftTransformation, rightTransformation);
                            checkExpr(transformedBinary);
                            transformations.add(transformedBinary);
                        }
                    }
                    if (transformations.isEmpty())
                        return Optional.empty();
                    else
                        return Optional.of(transformations);
                }
                case IMPLIES: {
                    Optional<Collection<Expr>> leftTransformations = transformToPredicateFormula(fromAsBinary.left);
                    Optional<Collection<Expr>> rightTransformations = transformToPredicateFormula(fromAsBinary.right);
                    if (!leftTransformations.isPresent() || !rightTransformations.isPresent())
                        return Optional.empty();
                    Collection<Expr> transformations = new LinkedList<>();
                    for (Expr leftTransformation : leftTransformations.get()) {
                        for (Expr rightTransformation : rightTransformations.get()) {
                            Expr transformedBinary = ExprBinary.Op.AND.make(null, null, leftTransformation, rightTransformation);
                            checkExpr(transformedBinary);
                            transformations.add(transformedBinary);
                        }
                    }
                    if (transformations.isEmpty())
                        return Optional.empty();
                    else
                        return Optional.of(transformations);
                }
                case IFF: {
                    Optional<Collection<Expr>> leftTransformations = transformToPredicateFormula(fromAsBinary.left);
                    Optional<Collection<Expr>> rightTransformations = transformToPredicateFormula(fromAsBinary.right);
                    if (!leftTransformations.isPresent() || !rightTransformations.isPresent())
                        return Optional.empty();
                    Collection<Expr> transformations = new LinkedList<>();
                    for (Expr leftTransformation : leftTransformations.get()) {
                        for (Expr rightTransformation : rightTransformations.get()) {
                            Expr transformedBinaryNegated = ExprBinary.Op.AND.make(null, null, negateExpression(leftTransformation), negateExpression(rightTransformation));
                            Expr transformedBinaryPositive = ExprBinary.Op.AND.make(null, null, leftTransformation, rightTransformation);
                            checkExpr(transformedBinaryNegated);
                            checkExpr(transformedBinaryPositive);
                            transformations.add(transformedBinaryNegated);
                            transformations.add(transformedBinaryPositive);
                        }
                    }
                    if (transformations.isEmpty())
                        return Optional.empty();
                    else
                        return Optional.of(transformations);
                }
                case EQUALS:
                case NOT_EQUALS:
                case LT:
                case LTE:
                case GT:
                case GTE:
                case NOT_LT:
                case NOT_LTE:
                case NOT_GT:
                case NOT_GTE:
                case IN:
                case NOT_IN: {
                    Expr noChange = (Expr) fromAsBinary.clone();
                    checkExpr(noChange);
                    return Optional.of(Collections.singletonList(noChange));
                }
            }
        } else if (from instanceof ExprList) {
            ExprList fromAsList = (ExprList) from;
            Expr listAsBinary = null;
            if (fromAsList.op.equals(ExprList.Op.AND) || fromAsList.op.equals(ExprList.Op.OR)) {
                for (Expr expr : fromAsList.args) {
                    if (listAsBinary == null) {
                        listAsBinary = (Expr) expr.clone();
                    } else if (fromAsList.op.equals(ExprList.Op.AND)) {
                        listAsBinary = ExprBinary.Op.AND.make(null, null, listAsBinary, (Expr) expr.clone());
                    } else {
                        listAsBinary = ExprBinary.Op.OR.make(null, null, listAsBinary, (Expr) expr.clone());
                    }
                    checkExpr(listAsBinary);
                }
                return transformToPredicateFormula(listAsBinary);
            }
            return Optional.empty();
        } else if (from instanceof ExprCall) {
            Expr noChange = (Expr) from.clone();
            checkExpr(noChange);
            return Optional.of(Collections.singletonList(noChange));
        } else if (from instanceof ExprLet) {
            ExprLet fromAsLet = (ExprLet) from;
            ExprVar clonedVar = (ExprVar) fromAsLet.var.clone();
            Expr boundClone = (Expr) fromAsLet.expr.clone();
            Optional<Collection<Expr>> formulaTransformations = transformToPredicateFormula((Expr) fromAsLet.sub.clone());
            if (formulaTransformations.isPresent()) {
                Collection<Expr> transformations = new LinkedList<>();
                for (Expr formulaTransformation : formulaTransformations.get()) {
                    Expr transformedLet = ExprLet.make(null, clonedVar, boundClone, formulaTransformation);
                    checkExpr(transformedLet);
                    transformations.add(transformedLet);
                }
                if (transformations.isEmpty())
                    return Optional.empty();
                else
                    return Optional.of(transformations);
            }
            return Optional.empty();
        }
        return Optional.empty();
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
            throw new CommandMutationException("Errors found in new expression: \n\tExpression: " + toCheck + "\n\tErrors: " + toCheck.errors.stream().map(Throwable::toString).collect(Collectors.joining(",")) + "\n");
    }

}

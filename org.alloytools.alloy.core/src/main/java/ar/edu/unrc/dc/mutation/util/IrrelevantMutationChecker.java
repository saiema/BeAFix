package ar.edu.unrc.dc.mutation.util;

import ar.edu.unrc.dc.mutation.Mutation;
import edu.mit.csail.sdg.ast.*;

import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;

import static ar.edu.unrc.dc.mutation.Mutator.*;
import static ar.edu.unrc.dc.mutation.util.TypeChecking.getContext;

/**
 * This class will check if a mutation results in an irrelevant mutation for repair purposes. As an example consider
 * the mutation:
 * <pre>
 *     Y ==> X in the context of X = Y
 * </pre>
 * The resulting mutation is irrelevant since it's the same as replacing the whole expression with {@code true}.
 */
public class IrrelevantMutationChecker {

    private static final boolean ENABLED = true;

    /**
     * Checks if a mutation is irrelevant as explained in the class' documentation.
     * Since a mutation can be a mutation of a mutation, but all previous mutations are supposed to be checked, then
     * this method will only check the latest mutation.
     * <b>
     *     Is important to note that this method will assume that all previous mutations are applied to the AST.
     * </b>
     *
     * @param mutation  :   the mutation to test for
     * @return {@code true} iff the mutation is irrelevant, in which case it should be discarded
     */
    public synchronized static boolean isIrrelevant(Mutation mutation) {
        if (!ENABLED)
            return false;
        Expr mutant = mutation.mutant();
        Expr original = mutation.original();
        switch (mutation.operator()) {
            case AORB: return checkIrrelevantArithmeticBinaryExpression(mutant);
            case BESOR: return checkIrrelevantSetBinaryOperation(mutant);
            case ROR:
            case EMOR:
            case SSE:
            case SSS:
            case COR: return checkSameOperands(mutant);
            case RUOR:
            case RUOI: return checkRelationalUnaryOperatorRepetitionWithRespectToOriginal(mutant, original);
            case BES:
            case VCR:
            case JES: {
                Optional<Expr> nearestBinaryExpression = getNearestBinaryOrListExpression(original);
                if (nearestBinaryExpression.isPresent()) {
                    if (nearestBinaryExpression.get() instanceof ExprBinary) {
                        ExprBinary originalBinaryExpression = (ExprBinary) nearestBinaryExpression.get();
                        Expr originalLeft = originalBinaryExpression.left;
                        Expr originalRight = originalBinaryExpression.right;
                        if (Browsable.equals(originalLeft, original))
                            return mutant.toString().compareTo(originalRight.toString()) == 0;
                        else if (Browsable.equals(originalRight, original))
                            return mutant.toString().compareTo(originalLeft.toString()) == 0;
                    } else {
                        ExprList originalListExpression = (ExprList) nearestBinaryExpression.get();
                        Set<String> expressions = new TreeSet<>();
                        boolean repeated = false;
                        for (Expr e : originalListExpression.args) {
                            if (Browsable.equals(e, original)) {
                                repeated = !expressions.add(mutant.toString());
                            } else {
                                repeated = !expressions.add(e.toString());
                            }
                        }
                        return repeated;
                    }
                }
            }
        }
        return false;
    }

    private static boolean checkRelationalUnaryOperatorRepetitionWithRespectToOriginal(Expr mutant, Expr original) {
        if (isUnaryRelationalExpression(mutant)) {
            ExprUnary mAsUnary = (ExprUnary) mutant;
            Optional<Expr> context = getContext(original);
            if (context.isPresent()) {
                Expr cExpr = context.get();
                if (cExpr instanceof ExprUnary) {
                    ExprUnary oAsUnary = getNearestNonNOOPUnaryParent((ExprUnary)cExpr);
                    return oAsUnary.op.equals(mAsUnary.op);
                }
            }
            return false;
        }
        throw new IllegalArgumentException("Mutant expression should be a unary relational expression " + mutant.toString());
    }

    private static ExprUnary getNearestNonNOOPUnaryParent(ExprUnary e) {
        Optional<Expr> nearestParent = getContext(e);
        if (nearestParent.isPresent()) {
            Expr parent = nearestParent.get();
            if (parent instanceof ExprUnary) {
                ExprUnary parentAsUnary = (ExprUnary) parent;
                if (parentAsUnary.op.equals(ExprUnary.Op.NOOP))
                    return getNearestNonNOOPUnaryParent(parentAsUnary);
                return parentAsUnary;
            }
        }
        return e;
    }

    private static Optional<Expr> getNearestBinaryOrListExpression(Expr e) {
        if (e instanceof ExprUnary && ((ExprUnary)e).op.equals(ExprUnary.Op.NOOP)) {
            return getNearestBinaryOrListExpression(((ExprUnary)e).sub);
        }
        Optional<Expr> context = getContext(e);
        if (context.isPresent()) {
            Expr cExpr = context.get();
            if (cExpr instanceof ExprBinary || cExpr instanceof ExprList)
                return Optional.of(cExpr);
        }
        return Optional.empty();
    }

    private static boolean checkSameOperands(Expr mutant) {
        if (mutant instanceof ExprBinary) {
            return ((ExprBinary)mutant).left.toString().compareTo(((ExprBinary)mutant).right.toString()) == 0;
        } else if (mutant instanceof ExprList) {
            ExprList mAsList = (ExprList) mutant;
            Set<String> operands = new TreeSet<>();
            for (Expr e : mAsList.args) {
                if (!operands.add(e.toString())) {
                    return true;
                }
            }
        } else
            throw new IllegalArgumentException("Mutant expression should be a binary or list expression " + mutant.toString());
        return false;
    }

    private static boolean checkIrrelevantSetBinaryOperation(Expr mutant) {
        if (isSetBinaryExpression(mutant)) {
            ExprBinary mAsBinary = (ExprBinary) mutant;
            Expr left = mAsBinary.left;
            Expr right = mAsBinary.right;
            switch (mAsBinary.op) {
                case PLUS :
                case MINUS:
                case INTERSECT:
                case PLUSPLUS: return left.toString().compareTo(right.toString()) == 0;
                default : return false;
            }
        }
        throw new IllegalArgumentException("Mutant expression should be a binary operation on sets " + mutant.toString());
    }

    private static boolean checkIrrelevantArithmeticBinaryExpression(Expr mutant) {
        if (isArithmeticBinaryExpression(mutant)) {
            ExprBinary mAsBinary = (ExprBinary) mutant;
            Expr left = mAsBinary.left;
            Expr right = mAsBinary.right;
            switch (mAsBinary.op) {
                case DIV:
                case MUL: {
                    return isOne(left) || isOne(right) || isZero(left) || isZero(right);
                }
                case IMINUS:
                case IPLUS: {
                    return isZero(left) || isZero(right);
                }
                default : return false;
            }
        }
        throw new IllegalArgumentException("Mutant expression should be a binary arithmetic expression " + mutant.toString());
    }

    private static boolean isOne(Expr e) {
        if (e instanceof ExprConstant)
            return e.toString().compareTo("1") == 0;
        return false;
    }

    private static boolean isZero(Expr e) {
        if (e instanceof ExprConstant)
            return e.toString().compareTo("0") == 0;
        return false;
    }

}

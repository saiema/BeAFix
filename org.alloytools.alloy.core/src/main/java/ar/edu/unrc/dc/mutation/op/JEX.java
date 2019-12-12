package ar.edu.unrc.dc.mutation.op;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.MutationConfiguration;
import ar.edu.unrc.dc.mutation.MutationConfiguration.ConfigKey;
import ar.edu.unrc.dc.mutation.Mutator;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.ExprBinary;
import edu.mit.csail.sdg.parser.CompModule;


public abstract class JEX extends Mutator {

    protected JEX(CompModule context) {
        super(context);
    }

    @Override
    public Optional<List<Mutation>> visit(ExprBinary x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        if (isMutable(x)) {
            Optional<List<Mutation>> mutantsLeft = generateMutants(x, x.left);
            Optional<List<Mutation>> mutantsRight = generateMutants(x, x.right);
            if (mutantsLeft.isPresent()) {
                mutations.addAll(mutantsLeft.get());
            }
            if (mutantsRight.isPresent()) {
                mutations.addAll(mutantsRight.get());
            }
        }
        Optional<List<Mutation>> leftMutations = x.left.accept(this);
        Optional<List<Mutation>> rightMutations = x.right.accept(this);
        if (leftMutations.isPresent())
            mutations.addAll(leftMutations.get());
        if (rightMutations.isPresent())
            mutations.addAll(rightMutations.get());
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return EMPTY;
    }

    protected abstract boolean isMutable(Expr x);

    protected abstract Optional<List<Mutation>> generateMutants(Expr from, Expr replace);

    protected boolean strictTypeCheck() {
        Optional<Object> configValue = MutationConfiguration.getInstance().getConfigValue(ConfigKey.OPERATOR_JEX_TYPE_CHECK);
        if (configValue.isPresent())
            return (Boolean) configValue.get();
        return false;
    }

    //    @Override
    //    protected boolean compatibleVariablesChecker(Expr toReplace, Expr replacement, Type replacementType, boolean strictTypeChecking) {
    //        if (strictTypeChecking)
    //            return toReplace.type().equals(replacementType);
    //        Type toReplaceType = toReplace.type();
    //        ProductType toReplaceTypeFirst = null;
    //        ProductType toReplaceTypeLast = null;
    //        ProductType replacementTypeFirst = null;
    //        ProductType replacementTypeLast = null;
    //        Iterator<ProductType> trIt = toReplaceType.iterator();
    //        Iterator<ProductType> rIt = replacementType.iterator();
    //        while (trIt.hasNext()) {
    //            ProductType current = trIt.next();
    //            if (toReplaceTypeFirst == null)
    //                toReplaceTypeFirst = current;
    //            toReplaceTypeLast = current;
    //        }
    //        while (rIt.hasNext()) {
    //            ProductType current = rIt.next();
    //            if (replacementTypeFirst == null)
    //                replacementTypeFirst = current;
    //            replacementTypeLast = current;
    //        }
    //        if (toReplaceTypeFirst == null || toReplaceTypeLast == null)
    //            return false;
    //        if (replacementTypeFirst == null || replacementTypeLast == null)
    //            return false;
    //        return toReplaceTypeFirst.equals(replacementTypeFirst) && toReplaceTypeLast.equals(replacementTypeLast);
    //        Browsable parentRaw = toReplace.getBrowsableParent();
    //        if (parentRaw instanceof ExprBinary && ((ExprBinary) parentRaw).op.equals(Op.JOIN)) {
    //            ExprBinary parent = (ExprBinary) toReplace.getBrowsableParent();
    //            Browsable toReplaceParentsParent = parent.getBrowsableParent();
    //            Type joinedType = Type.EMPTY;
    //            boolean left = false, right = false;
    //            if (toReplace.getID() == parent.left.getID()) {
    //                left = true;
    //                joinedType = replacementType.join(parent.right.type());
    //            } else if (toReplace.getID() == parent.right.getID()) {
    //                right = true;
    //                joinedType = parent.left.type().join(replacementType);
    //            }
    //            if (emptyOrNone(joinedType))
    //                return false;
    //            if (toReplaceParentsParent != null && toReplaceParentsParent instanceof ExprBinary) {
    //                ExprBinary trppAsBinary = (ExprBinary) toReplaceParentsParent;
    //                if (trppAsBinary.op.equals(Op.JOIN)) {
    //                    Type parentJoinedType = Type.EMPTY;
    //                    if (parent.getID() == trppAsBinary.left.getID() && right) {
    //                        parentJoinedType = replacementType.join(trppAsBinary.right.type());
    //                    } else if (parent.getID() == trppAsBinary.right.getID() && left) {
    //                        parentJoinedType = trppAsBinary.left.type().join(replacementType);
    //                    }
    //                    return !emptyOrNone(parentJoinedType);
    //                }
    //            }
    //        }
    //        throw new IllegalStateException("This method should be called to check a join expression");
    //    }

}

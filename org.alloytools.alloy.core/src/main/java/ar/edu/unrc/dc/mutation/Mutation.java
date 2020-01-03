package ar.edu.unrc.dc.mutation;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import ar.edu.unrc.dc.mutation.visitors.SearchAndReplace;
import ar.edu.unrc.dc.mutation.visitors.SearchExpr;
import edu.mit.csail.sdg.ast.Browsable;
import edu.mit.csail.sdg.ast.Decl;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.ExprBinary;
import edu.mit.csail.sdg.ast.ExprBinary.Op;
import edu.mit.csail.sdg.ast.ExprHasName;
import edu.mit.csail.sdg.ast.ExprQt;
import edu.mit.csail.sdg.ast.Func;
import edu.mit.csail.sdg.ast.Sig;
import edu.mit.csail.sdg.parser.CompModule;

/**
 * A mutation is represented by a tuple (operator, original, mutant) where the
 * values represent:
 * <li>operator: the operator used, an enum value</li>
 * <li>original: the original node</li>
 * <li>mutant: the mutated node</li>
 *
 * Both the original and the mutant are nodes of type {@code Expr} and the
 * operator is an enum value {@code Ops}
 * <p>
 * A mutation can have a parent mutation, where the parent's mutant node is the
 * this mutation's original node.
 *
 * @see Ops
 * @see edu.mit.csail.sdg.ast.Expr
 *
 */
public class Mutation {

    private Ops                operator;
    private Expr               original;
    private Expr               mutant;
    private Optional<Mutation> parent = Optional.empty();

    public Mutation(Ops operator, Expr original, Expr mutant) {
        this.operator = operator;
        this.original = original;
        this.mutant = mutant;
    }

    public Ops operator() {
        return this.operator;
    }

    public Expr original() {
        return this.original;
    }

    public Expr mutant() {
        return this.mutant;
    }

    public Optional<Mutation> parent() {
        return this.parent;
    }

    /**
     * Merges the mutation represented by this option with another one
     * <p>
     * Does not affect this mutation
     *
     * @param with the mutation to merge with, this means that this mutation will be
     *            applied after the mutation represented by {@code this}
     * @return the {@code with} mutation with {@code this} mutation set as parent,
     *         if {@code this} mutation is compatible with the {@code with} mutation
     *
     * @see #compatible(Mutation, Mutation)
     */
    public Mutation merge(Mutation with) {
        if (compatible(this, with)) {
            with.setParent(this);
            return with;
        } else {
            throw new IllegalArgumentException("Incompatible mutation, call #compatible(Mutation, Mutation) before this method");
        }
    }

    /**
     * Compresses all mutations into one
     *
     * @return {@code this} if this mutation has no parents, or a new mutation with
     *         all parent mutations applied
     */
    public Mutation compress() {
        if (!this.parent.isPresent())
            return this;
        if (!this.parent.get().parent().isPresent()) {
            //compress this with parent
            SearchAndReplace replacer = new SearchAndReplace(this.original, this.mutant);
            Optional<Expr> newMutant = replacer.visitThis(this.parent.get().mutant);
            if (newMutant.isPresent()) {
                return new Mutation(Ops.MULTI, this.parent.get().original, newMutant.get());
            } else {
                throw new IllegalStateException("Couldn't compress mutations but it should be possible");
            }
        } else {
            //compress this with the parent's compression
            Mutation parentCompress = this.parent.get().compress();
            setParent(parentCompress);
            return compress();
        }
    }

    public static boolean compatible(Mutation from, Mutation mutate) {
        if (from.operator.equals(Ops.QTBER) || mutate.operator.equals(Ops.QTBER))
            return false;
        SearchExpr searcher = new SearchExpr(mutate.original);
        return searcher.visitThis(from.mutant);
    }

    public static Optional<Integer> depth(Mutation from, Mutation mutate) {
        return depth(from.mutant(), mutate.original().getID(), 0);
    }

    private static Optional<Integer> depth(Browsable root, int id, int currentDepth) {
        if (root.getID() == id)
            return Optional.of(currentDepth);
        List<Browsable> nodesToVisit = new LinkedList<>();
        nodesToVisit.addAll(root.getSubnodes());
        while (!nodesToVisit.isEmpty()) {
            List<Browsable> childrens = new LinkedList<>();
            for (Browsable n : nodesToVisit) {
                depth(n, id, currentDepth + 1);
            }
        }
        return Optional.empty();
    }

    protected void setParent(Mutation parent) {
        if (parent == null)
            throw new IllegalArgumentException("Can't add a null parent");
        this.parent = Optional.of(parent);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        if (parent.isPresent())
            sb.append("FROM ").append(parent.get().toString()).append("\n\t");
        sb.append("(");
        sb.append(operator.toString());
        sb.append(", ");
        sb.append(toString(original));
        sb.append(", ");
        sb.append(toString(mutant));
        sb.append(")");
        return sb.toString();
    }

    private String toString(Expr x) {
        if (x instanceof ExprQt) {
            StringBuilder sb = new StringBuilder();
            ExprQt xAsQt = (ExprQt) x;
            xAsQt.toString(sb, -1);
            sb.append(" vars:");
            Iterator<Decl> dit = xAsQt.decls.iterator();
            while (dit.hasNext()) {
                Decl d = dit.next();
                Iterator< ? extends ExprHasName> varIt = d.names.iterator();
                while (varIt.hasNext()) {
                    ExprHasName var = varIt.next();
                    sb.append(var.toString());
                    if (varIt.hasNext())
                        sb.append(", ");
                    else
                        sb.append(" : ");
                }
                sb.append(d.expr.toString());
                if (dit.hasNext())
                    sb.append(";");
            }
            return sb.toString();
        }
        return x.toString();
    }

    //VARIABILIZATION RELATED UTILITIES

    private enum SEARCH {
                         LEFT,
                         RIGHT,
                         BOTH
    }

    public boolean isEqualMutation() {
        return isMutationOfBinExpr(this.original, Op.EQUALS, SEARCH.BOTH);
    }

    public boolean isInMutation() {
        return isMutationOfBinExpr(this.original, Op.IN, SEARCH.BOTH);
    }

    public boolean isEqualLeftMutation() {
        return isMutationOfBinExpr(this.original, Op.EQUALS, SEARCH.LEFT);
    }

    public boolean isEqualRightMutation() {
        return isMutationOfBinExpr(this.original, Op.EQUALS, SEARCH.RIGHT);
    }

    public boolean isInLeftMutation() {
        return isMutationOfBinExpr(this.original, Op.IN, SEARCH.LEFT);
    }

    public boolean isInRightMutation() {
        return isMutationOfBinExpr(this.original, Op.IN, SEARCH.RIGHT);
    }

    private boolean isMutationOfBinExpr(Browsable from, Op op, SEARCH search) {
        if (from == null)
            return false;
        if (from instanceof Func)
            return false;
        if (from instanceof Sig)
            return false;
        if (from instanceof CompModule)
            return false;
        if (from instanceof ExprBinary) {
            ExprBinary asBinExpr = (ExprBinary) from;
            if (asBinExpr.op.equals(op)) {
                //search for mutation
                if (from.getID() == this.original.getID()) {
                    //original is inside mutant
                    SearchExpr searcher = new SearchExpr(original);
                    if (searcher.visitThis(mutant))
                        return false;
                    //does the left original expression exists in the mutant
                    searcher = new SearchExpr(asBinExpr.left);
                    boolean originalLeftIsInMutant = searcher.visitThis(mutant);
                    //does the right original expression exists in the mutant
                    searcher = new SearchExpr(asBinExpr.right);
                    boolean originalRightIsInMutant = searcher.visitThis(mutant);
                    if (originalLeftIsInMutant && originalRightIsInMutant) {
                        //mutant changed the operator
                        return false;
                    } else if (originalLeftIsInMutant) {
                        //mutant changed the right part
                        return !search.equals(SEARCH.LEFT) ? true : false;
                    } else if (originalRightIsInMutant) {
                        //mutant changed the left part
                        return !search.equals(SEARCH.RIGHT) ? true : false;
                    } else {
                        //mutant changed the whole original expression
                        return false;
                    }

                } else {
                    SearchExpr searcher = new SearchExpr(original);
                    switch (search) {
                        case BOTH :
                            return searcher.visitThis(asBinExpr.left) || searcher.visitThis(asBinExpr.right);
                        case LEFT :
                            return searcher.visitThis(asBinExpr.left);
                        case RIGHT :
                            return searcher.visitThis(asBinExpr.right);
                    }
                }
            }
        }
        return isMutationOfBinExpr(from.getBrowsableParent(), op, search);
    }

}

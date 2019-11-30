package ar.edu.unrc.dc.mutation;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import edu.mit.csail.sdg.ast.Browsable;
import edu.mit.csail.sdg.ast.Expr;

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

    public static boolean compatible(Mutation from, Mutation mutate) {
        Expr fromMutant = from.mutant();
        int mutateOriginalID = mutate.original().getID();
        List<Browsable> nodesToVisit = new LinkedList<>();
        nodesToVisit.addAll(fromMutant.getSubnodes());
        while (!nodesToVisit.isEmpty()) {
            List<Browsable> childrens = new LinkedList<>();
            for (Browsable n : nodesToVisit) {
                if (n.getID() == mutateOriginalID)
                    return true;
                if (!n.getSubnodes().isEmpty())
                    childrens.addAll(n.getSubnodes());
            }
        }
        return false;
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

}

package ar.edu.unrc.dc.mutation;

import ar.edu.unrc.dc.mutation.MutationConfiguration.ConfigKey;
import ar.edu.unrc.dc.mutation.util.TypeChecking;
import edu.mit.csail.sdg.ast.Decl;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.ExprHasName;
import edu.mit.csail.sdg.ast.ExprQt;

import java.util.Iterator;
import java.util.Optional;

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

    private final Ops                operator;
    private final Expr               original;
    private final Expr               mutant;
    private Mutation parent = null;

    public Mutation(Ops operator, Expr original, Expr mutant) {
        this.operator = operator;
        this.original = original;
        this.mutant = mutant;
        updateMarkedStatus();
        updateVariabilizationVars();
    }

    private void updateMarkedStatus() {
        if (original.directMutGenLimit() > 0) {
            mutant.directBlockStatus(original.directBlockStatus());
            mutant.mutGenLimit(original.directMutGenLimit());
        }
    }

    private void updateVariabilizationVars() {
        if (original.directMutGenLimit() > 0) {
            mutant.setVariabilizationVariables(original.directVariabilizationVariables());
        }
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
        return parent != null?Optional.of(parent):Optional.empty();
    }

    public Expr getMayorAffectedExpression() {
        return TypeChecking.getMayorExpression(original);
    }

    protected void setParent(Mutation parent) {
        if (parent == null)
            throw new IllegalArgumentException("Can't add a null parent");
        this.parent = parent;
    }

    @Override
    protected Object clone() {
        Mutation clonedMutation = new Mutation(operator, original, mutant);
        parent().ifPresent(mutation -> clonedMutation.setParent((Mutation) mutation.clone()));
        return clonedMutation;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null)
            return false;
        if (!(obj instanceof Mutation))
            return false;
        Mutation omut = (Mutation) obj;
        if (!omut.operator().equals(operator))
            return false;
        if (omut.original().toString().compareTo(original.toString()) != 0)
            return false;
        return omut.mutant().toString().compareTo(mutant.toString()) == 0;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        parent().ifPresent(mutation -> sb.append("FROM ").append(mutation.toString()).append("\n\t"));
        sb.append("(");
        sb.append(operator.toString());
        sb.append(", ");
        sb.append(toString(original));
        sb.append(", ");
        sb.append(toString(mutant));
        sb.append(")");
        if (useFullToString()) {
            sb.append("\n");
            sb.append(" from mayor expression : ");
            sb.append(getMayorAffectedExpression().toString());
        }
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

    public int line() {
        return original.pos.y;
    }

    private boolean useFullToString() {
        if (overrideFullToString)
            return true;
        Optional<Object> configValue = MutationConfiguration.getInstance().getConfigValue(ConfigKey.MUTATION_TOSTRING_FULL);
        return configValue.map(o -> (Boolean) o).orElse((Boolean) ConfigKey.MUTATION_TOSTRING_FULL.defaultValue());
    }

    private static boolean overrideFullToString = false;
    public static void overrideFullToString(boolean v) {
        overrideFullToString = v;
    }

}

package ar.edu.unrc.dc.mutation.op;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import ar.edu.unrc.dc.mutation.CheatingIsBadMkay;
import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.MutationConfiguration;
import ar.edu.unrc.dc.mutation.MutationConfiguration.ConfigKey;
import ar.edu.unrc.dc.mutation.Mutator;
import ar.edu.unrc.dc.mutation.Ops;
import ar.edu.unrc.dc.mutation.visitors.QtBoundReplacementVerifier;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.Decl;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.ExprQt;
import edu.mit.csail.sdg.ast.Type;
import edu.mit.csail.sdg.parser.CompModule;

/**
 * Quantifier Bounded Expression Replacer
 * <p>
 *
 * Replaced the bound expression {@code b} in a declaration {@code x:b}
 *
 */
public class QTBER extends Mutator {

    public QTBER(CompModule context) {
        super(context);
    }


    @Override
    public Optional<List<Mutation>> visit(ExprQt x) throws Err {
        List<Mutation> mutations = new LinkedList<>();
        List<Decl> decls = x.decls;
        Expr formula = x.sub;
        Optional<List<Expr>> replacements;
        try {
            replacements = getAllTypes(minGeneration(), maxGeneration());
        } catch (CheatingIsBadMkay e) {
            throw new Error("There was a problem obtaining replacement types", e);
        }
        if (replacements.isPresent()) {
            for (Decl d : decls) {
                Expr bound = d.expr;
                for (Expr replacement : replacements.get()) {
                    if (bound.toString().compareTo(replacement.toString()) == 0)
                        continue;
                    if (checkReplacement(d, replacement, formula)) {
                        mutations.add(new Mutation(whoiam(), x, x.replaceBoundForDecl(d, replacement)));
                    }
                }
            }
        }
        if (!mutations.isEmpty())
            return Optional.of(mutations);
        return EMPTY;
    }

    private boolean checkReplacement(Decl d, Expr replacement, Expr formula) {
        Type trep = getType(replacement);
        for (Expr var : d.names) {
            if (!checkReplacement(var, trep, formula))
                return false;
        }
        return true;
    }

    private boolean checkReplacement(Expr var, Type treplacement, Expr formula) {
        QtBoundReplacementVerifier verifier = new QtBoundReplacementVerifier(var, treplacement);
        return verifier.visitThis(formula);
    }


    @Override
    protected Ops whoiam() {
        return Ops.QTBER;
    }

    private int minGeneration() {
        Optional<Object> configValue = MutationConfiguration.getInstance().getConfigValue(ConfigKey.OPERATOR_QTBER_BOUND_MIN_GENERATION);
        if (configValue.isPresent())
            return (int) configValue.get();
        return (int) ConfigKey.OPERATOR_QTBER_BOUND_MIN_GENERATION.defaultValue();
    }

    private int maxGeneration() {
        Optional<Object> configValue = MutationConfiguration.getInstance().getConfigValue(ConfigKey.OPERATOR_QTBER_BOUND_MAX_GENERATION);
        if (configValue.isPresent())
            return (int) configValue.get();
        return (int) ConfigKey.OPERATOR_QTBER_BOUND_MAX_GENERATION.defaultValue();
    }

}

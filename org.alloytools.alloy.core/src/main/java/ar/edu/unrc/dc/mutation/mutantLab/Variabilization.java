package ar.edu.unrc.dc.mutation.mutantLab;

import ar.edu.unrc.dc.mutation.CheatingIsBadMkay;
import ar.edu.unrc.dc.mutation.Cheats;
import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.Ops;
import ar.edu.unrc.dc.mutation.util.ContextExpressionExtractor;
import ar.edu.unrc.dc.mutation.util.RepairReport;
import ar.edu.unrc.dc.mutation.visitors.MarkedExpressionsCollector;
import edu.mit.csail.sdg.alloy4.A4Reporter;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.ast.*;
import edu.mit.csail.sdg.ast.Sig.Field;
import edu.mit.csail.sdg.parser.CompModule;
import edu.mit.csail.sdg.translator.A4Options;
import edu.mit.csail.sdg.translator.A4Solution;
import edu.mit.csail.sdg.translator.TranslateAlloyToKodkod;

import java.util.*;
import java.util.logging.Logger;
import java.util.stream.Collectors;

public class Variabilization {

    private static Variabilization instance;

    public synchronized static void initializeInstance(A4Reporter reporter, A4Options options) {
        if (instance != null)
            throw new IllegalStateException("Variabilization already initialized");
        instance = new Variabilization(reporter, options);
    }

    public synchronized static Variabilization getInstance() {
        if (instance == null)
            throw new IllegalStateException("Variabilization not initialized");
        return instance;
    }

    public synchronized static void destroyInstance() {
        if (instance == null)
            throw new IllegalStateException("Variabilization instance has not been initialized");
        instance = null;
    }

    private final A4Reporter reporter;
    private final A4Options options;

    private Variabilization(A4Reporter reporter, A4Options options) {
        if (options == null)
            throw new IllegalArgumentException("options can't be null");
        this.reporter = reporter;
        this.options = options;
    }

    public boolean variabilizationCheck(Candidate from, List<Command> commands, Logger logger) throws Err {
        if (from == null)
            throw new IllegalArgumentException("from can't be null");
        if (from.isLast())
            throw new IllegalArgumentException("from candidate can't be a last candidate");
        if (commands == null)
            throw new IllegalArgumentException("commands can't be null");
        logger.info("Variabilization check for:\n" + from.toString() + "Using commands: [" + commands.stream().map(Command::toString).collect(Collectors.joining(",")) + "]");
        if (commands.isEmpty())
            return true;
        if (!MutantLab.getInstance().applyCandidateToAst(from))
            throw new Error("There was a problem while mutating the ast");
        Optional<List<Pair<Expr, Boolean>>> markedExpressions = getMarkedExpressionsForVariabilizationCheck(from);
        Sig magicSig = null;
        boolean solverResult = true;
        boolean astRestored = false;
        if (markedExpressions.isPresent()) {
            CompModule context = from.getContext();
            Optional<Sig> magicSigOp = generateMagicSigForExpressions(markedExpressions.get());
            if (magicSigOp.isPresent()) {
                magicSig = magicSigOp.get();
                RepairReport.getInstance().incVariabilizationChecks();
                List<Mutation> variabilizationMutations = generateVariabilizationMutations(magicSig, markedExpressions.get());
                restoreAst(from, magicSig, true, false);
                astRestored = true;
                variabilizationMutations.addAll(from.getMutations());
                Candidate variabilizationCandidate = generateCandidateForVariabilization(from.getContext(), variabilizationMutations);
                logger.info("Reporter available: " + (reporter != null));
                logger.info("variabilization candidate:\n" + variabilizationCandidate.toString());
                logger.info("Magic signature:\n" + magicSig.toExtendedString());
                try {
                    Cheats.addSigToModule(context, magicSig);
                } catch (CheatingIsBadMkay e) {
                    throw new Error("There was a problem while adding the magic signature to the module", e);
                }
                solverResult = runSolver(from.getContext(), commands, variabilizationCandidate, logger);
                logger.info("solver returned: " + solverResult + "\n");
                if (solverResult)
                    RepairReport.getInstance().incVariabilizationChecksPassed();
                else
                    RepairReport.getInstance().incVariabilizationChecksFailed(from.getCurrentMarkedExpression());
            }
        }
        restoreAst(from, magicSig, !astRestored, true);
        return solverResult;
    }

    private Candidate generateCandidateForVariabilization(CompModule module, List<Mutation> variabilizationMutations) {
        Candidate variabilizationCandidate = Candidate.original(module);
        for (Mutation m : variabilizationMutations) {
            variabilizationCandidate = Candidate.mutantFromCandidate(variabilizationCandidate, m);
        }
        return variabilizationCandidate;
    }

    private void restoreAst(Candidate from, Sig magicSig, boolean undoMutations, boolean undoMagicSig) throws Err {
        CompModule context = from.getContext();
        try {
            if (undoMagicSig && magicSig != null) {
                Cheats.removeSigFromModule(context, magicSig);
            }
            if (undoMutations && !ASTMutator.getInstance().undoMutations()) {
                throw new Error("There was a problem while mutating the ast");
            }
        } catch (CheatingIsBadMkay e) {
            throw new Error("There was a problem while removing the magic signature from the module", e);
        }
    }

    private boolean runSolver(CompModule module, List<Command> commands, Candidate variabilizationCandidate, Logger logger) {
        boolean repaired = true;
        int commandsPassed = 0;
        for (Command cmd : commands) {
            try {
                Browsable.freezeParents();
                A4Solution ai = TranslateAlloyToKodkod.execute_commandFromBookWithMutation(reporter, module.getAllReachableSigs(), cmd, options, variabilizationCandidate);
                variabilizationCandidate.clearMutatedStatus();
                Browsable.unfreezeParents();
                if (ai != null) {
                    if (ai.satisfiable()) {
                        if (cmd.expects == 0 || (cmd.expects == -1 && cmd.check)) {
                            repaired = false;
                        }
                    } else {
                        if (cmd.expects == 1 || (cmd.expects == -1 && !cmd.check)) {
                            repaired = false;
                        }
                    }
                } else {
                    repaired = false;
                }
            } catch (Exception e) {
                repaired = false; // if the mutation fails in one command ignore the rest
                variabilizationCandidate.clearMutatedStatus();
                Browsable.unfreezeParents();
            }
            logger.info("Command " + cmd.toString() + (repaired?" SUCCEED":" FAILED"));
            if (!repaired) break;
            else {
                commandsPassed++;
                logger.info("Passed " + commandsPassed + " of " + commands.size());
            }
        }
        return repaired;
    }

    private List<Mutation> generateVariabilizationMutations(Sig magicSig, List<Pair<Expr, Boolean>> markedExpressions) throws Err {
        if (markedExpressions.stream().noneMatch(p -> p.b))
            throw new IllegalStateException("No marked expressions available to generate variabilization mutations");
        List<Mutation> variabilizationMutations = new LinkedList<>();
        for (Pair<Expr, Boolean> me : markedExpressions) {
            if (!me.b) {
                continue;
            }
            Expr magicExpr = generateVariabilizationExpression(me.a, magicSig);
            Mutation varMut;
            if (me.a.type().is_bool) {
                ExprUnary boolCheck = (ExprUnary) ExprUnary.Op.ONE.make(null, magicExpr);
                varMut = new Mutation(Ops.VAR, me.a, boolCheck);
            } else {
                varMut = new Mutation(Ops.VAR, me.a, magicExpr);
            }
            variabilizationMutations.add(varMut);
        }
        return variabilizationMutations;
    }

    private Expr generateVariabilizationExpression(Expr x, Sig magicSig) {
        Expr variabilizationExpression;
        Expr booleanVarsConjunction = null;
        if (hasBooleanVars(x)) {
            Optional<List<Expr>> booleanVariabilizationVars = getBooleanVariablesFor(x);
            if (!booleanVariabilizationVars.isPresent())
                throw new IllegalStateException("Expression " + x.toString() + " has boolean variables but non was retrieved");
            booleanVarsConjunction = generateBooleanVarsConjunction(booleanVariabilizationVars.get());
        }
        Optional<Field> magicField = getMarkedExpressionReplacement(magicSig, x);
        if (!magicField.isPresent())
            throw new IllegalStateException("Not magic field found for " + x.toString());
        Expr magicExpr = generateMagicExpr(magicSig, magicField.get(), x);
        if (booleanVarsConjunction != null) {
            variabilizationExpression = ExprITE.make(null, booleanVarsConjunction, magicExpr, Sig.PrimSig.NONE);
        } else {
            variabilizationExpression = magicExpr;
        }
        return variabilizationExpression;
    }

    private Expr generateBooleanVarsConjunction(List<Expr> vars) {
        Expr conjuntion = null;
        for (Expr v : vars) {
            if (!v.type().is_bool)
                throw new IllegalArgumentException("Non boolean variable found " + v.toString());
            if (conjuntion == null)
                conjuntion = v;
            else
                conjuntion = ExprBinary.Op.AND.make(null, null, (Expr) v.clone(), (Expr) conjuntion.clone());
        }
        return conjuntion;
    }

    private Expr generateMagicExpr(Sig magicSig, Field magicField, Expr x) {
        Sig sig = (Sig) magicSig.clone();
        Field field = (Field) magicField.fullCopy();
        Expr magicExpr = ExprBinary.Op.JOIN.make(sig.span().merge(field.span()), null, sig, field);
        Optional<List<Expr>> nonBooleanVariabilizationVars = getNonBooleanVariablesFor(x);
        if (nonBooleanVariabilizationVars.isPresent()) {
            List<Expr> reversedVariables = nonBooleanVariabilizationVars.get();
            Collections.reverse(reversedVariables);
            for (Expr vVar : reversedVariables) {
                magicExpr = ExprBinary.Op.JOIN.make(vVar.span().merge(magicExpr.span()), null, vVar, magicExpr);
            }
        }
        return magicExpr;
    }

    public void blockAllButCurrent(Candidate from) {
        if (from.isFirst() || from.isLast())
            throw new IllegalArgumentException("This method should be called with a candidate that is either first or last");
        Optional<List<Expr>> markedExpression = getMarkedExpressions(from.getContext());
        if (markedExpression.isPresent()) {
            int currentIndex = 1;
            for (Expr me : markedExpression.get()) {
                if (currentIndex == from.getCurrentMarkedExpression()) {
                    me.unblockMutations();
                } else {
                    me.blockMutations();
                }
                currentIndex++;
            }
        }
    }

    public Optional<List<Expr>> getMarkedExpressions(CompModule ast) {
        MarkedExpressionsCollector markedExpressionsCollector = new MarkedExpressionsCollector(ast);
        return markedExpressionsCollector.getMarkedExpressions();
    }

    public Optional<List<Pair<Expr, Boolean>>> getMarkedExpressionsForVariabilizationCheck(Candidate from) {
        MarkedExpressionsCollector markedExpressionsCollector = new MarkedExpressionsCollector(from);
        Optional<List<Expr>> markedExpressions = markedExpressionsCollector.getMarkedExpressions();
        List<Pair<Expr, Boolean>> result = new LinkedList<>();
        if (markedExpressions.isPresent()) {
            int idx = 1;
            for (Expr me : markedExpressions.get()) {
                Boolean toReplace;
                if (from.isIndexBlocked(idx)) {
                  toReplace = Boolean.FALSE;
                } else if (idx > from.getCurrentMarkedExpression()) {
                    toReplace = Boolean.TRUE;
                } else {
                    toReplace = Boolean.FALSE;
                }
                result.add(new Pair<>(me, toReplace));
                idx++;
            }
        }
        return result.isEmpty()?Optional.empty():Optional.of(result);
    }

    private static final String VARIABILIZATION_SIG_PREFIX = "QF_";
    private static final String VARIABILIZATION_FIELD_PREFIX = "magic_";

    public Optional<Sig> generateMagicSigForExpressions(List<Pair<Expr, Boolean>> expressions) {
        if (expressions.isEmpty())
            return Optional.empty();
        boolean atLeastOneUnblockedExpression = false;
        String sigName = VARIABILIZATION_SIG_PREFIX + generateRandomName(6);
        Sig varSig = new Sig.PrimSig(sigName, Attr.ONE);
        for (Pair<Expr, Boolean> me : expressions) {
            if (!me.b)
                continue;
            Expr x = me.a;
            Optional<Expr> fieldBound = generateMagicFieldBound(x);
            if (fieldBound.isPresent()) {
                String fieldName = VARIABILIZATION_FIELD_PREFIX + x.getID() + "_" + generateRandomName(3);
                atLeastOneUnblockedExpression = true;
                varSig.addField(fieldName, fieldBound.get());
            }
        }
        return atLeastOneUnblockedExpression?Optional.of(varSig):Optional.empty();
    }

    private Optional<Expr> generateMagicFieldBound(Expr x) {
        Expr current = generateMagicFieldLastBound(x);
        if (x.getVariabilizationVariables().isPresent()) {
            List<Expr> variabilizationVariables = getVariabilizationVariables(x);
            for (Expr vVar : variabilizationVariables) {
                if (vVar.type().is_bool)
                    continue;
                Expr vVarBound = generateMagicFieldLastBound(vVar, true);
                current = mergeIntoArrowExpression(vVarBound, current);
            }
        }
        return Optional.of(current);
    }

    private Optional<List<Expr>> getBooleanVariablesFor(Expr x) {
        return getVariabilizationVariablesFor(x, true);
    }

    private Optional<List<Expr>> getNonBooleanVariablesFor(Expr x) {
        return getVariabilizationVariablesFor(x, false);
    }

    private Optional<List<Expr>> getVariabilizationVariablesFor(Expr x, boolean bool) {
        if (!x.getVariabilizationVariables().isPresent())
            return Optional.empty();
        List<Expr> vars = new LinkedList<>();
        for (Expr vVar : getVariabilizationVariables(x)) {
            if (vVar.type().is_bool == bool)
                vars.add(vVar);
        }
        if (!vars.isEmpty())
            return Optional.of(vars);
        return Optional.empty();
    }

    private boolean hasBooleanVars(Expr x) {
        if (!x.getVariabilizationVariables().isPresent())
            return false;
        for (Expr vVar : getVariabilizationVariables(x)) {
            if (vVar.type().is_bool)
                return true;
        }
        return false;
    }

    private Expr mergeIntoArrowExpression(Expr a, Expr b) {
        String left = getArrowSubOp(a);
        String right = getArrowSubOp(b);
        Expr leftExpression = removeMultSubExpression(a);
        Expr rightExpression = removeMultSubExpression(b);
        ExprBinary.Op arrowOp = getArrowOp(left + "->" + right);
        if (arrowOp == null)
            throw new IllegalStateException("No arrow operator found for label " + left + "->" + right);
        return arrowOp.make(null, null, leftExpression, rightExpression);
    }

    private ExprBinary.Op getArrowOp(String label) {
        for (ExprBinary.Op op : ExprBinary.Op.values()) {
            if (op.toString().compareTo(label) == 0)
                return op;
        }
        return null;
    }

    private String getArrowSubOp(Expr x) {
        if (x instanceof ExprUnary) {
            switch (((ExprUnary)x).op) {
                case SOMEOF: return "some";
                case LONEOF: return "lone";
                case ONEOF: return "one";
            }
        }
        return "";
    }

    private Expr removeMultSubExpression(Expr x) {
        if (! (x instanceof ExprUnary))
            return x;
        ExprUnary expression = (ExprUnary) x;
        if (expression.op.equals(ExprUnary.Op.NOOP))
            return removeMultSubExpression(expression.sub);
        return expression.sub;
    }

    private List<Expr> getVariabilizationVariables(Expr x) {
        List<Expr> vVars = new LinkedList<>();
        if (x.getVariabilizationVariables().isPresent()) {
            for (String vVarId : x.getVariabilizationVariables().get()) {
                ContextExpressionExtractor.getLocalVariables(x).ifPresent(lVars -> {
                    boolean varFound = false;
                    for (Expr localVar : lVars) {
                        if (localVar.toString().compareTo(vVarId) == 0) {
                            varFound = true;
                            vVars.add(localVar);
                            break;
                        }
                    }
                    if (!varFound)
                        throw new IllegalStateException("Expression " + x.toString() + " has " + vVarId + " as variabilization related variable, but there is no local variable with that name");
                });
            }
        }
        return vVars;
    }

    private Expr generateMagicFieldLastBound(Expr x) {
        return generateMagicFieldLastBound(x,  false);
    }

    private Expr univ() {
        Expr univ = (Expr) Sig.PrimSig.UNIV.clone();
        univ.newID();
        return univ;
    }

    private Expr sigint() {
        Expr siginit = (Expr) Sig.PrimSig.SIGINT.clone();
        siginit.newID();
        return siginit;
    }

    private Expr generateMagicFieldLastBound(Expr x, boolean noSet) {
        if (x.type().is_int() || x.type().is_small_int()) {
            Expr intSig = sigint();
            intSig.newID();
            return noSet?intSig:ExprUnary.Op.SETOF.make(null, intSig);
        }
        if (x.type().arity() == 1 && !x.type().is_bool) {
            if (noSet) {
                Expr univSig = univ();
                univSig.newID();
                return univSig;
            }
            return ExprUnary.Op.SETOF.make(null, univ());
        } else if (x.type().is_bool) {
            return ExprUnary.Op.LONEOF.make(null, univ());
        } else {
            Expr current = univ();
            for (int i = 1; i < x.type().arity(); i++) {
                current = ExprBinary.Op.ARROW.make(null, null, current, univ());
            }
            return current;
        }
    }

    public Optional<Field> getMarkedExpressionReplacement(Sig varSig, Expr x) {
        if (!varSig.label.startsWith(VARIABILIZATION_SIG_PREFIX))
            return Optional.empty();
        int exprId = x.getID();
        for (Field f : varSig.getFields()) {
            String label = f.label;
            if (!label.startsWith(VARIABILIZATION_FIELD_PREFIX))
                continue;
            String[] labelParts = label.split("_");
            if (labelParts.length != 3)
                continue;
            if (Integer.toString(exprId).compareTo(labelParts[1]) == 0)
                return Optional.of(f);
        }
        return Optional.empty();
    }

    private static final String SYMBOLS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    private String generateRandomName(int length) {
        StringBuilder sb = new StringBuilder();
        Random rng = new Random();
        for (int i = 0; i < length; i++)
            sb.append(SYMBOLS.charAt(rng.nextInt(SYMBOLS.length())));
        return sb.toString();
    }

}
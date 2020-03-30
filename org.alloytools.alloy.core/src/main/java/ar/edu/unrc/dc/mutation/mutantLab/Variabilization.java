package ar.edu.unrc.dc.mutation.mutantLab;

import ar.edu.unrc.dc.mutation.CheatingIsBadMkay;
import ar.edu.unrc.dc.mutation.Cheats;
import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.Ops;
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

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.Random;
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

    private A4Reporter reporter;
    private A4Options options;

    private Variabilization(A4Reporter reporter, A4Options options) {
        if (options == null)
            throw new IllegalArgumentException("options can't be null");
        this.reporter = reporter;
        this.options = options;
    }

//    public void searchAndPrintMarkedExpressions(Candidate from, Logger logger) {
//        MarkedExpressionsCollector markedExpressionsCollector = new MarkedExpressionsCollector(from);
//        Optional<Mutation> lastMutation = from.getLastMutation();
//        StringBuilder sb = new StringBuilder("Marked expressions:\n");
//        markedExpressionsCollector.getMarkedExpressions().ifPresent(mes -> mes.forEach(me -> {
//            sb.append("Line ").append(me.pos.y).append(" : ").append(me.toString());
//            if (lastMutation.isPresent()) {
//                Optional<Expr> lastMutationRelatedMarkedExpression = lastMutation.get().original().getMarkedExpression();
//                if (lastMutationRelatedMarkedExpression.isPresent() && lastMutationRelatedMarkedExpression.get().getID() == me.getID()) {
//                    sb.append(" (current)");
//                } else if (lastMutation.get().mutant().getID() == me.getID()) {
//                    sb.append(" (current)");
//                } else if (!lastMutationRelatedMarkedExpression.isPresent()) {
//                    sb.append("ERROR: candidate has a mutation but does not have an associated marked expression");
//                }
//            }
//            sb.append("\n");
//        }));
//        logger.info(sb.toString());
//    }

//    public void printVariabilizationProcess(Candidate from, List<Command> commands, Logger logger) {
//        StringBuilder sb = new StringBuilder("VARIABILIZATION\n");
//        Optional<List<Pair<Expr, Boolean>>> expressionsForVariabilization = getMarkedExpressionsForVariabilizationCheck(from);
//        sb.append("Marked expressions, fixed and to replace:\n");
//        if (expressionsForVariabilization.isPresent()) {
//            for (Pair<Expr, Boolean> me : expressionsForVariabilization.get()) {
//                sb.append("Line ").append(me.a.pos.y).append(" : ").append(me.a.toString());
//                sb.append(me.b?" (TO REPLACE)":" (FIXED)");
//                sb.append("\n");
//            }
//            sb.append("Variabilization related sig:\n");
//            Optional<Sig> varSig = generateMagicSigForExpressions(expressionsForVariabilization.get());
//            if (varSig.isPresent()) {
//                sb.append(varSig.get().toExtendedString());
//                sb.append("\n");
//                sb.append("Marked expressions related fields (magic vars):\n");
//                for (Pair<Expr, Boolean> me : expressionsForVariabilization.get()) {
//                    if (!me.b)
//                        continue;
//                    Expr x = me.a;
//                    Optional<Field> magicVar = getMarkedExpressionReplacement(varSig.get(), x);
//                    sb.append("Magic var for expression : ").append(x.toString()).append(" : ");
//                    if (magicVar.isPresent()) {
//                        magicVar.get().toString(sb, 1);
//                    } else {
//                        sb.append("NOT FOUND (THIS SHOULD NOT HAPPEN!)");
//                    }
//                }
//                sb.append("Variabilization tests\n");
//                for (Command c : commands) {
//                    if (c.isVariabilizationTest()) {
//                        sb.append(c.toString());
//                        sb.append("\n");
//                    }
//                }
//            } else {
//                sb.append("NO SIG GENERATED (THIS SHOULD NOT HAPPEN!)");
//            }
//        } else {
//            sb.append("NO EXPRESSIONS FOUND\n");
//        }
//        logger.info(sb.toString());
//    }

//    public boolean variabilizationCheck(Candidate from, List<Command> commands, Logger logger) throws Err {
//        if (from == null)
//            throw new IllegalArgumentException("from can't be null");
//        if (from.isLast())
//            throw new IllegalArgumentException("from candidate can't be a last candidate");
//        if (commands == null)
//            throw new IllegalArgumentException("commands can't be null");
//        logger.info("Variabilization check for:\n" + from.toString() + "Using commands: [" + commands.stream().map(Command::toString).collect(Collectors.joining(",")) + "]");
//        if (commands.isEmpty())
//            return true;
//        Optional<List<Pair<Expr, Boolean>>> markedExpressions = getMarkedExpressionsForVariabilizationCheck(from);
//        if (markedExpressions.isPresent()) {
//            Optional<Sig> magicSig = generateMagicSigForExpressions(markedExpressions.get());
//            if (magicSig.isPresent()) {
//                RepairReport.getInstance().incVariabilizationChecks();
////                MutantLab.getInstance().lockCandidateGeneration();
//                List<Mutation> variabilizationMutations = generateVariabilizationMutations(magicSig.get(), markedExpressions.get());
//                Candidate variabilizationCandidate = generateCandidateForVariabilization(from.getContext(), variabilizationMutations);
//                logger.info("Reporter available: " + (reporter != null));
//                logger.info("variabilization candidate:\n" + variabilizationCandidate.toString());
//                prepareAstForVariabilization(from, magicSig.get());
//                boolean solverResult = runSolver(from.getContext(), commands, variabilizationCandidate);
//                logger.info("solver returned: " + solverResult + "\n");
//                restoreAst(from, magicSig.get());
////                MutantLab.getInstance().unlockCandidateGeneration();
//                if (solverResult)
//                    RepairReport.getInstance().incVariabilizationChecksPassed();
//                else
//                    RepairReport.getInstance().incVariabilizationChecksFailed();
//                return solverResult;
//            }
//        }
//        return true;
//    }

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
        if (markedExpressions.isPresent()) {
            CompModule context = from.getContext();
            Optional<Sig> magicSig = generateMagicSigForExpressions(markedExpressions.get());
            if (magicSig.isPresent()) {
                RepairReport.getInstance().incVariabilizationChecks();
                List<Mutation> variabilizationMutations = generateVariabilizationMutations(magicSig.get(), markedExpressions.get());
                Candidate variabilizationCandidate = generateCandidateForVariabilization(from.getContext(), variabilizationMutations);
                logger.info("Reporter available: " + (reporter != null));
                logger.info("variabilization candidate:\n" + variabilizationCandidate.toString());
                try {
                    Cheats.addSigToModule(context, magicSig.get());
                } catch (CheatingIsBadMkay e) {
                    throw new Error("There was a problem while adding the magic signature to the module", e);
                }
                boolean solverResult = runSolver(from.getContext(), commands, variabilizationCandidate);
                logger.info("solver returned: " + solverResult + "\n");
                restoreAst(from, magicSig.get());
                if (solverResult)
                    RepairReport.getInstance().incVariabilizationChecksPassed();
                else
                    RepairReport.getInstance().incVariabilizationChecksFailed(from.getCurrentMarkedExpression());
                return solverResult;
            }
        }
        return true;
    }

    private Candidate generateCandidateForVariabilization(CompModule module, List<Mutation> variabilizationMutations) {
        Candidate variabilizationCandidate = Candidate.original(module);
        for (Mutation m : variabilizationMutations) {
            variabilizationCandidate = Candidate.mutantFromCandidate(variabilizationCandidate, m);
        }
        return variabilizationCandidate;
    }

    private void restoreAst(Candidate from, Sig magicSig) throws Err {
        CompModule context = from.getContext();
        try {
            Cheats.removeSigFromModule(context, magicSig);
            if (!ASTMutator.getInstance().undoMutations()) {
                throw new Error("There was a problem while mutating the ast");
            }
        } catch (CheatingIsBadMkay e) {
            throw new Error("There was a problem while removing the magic signature to the module", e);
        }
    }

    private boolean runSolver(CompModule module, List<Command> commands, Candidate variabilizationCandidate) {
        boolean repaired = true;
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
            if (!repaired) break;
        }
        return repaired;
    }

    private void prepareAstForVariabilization(Candidate from, Sig magicSig) throws Err {
        CompModule context = from.getContext();
        try {
            Cheats.addSigToModule(context, magicSig);
            ASTMutator astMutator = ASTMutator.getInstance();
            from.getMutations().forEach(astMutator::pushNewMutation);
            if (!astMutator.applyMutations()) {
                throw new Error("There was a problem while mutating the ast");
            }
        } catch (CheatingIsBadMkay e) {
            throw new Error("There was a problem while adding the magic signature to the module", e);
        }
    }

    private List<Mutation> generateVariabilizationMutations(Sig magicSig, List<Pair<Expr, Boolean>> markedExpressions) throws Err {
        if (markedExpressions.stream().noneMatch(p -> p.b))
            throw new IllegalStateException("No marked expressions available to generate variabilization mutations");
        List<Mutation> variabilizationMutations = new LinkedList<>();
        for (Pair<Expr, Boolean> me : markedExpressions) {
            if (!me.b) {
                continue;
            }
            Optional<Field> magicField = getMarkedExpressionReplacement(magicSig, me.a);
            if (!magicField.isPresent())
                throw new IllegalStateException("Not magic field found for " + me.a.toString());
            Sig sig = (Sig) magicSig.clone();
            Field field = (Field) magicField.get().clone();
            Expr magicExpr = ExprBinary.Op.JOIN.make(sig.span().merge(field.span()), null, sig, field);
            Mutation varMut = new Mutation(Ops.VAR, me.a, magicExpr);
            variabilizationMutations.add(varMut);
        }
        return variabilizationMutations;
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

//    public Optional<List<Expr>> getMarkedExpressions(Candidate from) {
//        MarkedExpressionsCollector markedExpressionsCollector = new MarkedExpressionsCollector(from);
//        Optional<Mutation> lastMutation = from.getLastMutation();
//        Optional<List<Expr>> markedExpressions = markedExpressionsCollector.getMarkedExpressions();
//        List<Expr> result = new LinkedList<>();
//        if (markedExpressions.isPresent()) {
//            for (Expr me : markedExpressions.get()) {
//                if (lastMutation.isPresent()) {
//                    Optional<Expr> lastMutationRelatedMarkedExpression = lastMutation.get().original().getMarkedExpression();
//                    Expr markedExpression = me;
//                    if (lastMutationRelatedMarkedExpression.isPresent() && lastMutationRelatedMarkedExpression.get().getID() == me.getID()) {
//                        markedExpression = lastMutationRelatedMarkedExpression.get();
//                    } else if (lastMutation.get().mutant().getID() == me.getID()) {
//                        markedExpression = lastMutation.get().mutant();
//                    }
//                    result.add(markedExpression);
//                }
//            }
//        }
//        return result.isEmpty()?Optional.empty():Optional.of(result);
//    }

    public Optional<List<Expr>> getMarkedExpressions(CompModule ast) {
        MarkedExpressionsCollector markedExpressionsCollector = new MarkedExpressionsCollector(ast);
        return markedExpressionsCollector.getMarkedExpressions();
    }

//    public Optional<List<Pair<Expr, Boolean>>> getMarkedExpressionsForVariabilizationCheck(Candidate from) {
//        return getMarkedExpressionsForVariabilizationCheck(from, false);
//    }

//    public Optional<List<Pair<Expr, Boolean>>> getMarkedExpressionsForVariabilizationCheck(Candidate from) {
//        MarkedExpressionsCollector markedExpressionsCollector = new MarkedExpressionsCollector(from);
//        Optional<Mutation> lastMutation = from.getLastMutation();
//        Optional<List<Expr>> markedExpressions = markedExpressionsCollector.getMarkedExpressions();
//        List<Pair<Expr, Boolean>> result = new LinkedList<>();
//        if (markedExpressions.isPresent()) {
//            int idx = 1;
//            for (Expr me : markedExpressions.get()) {
//                if (!from.isFirst() && from.getCurrentMarkedExpression() >= idx) {
//                    idx++;
//                    continue;
//                }
//                Boolean toReplace;
//                Expr markedExpression = me;
//                if (lastMutation.isPresent()) {
//                    Optional<Expr> lastMutationRelatedMarkedExpression = lastMutation.get().original().getMarkedExpression();
//                    if (lastMutationRelatedMarkedExpression.isPresent() && lastMutationRelatedMarkedExpression.get().getID() == me.getID()) {
//                        markedExpression = lastMutationRelatedMarkedExpression.get();
//                        toReplace = Boolean.FALSE;
//                    } else if (lastMutation.get().mutant().getID() == me.getID()) {
//                        markedExpression = lastMutation.get().mutant();
//                        toReplace = Boolean.FALSE;
//                    } else {
//                        toReplace = Boolean.TRUE;
//                    }
//                } else {
//                    toReplace = Boolean.TRUE;
//                }
//                result.add(new Pair<>(markedExpression, toReplace));
//            }
//        }
//        return result.isEmpty()?Optional.empty():Optional.of(result);
//    }

    public Optional<List<Pair<Expr, Boolean>>> getMarkedExpressionsForVariabilizationCheck(Candidate from) {
        MarkedExpressionsCollector markedExpressionsCollector = new MarkedExpressionsCollector(from);
        Optional<List<Expr>> markedExpressions = markedExpressionsCollector.getMarkedExpressions();
        List<Pair<Expr, Boolean>> result = new LinkedList<>();
        if (markedExpressions.isPresent()) {
            int idx = 1;
            for (Expr me : markedExpressions.get()) {
                Boolean toReplace;
                if (idx > from.getCurrentMarkedExpression())
                    toReplace = Boolean.TRUE;
                else
                    toReplace = Boolean.FALSE;
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
        String sigName = VARIABILIZATION_SIG_PREFIX + generateRandomName(6);
        Sig varSig = new Sig.PrimSig(sigName, Attr.ONE);
        for (Pair<Expr, Boolean> me : expressions) {
            if (!me.b)
                continue;
            Expr x = me.a;
            String fieldName = VARIABILIZATION_FIELD_PREFIX + x.getID() + "_" + generateRandomName(3);
            Expr fieldBound = generateMagicFieldBound(x);
            varSig.addField(fieldName, fieldBound);
        }
        return Optional.of(varSig);
    }

    private Expr generateMagicFieldBound(Expr x) {
        if (x.type().arity() == 1)
            return ExprUnary.Op.SETOF.make(null, Sig.UNIV);
        else {
            Expr current = Sig.UNIV;
            for (int i = 1; i < x.type().arity(); i++) {
                current = ExprBinary.Op.ARROW.make(null, null, current, Sig.UNIV);
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
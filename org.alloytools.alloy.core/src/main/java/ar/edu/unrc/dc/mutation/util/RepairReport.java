package ar.edu.unrc.dc.mutation.util;

import ar.edu.unrc.dc.mutation.MutationConfiguration;
import ar.edu.unrc.dc.mutation.MutationConfiguration.ConfigKey;
import ar.edu.unrc.dc.mutation.mutantLab.Candidate;
import ar.edu.unrc.dc.mutation.mutantLab.MutantLab;
import ar.edu.unrc.dc.mutation.mutantLab.Pruning;
import ar.edu.unrc.dc.mutation.visitors.ExprToString;
import edu.mit.csail.sdg.ast.Command;
import edu.mit.csail.sdg.ast.Expr;

import java.util.*;
import java.util.concurrent.TimeUnit;

public class RepairReport {

    private static RepairReport instance;
    public synchronized static RepairReport getInstance() {
        if (instance == null)
            instance = new RepairReport();
        return instance;
    }

    public synchronized static void destroyInstance() {
        if (instance == null)
            throw new IllegalStateException("RepairReport not initialized");
        instance = null;
    }

    private final int mutationDepth;

    private boolean mutantGeneration = false;
    private int examinedCandidates;
    private int generatedCandidates;
    private int invalidCandidates;
    private int repeatedCandidates;
    private int irrelevantMutationsSkipped;
    private int variabilizationChecks;
    private int variabilizationChecksPassed;
    private int variabilizationChecksFailed;
    private int[][] variabilizationChecksFailedPerIndexPerDepth;
    private int partialPruningChecks;
    private int partialPruningChecksPassed;
    private int partialPruningChecksFailed;
    private int[][] partialPruningChecksFailedPerIndexPerDepth;
    private int[] maxReachedDepthPerIndex;

    private int markedExpressions;
    private int commands;
    private int variabilizationRelatedCommands;

    private long time = 0; //used to calculate the whole repair process time
    private long timeSpentInVariabilization = 0;
    private long timeSpentInGeneration = 0;
    private long timeSpentInValidation = 0;
    private long timeSpentInTestGeneration = 0;

    private Map<Integer, int[]> mutationsPerIndex; //(index, generations, mutants)
    private int[] averageMutationsPerIndex;
    private int totalMutations;
    private int generationCount;
    private int averageMutations;
    private boolean repairEnded;

    private Candidate repair;
    private final List<String> initialMarkedExpressions;

    private RepairReport() {
        repairEnded = false;
        examinedCandidates = 0;
        generatedCandidates = 0;
        invalidCandidates = 0;
        repeatedCandidates = 0;
        irrelevantMutationsSkipped = 0;
        variabilizationChecks = 0;
        variabilizationChecksPassed = 0;
        variabilizationChecksFailed = 0;
        partialPruningChecks = 0;
        partialPruningChecksPassed = 0;
        partialPruningChecksFailed = 0;
        markedExpressions = -1;
        commands = -1;
        variabilizationRelatedCommands = -1;
        repair = null;
        initialMarkedExpressions = new LinkedList<>();
        totalMutations = 0;
        generationCount = 0;
        mutationDepth = (int) MutationConfiguration.getInstance().getConfigValue(ConfigKey.REPAIR_MAX_DEPTH).orElse(ConfigKey.REPAIR_MAX_DEPTH.defaultValue());
    }

    public void setAsMutantGenerationRun() {
        mutantGeneration = true;
    }

    public void incExaminedCandidates() {
        examinedCandidates++;
    }

    public void incGeneratedCandidates() {
        generatedCandidates++;
    }

    public void incRepeatedCandidates() {repeatedCandidates++;}

    public void incInvalidCandidates() {
        invalidCandidates++;
    }

    public void incIrrelevantMutationsSkipped() { irrelevantMutationsSkipped++; }

    public void updateMaxReachedDepth(int index, int depth) {
        if (index <= 0 || index > markedExpressions)
            throw new IllegalArgumentException("index must be between 1 and " + (markedExpressions) + " (" + index + ")");
        if (depth < 0 || depth > mutationDepth)
            throw new IllegalArgumentException("depth must be a number between 0 and " + mutationDepth + " (" + depth + ")");
        maxReachedDepthPerIndex[index] = Math.max(maxReachedDepthPerIndex[index], depth);
    }

    public void incVariabilizationChecks() {
        variabilizationChecks++;
    }

    public void incPartialPruningChecks() {
        partialPruningChecks++;
    }

    public void incVariabilizationChecksPassed() {
        variabilizationChecksPassed++;
    }

    public void incPartialPruningChecksPassed() {
        partialPruningChecksPassed++;
    }

    public void incVariabilizationChecksFailed(int index, int depth) {
        if (index < 0 || index > markedExpressions)
            throw new IllegalArgumentException("index must be between 0 and " + (markedExpressions) + " (" + index + ")");
        if (depth < 0 || depth > mutationDepth)
            throw new IllegalArgumentException("depth must be a number between 0 and " + mutationDepth + " (" + depth + ")");
        variabilizationChecksFailedPerIndexPerDepth[index][depth]++;
        variabilizationChecksFailed++;
    }

    public void incPartialPruningChecksFailed(int index, int depth) {
        if (index < 0 || index > markedExpressions)
            throw new IllegalArgumentException("index must be between 0 and " + (markedExpressions) + " (" + index + ")");
        if (depth < 0 || depth > mutationDepth)
            throw new IllegalArgumentException("depth must be a number between 0 and " + mutationDepth + " (" + depth + ")");
        partialPruningChecksFailedPerIndexPerDepth[index][depth]++;
        partialPruningChecksFailed++;
    }

    public void setMarkedExpressions(int markedExpressions) {
        if (this.markedExpressions != -1)
            throw new IllegalStateException("markedExpressions value already set");
        if (markedExpressions < 0)
            throw new IllegalArgumentException("markedExpressions must be a positive (+0) value");
        this.markedExpressions = markedExpressions;
        mutationsPerIndex = new HashMap<>();
        for (int i = 1; i <= markedExpressions; i++) {
            mutationsPerIndex.put(i, new int[] {0, 0});
        }
        variabilizationChecksFailedPerIndexPerDepth = new int[markedExpressions + 1][mutationDepth + 1];
        for (int[] ints : variabilizationChecksFailedPerIndexPerDepth) {
            Arrays.fill(ints, 0);
        }
        partialPruningChecksFailedPerIndexPerDepth = new int[markedExpressions + 1][mutationDepth + 1];
        for (int[] ints : partialPruningChecksFailedPerIndexPerDepth) {
            Arrays.fill(ints, 0);
        }
        maxReachedDepthPerIndex = new int[markedExpressions + 1];
        Arrays.fill(maxReachedDepthPerIndex, 0);
    }

    public void setCommands(int commands) {
        if (this.commands != -1)
            throw new IllegalStateException("commands value already set");
        if (commands < 0)
            throw new IllegalArgumentException("commands must be a positive (+0) value");
        this.commands = commands;
    }


    public void setVariabilizationRelatedCommands(int variabilizationRelatedCommands) {
        if (this.variabilizationRelatedCommands != -1)
            throw new IllegalStateException("variabilizationRelatedCommands value already set");
        if (variabilizationRelatedCommands < 0)
            throw new IllegalArgumentException("variabilizationRelatedCommands must be a positive (+0) value");
        this.variabilizationRelatedCommands = variabilizationRelatedCommands;
    }

    public void setRepair(Candidate repair) {
        if (this.repair != null)
            throw new IllegalStateException("repair value already set");
        if (repair == null)
            throw new IllegalArgumentException("repair can't be null");
        this.repair = repair;
    }

    public void addMarkedExpression(Expr x) {
        initialMarkedExpressions.add(x.toString());
    }

    public void addMutations(int mutations, int index) {
        if (index < 1 || index > markedExpressions)
            throw new IllegalArgumentException("index must be between 1 and " + markedExpressions + " (" + index + ")");
        if (mutations < 0)
            throw new IllegalArgumentException("mutations can't be a negative value (" + mutations + ")");
        int[] avgMuts = mutationsPerIndex.get(index);
        if (avgMuts == null)
            throw new IllegalStateException("No average mutations information available for valid index (" + index + ")");
        avgMuts[1] += mutations;
        totalMutations += mutations;

    }

    public void incGenerations(int index) {
        if (index < 1 || index > markedExpressions)
            throw new IllegalArgumentException("index must be between 1 and " + markedExpressions + " (" + index + ")");
        int[] avgMuts = mutationsPerIndex.get(index);
        if (avgMuts == null)
            throw new IllegalStateException("No average mutations information available for valid index (" + index + ")");
        avgMuts[0]++;
        generationCount++;
    }

    private void calculateAvgMutations() {
        averageMutations = generationCount!=0?(totalMutations / generationCount):0;
        averageMutationsPerIndex = new int[markedExpressions];
        for (int i = 1; i <= markedExpressions; i++) {
            int[] avgMuts = mutationsPerIndex.get(i);
            if (avgMuts == null)
                throw new IllegalStateException("No average mutations information available for valid index (" + i + ")");
            averageMutationsPerIndex[i-1] = avgMuts[0] == 0? 0: avgMuts[1] / avgMuts[0];
        }
    }

    public void clockStart() {
        this.time=System.nanoTime();
    }

    public void clockEnd() {
        repairEnded = true;
        this.time=System.nanoTime()-this.time;
    }

    private long variabilizationStartTime;
    public void variabilizationClockStart() {
        variabilizationStartTime = System.nanoTime();
    }

    public void variabilizationClockEnd() {
        long timeSpent = System.nanoTime() - variabilizationStartTime;
        timeSpentInVariabilization += timeSpent;
        variabilizationStartTime = 0;
    }

    private long generationStartTime;
    public void generationClockStart() {
        generationStartTime = System.nanoTime();
    }

    public void generationClockEnd() {
        long timeSpent = System.nanoTime() - generationStartTime;
        timeSpentInGeneration += timeSpent;
        generationStartTime = 0;
    }

    private long validationStartTime;
    public void validationClockStart() {
        validationStartTime = System.nanoTime();
    }

    public void validationClockEnd() {
        long timeSpent = System.nanoTime() - validationStartTime;
        timeSpentInValidation += timeSpent;
        validationStartTime = 0;
    }

    private long testGenerationStartTime;
    public void testGenerationClockStart() {
        testGenerationStartTime = System.nanoTime();
    }

    public void testGenerationClockEnd() {
        long timeSpent = System.nanoTime() - testGenerationStartTime;
        timeSpentInTestGeneration += timeSpent;
        testGenerationStartTime = 0;
    }

    public long getTime() {
        if (!repairEnded)
            throw new IllegalStateException("Repair process has not ended");
        return time;
    }

    private String repairRepresentation = null;
    private void generateRepairRepresentation() {
        if (repairRepresentation != null)
            return;
        if (repair == null)
            return;
        StringBuilder sb = new StringBuilder();
        for (Expr e : MutantLab.getInstance().getModifiedAssertionsFunctionsAndFacts(repair)) {
            sb.append("|ORIGINAL: ");
            ExprToString exprToString = new ExprToString(null, true);
            exprToString.visitThis(e);
            sb.append(exprToString.getStringRepresentation()).append("\n");
            sb.append("REPAIRED: ");
            exprToString = new ExprToString(repair, true);
            exprToString.visitThis(e);
            sb.append(exprToString.getStringRepresentation()).append("\n");
            sb.append("|\n");
        }
        sb.append("\n");
        repairRepresentation = sb.toString();
    }

    private void timesToMs() {
        time = TimeUnit.NANOSECONDS.toMillis(time);
        timeSpentInVariabilization = TimeUnit.NANOSECONDS.toMillis(timeSpentInVariabilization);
        timeSpentInValidation = TimeUnit.NANOSECONDS.toMillis(timeSpentInValidation);
        timeSpentInGeneration = TimeUnit.NANOSECONDS.toMillis(timeSpentInGeneration);
        timeSpentInTestGeneration = TimeUnit.NANOSECONDS.toMillis(timeSpentInTestGeneration);
    }

    @Override
    public String toString() {
        timesToMs();
        calculateAvgMutations();
        boolean variabilizationEnabledAndSupported = Pruning.getInstance().useVariabilization() && MutantLab.getInstance().isVariabilizationSupported();
        StringBuilder sb = new StringBuilder("***BeAFix report***\n");
        if (mutantGeneration) {
            sb.append("Mutants generation mode\n");
        } else {
            if (repair != null) {
                sb.append("REPAIR FOUND\n");
                generateRepairRepresentation();
                sb.append(repairRepresentation);
            } else {
                sb.append("REPAIR NOT FOUND\n");
            }
        }
        sb.append("Expressions marked to be mutated:").append("\t").append(markedExpressions).append("\n");
        sb.append(String.join("\n", initialMarkedExpressions));
        if (!initialMarkedExpressions.isEmpty())
            sb.append("\n");
        sb.append("\n");
        sb.append("Commands used:").append("\t").append(commands).append("\n");
        for (Command c : DependencyGraph.getInstance().getAllCommands()) {
            int complexity = DependencyGraph.getInstance().getCommandComplexity(c);
            boolean isVariabilizationTest = c.isVariabilizationTest();
            boolean isPerfectOracleTest = c.isPerfectOracleTest();
            boolean isGenerated = c.isGenerated();
            sb.append(c.toString()).append("\t").append(" complexity (").append(complexity).append(")");
            if (isGenerated)
                sb.append(" (generated)");
            if (isVariabilizationTest && variabilizationEnabledAndSupported)
                sb.append(" used as variabilization test");
            if (isPerfectOracleTest)
                sb.append(" used as perfect oracle test");
            sb.append("\n");
        }
        sb.append("\n");
        sb.append("Variabilization related commands:").append("\t").append(variabilizationRelatedCommands).append("\n");
        sb.append("---Candidates---").append("\n");
        sb.append("Examined:").append("\t").append(examinedCandidates).append("\n");
        sb.append("Generated:").append("\t").append(generatedCandidates).append("\n");
        sb.append("Invalid:").append("\t").append(invalidCandidates).append("\n");
        sb.append("Repeated:").append("\t").append(repeatedCandidates).append("\n");
        sb.append("Irrelevant mutations skipped:").append("\t").append(irrelevantMutationsSkipped).append("\n");
        sb.append("Average mutations per generation:").append("\t").append(averageMutations).append("\n");
        sb.append("Average mutations per generation, per marked expression:").append("\t").append(Arrays.toString(averageMutationsPerIndex)).append("\n");
        sb.append("\n");
        sb.append("---Pruning---").append("\n");
        sb.append("---Variabilization---").append("\n");
        sb.append("\t\tTotal checks:").append("\t").append(variabilizationChecks).append("\n");
        sb.append("\t\tChecks passed:").append("\t").append(variabilizationChecksPassed).append("\n");
        sb.append("\t\tChecks failed:").append("\t").append(variabilizationChecksFailed).append("\n");
        sb.append("\t\tChecks failed by index and depth:").append("\n");
        sb.append("\t\t\t");
        for (int i = 0; i <= markedExpressions; i++) {
            sb.append(i).append("\t\t");
        }
        sb.append("\n");
        for (int d = 0; d <= mutationDepth; d++) {
            sb.append("\t\t").append(d).append("\t");
            for (int i = 0; i <= markedExpressions; i++) {
                sb.append(variabilizationChecksFailedPerIndexPerDepth[i][d]).append("\t\t");
            }
            sb.append("\n");
        }
        sb.append("---Partial Pruning---").append("\n");
        sb.append("\t\tTotal checks:").append("\t").append(partialPruningChecks).append("\n");
        sb.append("\t\tChecks passed:").append("\t").append(partialPruningChecksPassed).append("\n");
        sb.append("\t\tChecks failed:").append("\t").append(partialPruningChecksFailed).append("\n");
        sb.append("\t\tChecks failed by index and depth:").append("\n");
        sb.append("\t\t\t");
        for (int i = 0; i <= markedExpressions; i++) {
            sb.append(i).append("\t\t");
        }
        sb.append("\n");
        for (int d = 0; d <= mutationDepth; d++) {
            sb.append("\t\t").append(d).append("\t");
            for (int i = 0; i <= markedExpressions; i++) {
                sb.append(partialPruningChecksFailedPerIndexPerDepth[i][d]).append("\t\t");
            }
            sb.append("\n");
        }
        sb.append("---Times---").append("\n");
        sb.append("Time spent in pruning:        ").append("\t").append(timeSpentInVariabilization).append("ms");
        sb.append(" (").append(percentage(timeSpentInVariabilization, time)).append("%)").append("\n");
        sb.append("Time spent in generation:     ").append("\t").append(timeSpentInGeneration).append("ms");
        sb.append(" (").append(percentage(timeSpentInGeneration, time)).append("%)").append("\n");
        sb.append("Time spent in validation:     ").append("\t").append(timeSpentInValidation).append("ms");
        sb.append(" (").append(percentage(timeSpentInValidation, time)).append("%)").append("\n");
        sb.append("Time spent in test generation:").append("\t").append(timeSpentInTestGeneration).append("ms");
        sb.append(" (").append(percentage(timeSpentInTestGeneration, time)).append("%)").append("\n");
        sb.append("\n");
        sb.append("Total time :").append("\t").append(time).append("ms");
        return sb.toString();
    }

    private double percentage(long value, long of) {
        return ((value * 100.0) / of);
    }

}

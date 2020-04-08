package ar.edu.unrc.dc.mutation.util;

import ar.edu.unrc.dc.mutation.MutationConfiguration;
import ar.edu.unrc.dc.mutation.MutationConfiguration.ConfigKey;
import ar.edu.unrc.dc.mutation.mutantLab.Candidate;
import ar.edu.unrc.dc.mutation.mutantLab.MutantLab;
import ar.edu.unrc.dc.mutation.visitors.ExprToString;
import edu.mit.csail.sdg.ast.Command;
import edu.mit.csail.sdg.ast.Expr;

import java.util.*;

public class RepairReport {

    private static RepairReport instance;
    public synchronized static RepairReport getInstance() {
        if (instance == null)
            instance = new RepairReport();
        return instance;
    }

    private int mutationDepth;

    private int examinedCandidates;
    private int generatedCandidates;
    private int invalidCandidates;
    private int variabilizationChecks;
    private int variabilizationChecksPassed;
    private int variabilizationChecksFailed;
    private int[] variabilizationChecksFailedPerIndex;

    private int markedExpressions;
    private int commands;
    private int variabilizationRelatedCommands;

    private long time =0; //used to calculate the whole repair process time

    private Map<Integer, int[]> mutationsPerIndex; //(index, generations, mutants)
    private int[] averageMutationsPerIndex;
    private int totalMutations;
    private int generationCount;
    private int averageMutations;

    private Candidate repair;
    private List<String> initialMarkedExpressions;

    private RepairReport() {
        examinedCandidates = 0;
        generatedCandidates = 0;
        invalidCandidates = 0;
        variabilizationChecks = 0;
        variabilizationChecksPassed = 0;
        variabilizationChecksFailed = 0;
        markedExpressions = -1;
        commands = -1;
        variabilizationRelatedCommands = -1;
        repair = null;
        initialMarkedExpressions = new LinkedList<>();
        totalMutations = 0;
        generationCount = 0;
        mutationDepth = (int) MutationConfiguration.getInstance().getConfigValue(ConfigKey.REPAIR_MAX_DEPTH).orElse(ConfigKey.REPAIR_MAX_DEPTH.defaultValue());
    }

    public void incExaminedCandidates() {
        examinedCandidates++;
    }

    public void incGeneratedCandidates() {
        generatedCandidates++;
    }

    public void incInvalidCandidates() {
        invalidCandidates++;
    }

    public void incVariabilizationChecks() {
        variabilizationChecks++;
    }

    public void incVariabilizationChecksPassed() {
        variabilizationChecksPassed++;
    }

    public void incVariabilizationChecksFailed(int index) {
        if (index < 0 || index >= markedExpressions)
            throw new IllegalArgumentException("index must be between 0 and " + (markedExpressions - 1) + " (" + index + ")");
        variabilizationChecksFailedPerIndex[index]++;
        variabilizationChecksFailed++;
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
        variabilizationChecksFailedPerIndex = new int[markedExpressions];
        Arrays.fill(variabilizationChecksFailedPerIndex, 0);
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
        avgMuts[0]++;
        avgMuts[1] += mutations;
        totalMutations += mutations;
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

    private int variabilizationEstimatedPrunedCandidates;
    private int maxIndexForEstimation;
    private void calculateMaxIndexForEstimation() {
        for (int i = 1; i <= markedExpressions; i++) {
            if (averageMutationsPerIndex[i-1] != 0)
                maxIndexForEstimation = i;
        }
    }
    private void calculateVariabilizationEstimatedPruning() {
        calculateMaxIndexForEstimation();
        int prunedCandidates = 0;
        for (int v = 0; v < variabilizationChecksFailedPerIndex.length; v++) {
            int variabilizationFails = variabilizationChecksFailedPerIndex[v];
            int oneVarPrunedCandidates = 0;
            if (variabilizationFails == 0)
                continue;
            for (int i = v + 1; i <= maxIndexForEstimation; i++) {
                if (oneVarPrunedCandidates == 0) {
                    oneVarPrunedCandidates = averageMutations * mutationDepth;
                } else {
                    oneVarPrunedCandidates *= averageMutations * mutationDepth;
                }
            }
            prunedCandidates += oneVarPrunedCandidates * variabilizationFails;
        }
        variabilizationEstimatedPrunedCandidates = prunedCandidates;
    }

    public void clockStart() {
        this.time=System.currentTimeMillis();
    }

    public void clockEnd() {
        this.time=System.currentTimeMillis()-this.time;
    }

    private String repairRepresentation = null;
    private void generateRepairRepresentation() {
        if (repairRepresentation != null)
            return;
        if (repair == null)
            return;
        StringBuilder sb = new StringBuilder();
        for (Expr e : MutantLab.getInstance().getModifiedAssertionsFunctionsAndFacts(repair)) {
            sb.append("Original:\n");
            ExprToString exprToString = new ExprToString(null, true);
            exprToString.visitThis(e);
            sb.append(exprToString.getStringRepresentation()).append("\n");
            sb.append("Repaired:\n");
            exprToString = new ExprToString(repair, true);
            exprToString.visitThis(e);
            sb.append(exprToString.getStringRepresentation()).append("\n");
            sb.append("\n");
        }
        sb.append("\n");
        repairRepresentation = sb.toString();
    }

    public String getRepairRepresentation() {
        if (repair == null) {
            throw new IllegalStateException("There was no repair found!");
        }
        generateRepairRepresentation();
        return repairRepresentation;
    }

    @Override
    public String toString() {
        calculateAvgMutations();
        calculateVariabilizationEstimatedPruning();
        StringBuilder sb = new StringBuilder("***AStryker report***\n");
        if (repair != null) {
            sb.append("REPAIR FOUND\n");
            generateRepairRepresentation();
            sb.append(repairRepresentation);
        } else {
            sb.append("REPAIR NOT FOUND\n");
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
            sb.append(c.toString()).append("\t").append(" complexity (").append(complexity).append(")");
            if (isVariabilizationTest)
                sb.append(" used as variabilization test");
            sb.append("\n");
        }
        sb.append("\n");
        sb.append("Variabilization related commands:").append("\t").append(variabilizationRelatedCommands).append("\n");
        sb.append("---Candidates---").append("\n");
        sb.append("Examined:").append("\t").append(examinedCandidates).append("\n");
        sb.append("Generated:").append("\t").append(generatedCandidates).append("\n");
        sb.append("Invalid:").append("\t").append(invalidCandidates).append("\n");
        sb.append("Average mutations per generation:").append("\t").append(averageMutations).append("\n");
        sb.append("Average mutations per generation, per marked expression:").append("\t").append(Arrays.toString(averageMutationsPerIndex)).append("\n");
        sb.append("\n");
        sb.append("---Variabilization---").append("\n");
        sb.append("Total checks:").append("\t").append(variabilizationChecks).append("\n");
        sb.append("Checks passed:").append("\t").append(variabilizationChecksPassed).append("\n");
        sb.append("Checks failed:").append("\t").append(variabilizationChecksFailed).append("\n");
        sb.append("Estimated candidates pruned:").append("\t").append(variabilizationEstimatedPrunedCandidates).append("\n");
        sb.append("\n");
        sb.append("Total time (ms): ").append(time);
        return sb.toString();
    }
}

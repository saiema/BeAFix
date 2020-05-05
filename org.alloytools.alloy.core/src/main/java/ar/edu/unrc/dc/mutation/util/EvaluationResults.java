package ar.edu.unrc.dc.mutation.util;

import edu.mit.csail.sdg.ast.Command;

import java.util.Map;

public final class EvaluationResults {

    private final boolean isPartialRepair;
    private final boolean repaired;
    private final boolean discarded;
    private final Exception exception;
    private final Map<Command, Boolean> commandResults; //only for partial repairs

    public static EvaluationResults normalEvaluationResults(boolean repaired, Exception exception) {
        return new EvaluationResults(repaired, repaired, exception != null, exception, null);
    }

    public static EvaluationResults partialRepairEvaluationResults(boolean repaired, boolean isPartialRepair, Exception exception, Map<Command, Boolean> commandResults) {
        return new EvaluationResults(isPartialRepair && exception == null, repaired, exception != null, exception, commandResults);
    }

    private EvaluationResults(boolean isPartialRepair, boolean repaired, boolean discarded, Exception exception, Map<Command, Boolean> commandResults) {
        this.isPartialRepair = isPartialRepair;
        this.repaired = repaired;
        this.discarded = discarded;
        this.exception = exception;
        this.commandResults = commandResults;
    }

    public boolean isPartialRepair() {
        return isPartialRepair;
    }

    public boolean isRepaired() {
        return repaired;
    }

    public boolean isDiscarded() {
        return discarded;
    }

    public Exception getException() {
        return exception;
    }

    public Map<Command, Boolean> getCommandResults() {
        return commandResults;
    }

}

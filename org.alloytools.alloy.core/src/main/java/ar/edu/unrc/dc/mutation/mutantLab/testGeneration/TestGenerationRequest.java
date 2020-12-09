package ar.edu.unrc.dc.mutation.mutantLab.testGeneration;

import edu.mit.csail.sdg.ast.Command;
import edu.mit.csail.sdg.parser.CompModule;
import edu.mit.csail.sdg.translator.A4Solution;

import java.util.LinkedList;
import java.util.List;

public class TestGenerationRequest {

    private enum InstanceTestGeneration {NEGATIVE, POSITIVE, BOTH}

    private A4Solution solution;
    private final CompModule context;
    private final Command command;
    private final InstanceTestGeneration instanceTestGeneration;
    private final boolean fromTrustedCommand;

    public static TestGenerationRequest createCETestRequest(A4Solution solution, CompModule context, Command command) {
        if (solution.getOriginalCommand().compareTo(command.toString()) != 0)
            throw new IllegalArgumentException("Command argument doesn't match the one use for the obtained solution");
        if (!command.check && command.expects != 0)
            throw new IllegalArgumentException("Command is not a check or run with expect 0");
        return new TestGenerationRequest(solution, context, command, null, false);
    }

    public static TestGenerationRequest createInstancePositiveTestRequestFromUntrustedCommand(A4Solution solution, CompModule context, Command command) {
        return createInstancePositiveTest(solution, context, command, false);
    }

    public static TestGenerationRequest createInstancePositiveTestRequestFromTrustedCommand(A4Solution solution, CompModule context, Command command) {
        return createInstancePositiveTest(solution, context, command, true);
    }

    public static TestGenerationRequest createInstancePositiveAndNegativeTests(A4Solution solution, CompModule context, Command command) {
        if (solution.getOriginalCommand().compareTo(command.toString()) != 0)
            throw new IllegalArgumentException("Command argument doesn't match the one use for the obtained solution");
        if (command.check || command.expects == 0)
            throw new IllegalArgumentException("Command is not a run with expect != 0");
        return new TestGenerationRequest(solution, context, command, InstanceTestGeneration.BOTH, false);
    }

    public List<TestGenerationRequest> splitABothRequestIntoPositiveAndNegative() {
        if (!instanceTestGeneration.equals(InstanceTestGeneration.BOTH)) {
            throw new IllegalStateException("This is not a request for both positive and negative tests creation");
        }
        List<TestGenerationRequest> split = new LinkedList<>();
        TestGenerationRequest positive = TestGenerationRequest.createInstancePositiveTest(solution, context, command, false);
        TestGenerationRequest negative = TestGenerationRequest.createInstanceNegativeTestRequest(solution, context, command);
        split.add(positive);
        split.add(negative);
        return split;
    }

    private static TestGenerationRequest createInstancePositiveTest(A4Solution solution, CompModule context, Command command, boolean fromTrustedCommand) {
        if (solution.getOriginalCommand().compareTo(command.toString()) != 0)
            throw new IllegalArgumentException("Command argument doesn't match the one use for the obtained solution");
        if (command.check || command.expects == 0)
            throw new IllegalArgumentException("Command is not a run with expect != 0");
        return new TestGenerationRequest(solution, context, command, InstanceTestGeneration.POSITIVE, fromTrustedCommand);
    }

    public static TestGenerationRequest createInstanceNegativeTestRequest(A4Solution solution, CompModule context, Command command) {
        if (solution.getOriginalCommand().compareTo(command.toString()) != 0)
            throw new IllegalArgumentException("Command argument doesn't match the one use for the obtained solution");
        if (command.check || command.expects == 0)
            throw new IllegalArgumentException("Command is not a run with expect != 0");
        return new TestGenerationRequest(solution, context, command, InstanceTestGeneration.NEGATIVE, false);
    }

    private TestGenerationRequest(A4Solution solution, CompModule context, Command command, InstanceTestGeneration instanceTestGeneration, boolean fromTrustedCommand) {
        this.solution = solution;
        this.context = context;
        this.command = command;
        this.instanceTestGeneration = instanceTestGeneration;
        this.fromTrustedCommand = fromTrustedCommand;
    }

    public A4Solution solution() {
        return solution;
    }

    public void updateSolution(A4Solution solution) {
        this.solution = solution;
    }

    public CompModule context() {
        return context;
    }

    public Command command() {
        return command;
    }

    public boolean isInstanceTestRequest() {
        return !command.check && command.expects != 0;
    }

    public boolean isInstancePositiveTestRequest() {
        return isInstanceTestRequest() && instanceTestGeneration.equals(InstanceTestGeneration.POSITIVE);
    }

    public boolean isInstanceNegativeTestRequest() {
        return isInstanceTestRequest() && instanceTestGeneration.equals(InstanceTestGeneration.NEGATIVE);
    }

    public boolean isInstancePositiveAndNegativeTestRequest() {
        return isInstanceTestRequest() && instanceTestGeneration.equals(InstanceTestGeneration.BOTH);
    }

    public boolean fromTrustedCommand() {
        return fromTrustedCommand;
    }

}

package ar.edu.unrc.dc.mutation.util;

import ar.edu.unrc.dc.mutation.visitors.FunctionsCollector;
import edu.mit.csail.sdg.ast.Browsable;
import edu.mit.csail.sdg.ast.Command;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.Func;

import java.util.*;

public class DependencyGraph {

    private static DependencyGraph instance;

    public static void initialize(List<Command> commands) {
        if (instance != null)
            throw new IllegalStateException("DependencyGraph already initialized");
        instance = new DependencyGraph(commands);
    }

    public static DependencyGraph getInstance() {
        if (instance == null)
            throw new IllegalStateException("DependencyGraph not yet initialized");
        return instance;
    }

    public static void destroyInstance() {
        if (instance == null)
            throw new IllegalStateException("No instance to destroy");
        instance = null;
    }

    private final Map<Browsable, List<Command>> dependencyGraph;
    private final Map<Browsable, List<Command>> directDependencyGraph;
    private final Map<Command, Integer> commandComplexity;
    private final List<Command> commands;

    private DependencyGraph(List<Command> commands) {
        if (commands == null)
            throw new IllegalArgumentException("commands list cannot be null");
        dependencyGraph = new HashMap<>();
        commandComplexity = new HashMap<>();
        directDependencyGraph = new HashMap<>();
        this.commands = commands;
    }

    public void enablePartialRepairMode() {
    }

    public void addDependencies(Browsable b, List<Command> commands) {
        dependencyGraph.put(b, commands);
    }
    public void addDirectDependencies(Browsable b, List<Command> commands) {
        directDependencyGraph.put(b, commands);
    }

    private final Map<Browsable, List<Command>> directIndependentCommands = new HashMap<>();
    public List<Command> getDirectIndependentCommandsFor(Browsable target, List<Browsable> allBuggedFunctionsAndAssertions) {
        List<Command> independentCommands = getDirectCommands(target);
        if (target instanceof Func) {
            Func targetAsFunc = (Func) target;
            for (Browsable b : directDependencyGraph.keySet()) {
                if (Browsable.equals(b, target))
                    continue;
                if (allBuggedFunctionsAndAssertions.stream().anyMatch(bb -> Browsable.equals(bb, b)))
                    continue;
                if (onlyCallsTargetAndNonBuggyFunctions(targetAsFunc, b, allBuggedFunctionsAndAssertions)) {
                    for (Command c : directDependencyGraph.get(b)) {
                        if (!independentCommands.contains(c))
                            independentCommands.add(c);
                    }
                }
            }
        }
        if (!directIndependentCommands.containsKey(target))
            directIndependentCommands.put(target, independentCommands);
        return independentCommands;
    }

    public List<Command> getDirectIndependentCommandsFor(Browsable target) {
        return directIndependentCommands.get(target);
    }

    private boolean onlyCallsTargetAndNonBuggyFunctions(Func target, Browsable from, List<Browsable> allBuggedFunctionsAndAssertions) {
        FunctionsCollector functionsCollector = new FunctionsCollector();
        Stack<Func> calledFuncs = new Stack<>();
        Expr bodyToScan;
        if (from instanceof Func) {
            bodyToScan = ((Func) from).getBody();
        } else {
            bodyToScan = (Expr) from;
        }
        Set<Func> collectedFunctions = functionsCollector.visitThis(bodyToScan);
        calledFuncs.addAll(collectedFunctions);
        return onlyCallsTargetAndNonBuggyFunctions(target, calledFuncs, allBuggedFunctionsAndAssertions);
    }

    private boolean onlyCallsTargetAndNonBuggyFunctions(Func target, Stack<Func> calledFunctions, List<Browsable> allBuggedFunctionsAndAssertions) {
        if (calledFunctions.size() == 1 && calledFunctions.contains(target))
            return true;
        boolean callsTarget = false;
        Set<String> visitedFunctions = new LinkedHashSet<>();
        while (!calledFunctions.isEmpty()) {
            Func calledFunc = calledFunctions.pop();
            visitedFunctions.add(calledFunc.label);
            if (Browsable.equals(target, calledFunc)) {
                callsTarget = true;
                continue;
            }
            if (allBuggedFunctionsAndAssertions.contains(calledFunc)) {
                return false;
            }
            FunctionsCollector functionsCollector = new FunctionsCollector();
            for (Func cFunc : functionsCollector.visitThis(calledFunc.getBody())) {
                if (visitedFunctions.add(cFunc.label)) {
                    calledFunctions.push(cFunc);
                }
            }
        }
        return callsTarget;
    }

    public List<Command> getPriorityCommands(List<Browsable> bList) {
        List<Command> priorityCommands = new LinkedList<>();
        bList.forEach(b -> getPriorityCommands(b).stream().filter(c -> !priorityCommands.contains(c)).forEach(priorityCommands::add));
        priorityCommands.sort((Comparator.comparingInt(this::getCommandComplexity)));
        return priorityCommands;
    }

    public List<Command> getNonPriorityCommands(List<Browsable> bList) {
        List<Command> nonPriorityCommands = new LinkedList<>(commands);
        nonPriorityCommands.removeAll(getPriorityCommands(bList));
        nonPriorityCommands.sort((Comparator.comparingInt(this::getCommandComplexity)));
        return nonPriorityCommands;
    }

    public List<Command> getPriorityCommands(Browsable b) {
        return dependencyGraph.containsKey(b)?dependencyGraph.get(b):new LinkedList<>();
    }

    public List<Command> getNonPriorityCommands(Browsable b) {
        List<Command> nonPriorityCommands = new LinkedList<>(commands);
        nonPriorityCommands.removeAll(getPriorityCommands(b));
        nonPriorityCommands.sort((Comparator.comparingInt(this::getCommandComplexity)));
        return nonPriorityCommands;
    }

    public List<Command> getDirectCommands(Browsable b) {
        return directDependencyGraph.containsKey(b)?directDependencyGraph.get(b):new LinkedList<>();
    }

    public List<Command> getAllCommands() {
        return commands;
    }

    public void setCommandComplexity(Command c, Integer complexity) {
        commandComplexity.put(c, complexity);
    }

    public int getCommandComplexity(Command c) {
        return getCommandComplexity(c, Integer.MAX_VALUE);
    }

    public int getCommandComplexity(Command c, Integer defaultValue) {
        if (defaultValue == null)
            throw new IllegalArgumentException("default value can't be null");
        if (c == null)
            throw new IllegalArgumentException("command can't be null");
        if (defaultValue < 0)
            throw new IllegalArgumentException("default value must be positive or zero");
        return commandComplexity.getOrDefault(c, defaultValue);
    }

}

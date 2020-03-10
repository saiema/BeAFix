package ar.edu.unrc.dc.mutation.util;

import edu.mit.csail.sdg.ast.Browsable;
import edu.mit.csail.sdg.ast.Command;

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

    private Map<Browsable, List<Command>> dependencyGraph;
    private Map<Command, Integer> commandComplexity;
    private List<Command> commands;

    private DependencyGraph(List<Command> commands) {
        if (commands == null)
            throw new IllegalArgumentException("commands list cannot be null");
        dependencyGraph = new HashMap<>();
        commandComplexity = new HashMap<>();
        this.commands = commands;
    }

    public void addDependencies(Browsable b, List<Command> commands) {
        dependencyGraph.put(b, commands);
    }

    public List<Command> getPriorityCommands(List<Browsable> bList) {
        List<Command> priorityCommands = new LinkedList<>();
        bList.forEach(b -> {
            getPriorityCommands(b).stream().filter(c -> !priorityCommands.contains(c)).forEach(priorityCommands::add);
        });
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

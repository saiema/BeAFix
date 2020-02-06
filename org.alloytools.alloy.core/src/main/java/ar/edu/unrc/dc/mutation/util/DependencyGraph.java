package ar.edu.unrc.dc.mutation.util;

import edu.mit.csail.sdg.ast.Browsable;
import edu.mit.csail.sdg.ast.Command;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class DependencyGraph {

    private Map<Browsable, List<Command>> dependencyGraph;
    private List<Command> commands;

    public DependencyGraph(List<Command> commands) {
        if (commands == null)
            throw new IllegalArgumentException("commands list cannot be null");
        dependencyGraph = new HashMap<>();
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
        return priorityCommands;
    }

    public List<Command> getNonPriorityCommands(List<Browsable> bList) {
        List<Command> nonPriorityCommands = new LinkedList<>(commands);
        nonPriorityCommands.removeAll(getPriorityCommands(bList));
        return nonPriorityCommands;
    }

    public List<Command> getPriorityCommands(Browsable b) {
        return dependencyGraph.containsKey(b)?dependencyGraph.get(b):new LinkedList<>();
    }

    public List<Command> getNonPriorityCommands(Browsable b) {
        List<Command> nonPriorityCommands = new LinkedList<>(commands);
        nonPriorityCommands.removeAll(getPriorityCommands(b));
        return nonPriorityCommands;
    }

}

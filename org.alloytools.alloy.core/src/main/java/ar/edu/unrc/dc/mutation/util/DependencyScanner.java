package ar.edu.unrc.dc.mutation.util;

import ar.edu.unrc.dc.mutation.visitors.FunctionsCollector;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.alloy4.SafeList;
import edu.mit.csail.sdg.ast.Browsable;
import edu.mit.csail.sdg.ast.Command;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.Func;
import edu.mit.csail.sdg.parser.CompModule;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class DependencyScanner {

    public static DependencyGraph scanDependencies(CompModule module) {
        TemporalGraph temporalGraph = new TemporalGraph();
        scanFunctions(module.getAllFunc(), module.getAllCommands(), temporalGraph);
        scanAsserts(module.getAllAssertions(), module.getAllCommands(), temporalGraph);
        temporalGraph.merge();
        DependencyGraph dependencyGraph = new DependencyGraph(module.getAllCommands());
        module.getAllFunc().forEach(f -> {
            dependencyGraph.addDependencies(f, temporalGraph.getRelatedCommands(f));
        });
        module.getAllAssertions().forEach(a -> {
            dependencyGraph.addDependencies(a.b, temporalGraph.getRelatedCommands(a.b));
        });
        return dependencyGraph;
    }

    private static void scanFunctions(SafeList<Func> functions, ConstList<Command> commands, TemporalGraph temporalGraph) {
        FunctionsCollector functionsCollector = new FunctionsCollector();
        functions.forEach(f -> {
            for (Func rf : functionsCollector.visitThis(f.getBody())) {
                temporalGraph.addFuncDependency(f, rf);
            }
        });
        commands.stream().filter(c -> !c.check).forEach(c -> {
            temporalGraph.addRelatedCommand(c.formula, c);
        });
    }

    private static void scanAsserts(ConstList<Pair<String, Expr>> assertions, ConstList<Command> commands, TemporalGraph temporalGraph) {
        FunctionsCollector functionsCollector = new FunctionsCollector();
        assertions.forEach(namedAssertion -> {
            Expr assertion = namedAssertion.b;
            functionsCollector.visitThis(assertion).forEach(rf -> {
                temporalGraph.addAssertDependency(assertion, rf);
            });
        });
        commands.stream().filter(c -> c.check).forEach(c -> {
            Expr assertionsName = c.formula;
            assertions.stream().filter(namedAssertion -> namedAssertion.a.compareTo(assertionsName.toString()) == 0).findFirst().ifPresent(nameAssertion -> {
                temporalGraph.addRelatedCommand(nameAssertion.b, c);
            });
        });
    }

    private static class TemporalGraph {
        private Map<Expr, List<Func>> assertRelatedFunctions;
        private Map<Expr, List<Command>> assertRelatedCommands;
        private Map<Func, List<Func>> funcDependencyGraph;
        private Map<Func, List<Command>> funcRelatedCommands;

        public TemporalGraph() {
            funcDependencyGraph = new HashMap<>();
            funcRelatedCommands = new HashMap<>();
            assertRelatedCommands = new HashMap<>();
            assertRelatedFunctions = new HashMap<>();
        }

        public void addFuncDependency(Func f1, Func f2) {
            List<Func> associatedFunctions;
            if (funcDependencyGraph.containsKey(f1)) {
                associatedFunctions = funcDependencyGraph.get(f1);
            } else {
                associatedFunctions = new LinkedList<>();
                funcDependencyGraph.put(f1, associatedFunctions);
            }
            if (!associatedFunctions.contains(f2))
                associatedFunctions.add(f2);
        }

        public void addRelatedCommand(Func f, Command c) {
            List<Command> associatedCommands;
            if (funcRelatedCommands.containsKey(f)) {
                associatedCommands = funcRelatedCommands.get(f);
            } else {
                associatedCommands = new LinkedList<>();
                funcRelatedCommands.put(f, associatedCommands);
            }
            if (!associatedCommands.contains(c))
                associatedCommands.add(c);
        }

        public void addAssertDependency(Expr a, Func f) {
            List<Func> associatedFunctions;
            if (assertRelatedFunctions.containsKey(a)) {
                associatedFunctions = assertRelatedFunctions.get(a);
            } else {
                associatedFunctions = new LinkedList<>();
                assertRelatedFunctions.put(a, associatedFunctions);
            }
            if (!associatedFunctions.contains(f))
                associatedFunctions.add(f);
        }

        public void addRelatedCommand(Expr a, Command c) {
            List<Command> associatedCommands;
            if (assertRelatedCommands.containsKey(a)) {
                associatedCommands = assertRelatedCommands.get(a);
            } else {
                associatedCommands = new LinkedList<>();
                assertRelatedCommands.put(a, associatedCommands);
            }
            if (!associatedCommands.contains(c))
                associatedCommands.add(c);
        }

        public List<Command> getRelatedCommands(Browsable b) {
            if (b instanceof Expr) {
                return assertRelatedCommands.containsKey(b)?assertRelatedCommands.get(b):new LinkedList<>();
            } else if (b instanceof Func) {
                return funcRelatedCommands.containsKey(b)?funcRelatedCommands.get(b):new LinkedList<>();
            } else {
                throw new IllegalArgumentException("Argument is neither an Expr nor a Func");
            }
        }

        public void merge() {
            mergeFunctionDependencies();
            mergeFunctionRelatedCommands();
            mergeAssertsRelatedCommands();
        }

        private void mergeFunctionDependencies() {
            for (Func f : funcDependencyGraph.keySet()) {
                List<Func> relatedFunctions = new LinkedList<>();
                collectRelatedFunctions(f, relatedFunctions);
                funcDependencyGraph.put(f, relatedFunctions);
            }
        }

        private void mergeFunctionRelatedCommands() {
            for (Func f : funcDependencyGraph.keySet()) {
                List<Command> relatedCommands = new LinkedList<>();
                collectRelatedCommands(f, relatedCommands);
                funcRelatedCommands.put(f, relatedCommands);
            }
        }

        private void mergeAssertsRelatedCommands() {
            for (Expr a : assertRelatedFunctions.keySet()) {
                List<Command> collectedCommands = new LinkedList<>();
                for (Func rf : assertRelatedFunctions.get(a)) {
                    for (Command c : funcRelatedCommands.get(rf)) {
                        if (!collectedCommands.contains(c))
                            collectedCommands.add(c);
                    }
                }
                for (Command c : assertRelatedCommands.get(a)) {
                    if (!collectedCommands.contains(c))
                        collectedCommands.add(c);
                }
                assertRelatedCommands.put(a, collectedCommands);
            }
        }

        private void collectRelatedFunctions(Func f, List<Func> relatedFunctions) {
            for (Func rf : funcDependencyGraph.get(f)) {
                if (!relatedFunctions.contains(rf)) {
                    relatedFunctions.add(f);
                    collectRelatedFunctions(rf, relatedFunctions);
                }
            }
        }

        private void collectRelatedCommands(Func f, List<Command> relatedCommands) {
            for (Command rc : funcRelatedCommands.get(f)) {
                if (!relatedCommands.contains(rc))
                    relatedCommands.add(rc);
            }
            for (Func rf : funcDependencyGraph.get(f)) {
                collectRelatedCommands(rf, relatedCommands);
            }
        }

    }

}

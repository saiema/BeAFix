package ar.edu.unrc.dc.mutation.mutantLab.testGeneration;

import edu.mit.csail.sdg.ast.Command;
import edu.mit.csail.sdg.ast.ExprVar;
import edu.mit.csail.sdg.ast.Sig;

import java.util.*;
import java.util.Map.Entry;

import static ar.edu.unrc.dc.mutation.mutantLab.testGeneration.TestGeneratorHelper.alloyNameToSkolem;

public class VariableMapping {

    private final Map<Sig, List<ExprVar>> signatureValues;
    private final Map<ExprVar, ExprVar> originalVarsToSkolemVars;
    private final Map<String, Integer> originalVarsLabelCount;
    private final List<ExprVar> skolemVars;
    private final Command cmd;
    private boolean extended = false;

    public VariableMapping(List<ExprVar> originalVars, List<ExprVar> skolemVars, Map<Sig, List<ExprVar>> signatureValues, Command cmd) {
        originalVarsToSkolemVars = new HashMap<>();
        originalVarsLabelCount = new TreeMap<>();
        this.skolemVars = new LinkedList<>(skolemVars);
        this.cmd = cmd;
        this.signatureValues = new HashMap<>(signatureValues);
        createMapping(originalVars, skolemVars);
    }

    private VariableMapping(VariableMapping from) {
        originalVarsToSkolemVars = new HashMap<>();
        originalVarsLabelCount = new TreeMap<>();
        originalVarsToSkolemVars.putAll(from.originalVarsToSkolemVars);
        originalVarsLabelCount.putAll(from.originalVarsLabelCount);
        cmd = from.cmd;
        this.signatureValues = new HashMap<>(from.signatureValues);
        skolemVars = new LinkedList<>(from.skolemVars);
        extended = from.extended;
    }

    public static VariableMapping extendPreviousMapping(VariableMapping from, List<ExprVar> newVars) {
        VariableMapping variableMapping = new VariableMapping(from);
        variableMapping.createMapping(newVars, variableMapping.skolemVars);
        variableMapping.extended = true;
        return variableMapping;
    }

    public ExprVar skolemVar(ExprVar originalVar) {
        return originalVarsToSkolemVars.get(originalVar);
    }

    public boolean availableSkolem(ExprVar var) {return originalVarsToSkolemVars.containsKey(var);}

    public boolean isSkolemUsed(ExprVar svar) {
        return originalVarsToSkolemVars.values().stream().anyMatch(exprVar -> exprVar.label.compareTo(svar.label) == 0);
    }

    public Map<Sig, List<ExprVar>> signatureValues() {
        return signatureValues;
    }

    public boolean isExtended() { return extended; }

    public VariableMapping cleanMappingToAlloyNames() {
        return new VariableMapping(new LinkedList<>(originalVarsToSkolemVars.keySet()), new LinkedList<>(skolemVars), this.signatureValues, cmd);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (Entry<ExprVar, ExprVar> map : originalVarsToSkolemVars.entrySet()) {
            sb.append(map.getKey().toString()).append(" -> ").append(map.getValue().toString());
            sb.append("\n");
        }
        return sb.toString();
    }

    private void createMapping(List<ExprVar> vars, List<ExprVar> skolemVars) {
        for (ExprVar v : vars) {
            if (!originalVarsToSkolemVars.containsKey(v)) { //first time variable is seen
                String skolemLabel;
                String varName = "$" + alloyNameToSkolem(v.label, cmd);
                if (!originalVarsLabelCount.containsKey(v.label)) { //first time variable's label is seen
                    skolemLabel = varName;
                    originalVarsLabelCount.put(v.label, 1);
                } else { //label already seen
                    Integer currentCount = originalVarsLabelCount.get(v.label);
                    skolemLabel = primeVar(varName, currentCount);
                    currentCount++;
                    originalVarsLabelCount.put(v.label, currentCount);
                }
                ExprVar skolemVar = getSkolemVar(skolemLabel, skolemVars);
                if (skolemVar == null)
                    throw new IllegalStateException("Couldn't find skolem var for label " + skolemLabel);
                originalVarsToSkolemVars.put(v, skolemVar);
            }
        }
    }

    private String primeVar(String label, Integer count) {
        StringBuilder primedVar = new StringBuilder(label);
        for (int i = 0; i < count; i++)
            primedVar.append('\'');
        return primedVar.toString();
    }

    private ExprVar getSkolemVar(String label, List<ExprVar> skolemVars) {
        for (ExprVar sVar : skolemVars) {
            if (sVar.label.compareTo(label) == 0)
                return sVar;
        }
        return null;
    }

}

package ar.edu.unrc.dc.mutation.mutantLab.testGeneration;

import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.ast.*;
import edu.mit.csail.sdg.ast.Sig.Field;
import edu.mit.csail.sdg.parser.CompModule;
import edu.mit.csail.sdg.translator.A4Solution;
import kodkod.ast.Relation;
import kodkod.instance.Tuple;

import java.util.*;
import java.util.stream.Collectors;

public class TestGeneratorHelper {

    static boolean isNumber(Object o) {
        if (o == null) {
            return false;
        }
        try {
            Integer.parseInt(o.toString());
        } catch (NumberFormatException nfe) {
            return false;
        }
        return true;
    }

    static boolean isMin(Object o) {
        if (o == null) {
            return false;
        }
        return o.toString().compareTo("min") == 0;
    }

    static boolean isMax(Object o) {
        if (o == null) {
            return false;
        }
        return o.toString().compareTo("max") == 0;
    }

    static boolean isNone(Object o) {
        if (o == null) {
            return false;
        }
        return o.toString().compareTo("none") == 0;
    }

    static boolean isIden(Object o) {
        if (o == null) {
            return false;
        }
        return o.toString().compareTo("iden") == 0;
    }

    static boolean isConstant(Object o) {
        return isNumber(o) || isMin(o) || isMax(o) || isNone(o) || isIden(o);
    }

    static Optional<Expr> objectToExpr(Object o, Map<Sig, List<ExprVar>> signatureValues) {
        if (isConstant(o)) {
            if (isNumber(o)) {
                return Optional.of(ExprConstant.makeNUMBER(Integer.parseInt(o.toString())));
            }
            if (isMin(o)) {
                return Optional.of(ExprConstant.MIN);
            }
            if (isMax(o)) {
                return Optional.of(ExprConstant.MAX);
            }
            if (isNone(o)) {
                return Optional.of(ExprConstant.EMPTYNESS);
            }
            if (isIden(o)) {
                return Optional.of(ExprConstant.IDEN);
            }
        } else {
            String oValueAsAlloyName = internalAtomNotationToAlloyName(o.toString());
            for (List<ExprVar> vars : signatureValues.values()) {
                for (ExprVar var : vars) {
                    if (var.label.compareTo(oValueAsAlloyName) == 0)
                        return Optional.of(var);
                }
            }
        }
        return Optional.empty();
    }

    public static String internalAtomNotationToAlloyName(String internalNotation) {
        return internalNotation.replaceAll("\\$", "");
    }

    public static String alloyVarNameToInternalAtomNotation(String alloyName, boolean var) {
        if (alloyName.startsWith("\\$"))
            throw new IllegalArgumentException("Alloy Name is already in internal atom notation (" + alloyName + ")");
        if (var)
            return "$" + alloyName;
        int index = 0;
        while (index < alloyName.length() && !Character.isDigit(alloyName.charAt(index)))
            index++;
        if (index == alloyName.length())
            throw new IllegalArgumentException("There should be a digit in the name");
        return alloyName.substring(0, index) + "$" + alloyName.substring(index);
    }

    static String alloyNameToSkolem(String alloyName, Command cmd) {
        if (cmd.nameExpr instanceof ExprVar) {
            return ((ExprVar) cmd.nameExpr).label + "_" + alloyName;
        } else {
            return alloyName;
        }
    }

     static Optional<Sig> nameToSig(Relation relation, CompModule context) {
        String targetName = relation.name().replace(" remainder", "");
        for (Sig sig : getAllSigs(context)) {
             if (targetName.compareTo(sig.label) == 0)
                 return Optional.of(sig);
         }
         return Optional.empty();
     }

     static Optional<Sig.Field> nameToField(Relation relation, CompModule context) {
         for (Sig sig : getAllSigs(context)) {
             for (Sig.Field field : sig.getFields()) {
                 String fieldName = sig.label + "." + field.label;
                 if (relation.name().compareTo(fieldName) == 0)
                     return Optional.of(field);
             }
         }
         return Optional.empty();
     }

     static boolean filterRelation(Relation relation, A4Solution solution) {
         if (relation.name().startsWith("Int/"))
             return true;
         if (relation.name().compareTo("seq/Int") == 0)
             return true;
         if (relation.name().compareTo("String") == 0)
             return true;
         if (relation.name().contains("$"))
             return true;
         return isSkolem(relation, solution);
     }

     static boolean isSignature(Relation relation) {
         return !relation.isSkolem() && !relation.name().contains(".");
     }

     static boolean isSkolem(Relation relation, A4Solution solution) {
         for (ExprVar skolem : solution.getAllSkolems()) {
             if (relation.name().equals(skolem.label))
                 return true;
         }
         return false;
     }

     static boolean isField(Relation relation) {
         return !relation.isSkolem() && relation.name().contains(".");
     }

     static Expr tupleToExpr(Tuple rawValue, Map<Sig, List<ExprVar>> signatureValues) {
         Expr expr = null;
         for (int i = 0; i < rawValue.arity(); i++) {
             Object atom = rawValue.atom(i);
             Optional<Expr> atomAsExpr = objectToExpr(atom, signatureValues);
             if (!atomAsExpr.isPresent())
                 throw new IllegalStateException("Couldn't get an expression for " + atom.toString());
             if (expr == null) {
                 expr = (Expr) atomAsExpr.get().clone();
             } else {
                 expr = ExprBinary.Op.ARROW.make(null, null, (Expr) expr.clone(), (Expr) atomAsExpr.get().clone());
             }
         }
         return expr;
     }

     static Expr getInitValue(List<? extends Expr> values) {
        if (values == null || values.isEmpty())
            return null;
        Expr initValue = null;
        for (Expr v : values) {
            if (initValue == null)
                initValue = (Expr) v.clone();
            else
                initValue = ExprBinary.Op.PLUS.make(null, null, (Expr) initValue.clone(), (Expr) v.clone());
            if (initValue.errors != null && !initValue.errors.isEmpty())
                throw new IllegalStateException("Bad expression generated when creating init value (" +
                        values.stream().map(Expr::toString).collect(Collectors.joining(",")) +
                        ") : " + initValue.errors.stream().map(Throwable::toString).collect(Collectors.joining(","))
                );
        }
        return initValue;
     }

    private static final String SYMBOLS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    static String generateRandomName(int length) {
        StringBuilder sb = new StringBuilder();
        Random rng = new Random();
        for (int i = 0; i < length; i++)
            sb.append(SYMBOLS.charAt(rng.nextInt(SYMBOLS.length())));
        return sb.toString();
    }

    static boolean extendsNonBuiltIn(Sig s) {
        if (s == null)
            return false;
        if (s.isBuiltInSig())
            return false;
        if (s instanceof Sig.PrimSig) {
            Sig.PrimSig sAsPrimSig = (Sig.PrimSig) s;
            return sAsPrimSig.parent != null && !sAsPrimSig.parent.isBuiltInSig() && sAsPrimSig.parent.isAbstract == null;
        }
        if (s instanceof Sig.SubsetSig) {
            Sig.SubsetSig sAsSubsetSig = (Sig.SubsetSig) s;
            for (Sig parent : sAsSubsetSig.parents) {
                if (!parent.isBuiltInSig() && parent.isAbstract == null)
                    return true;
            }
            return false;
        }
        return false;
    }

    static boolean extendsSignatures(Sig thiz, Sig that) {
        if (thiz.isBuiltInSig())
            return false;
        if (thiz instanceof Sig.PrimSig) {
            Sig.PrimSig thizAsPrimSig = (Sig.PrimSig) thiz;
            Sig parent = thizAsPrimSig.parent;
            if (parent.equals(that))
                return true;
            return extendsSignatures(parent, that);
        }
        if (thiz instanceof Sig.SubsetSig) {
            Sig.SubsetSig thizAsSubsetSig = (Sig.SubsetSig) thiz;
            for (Sig parent : thizAsSubsetSig.parents) {
                if (parent.equals(that))
                    return true;
                return extendsSignatures(parent, that);
            }
        }
        return false;
    }

    static Map<Sig, List<ExprVar>> mergeExtendingSignaturesValues(Map<Sig, List<ExprVar>> signaturesValues) {
        Map<Sig, List<ExprVar>> mergedSignatures = new HashMap<>();
        signaturesValues.forEach((sig, exprVars) -> mergedSignatures.put(sig, new LinkedList<>(exprVars)));
        boolean modified = true;
        while (modified) {
            modified = false;
            List<Sig> sigs = new LinkedList<>(mergedSignatures.keySet());
            for (Sig s : sigs) {
                if (!mergedSignatures.containsKey(s))
                    continue;
                List<ExprVar> sVars = mergedSignatures.get(s);
                Set<String> sVarsNames = sVars.stream().map(v -> v.label).collect(Collectors.toCollection(TreeSet::new));
                Map<Sig, List<String>> toRemove = new HashMap<>();
                for (Sig extendingSig : extendingSignatures(s, sigs)) {
                    List<ExprVar> extendingSigVars = mergedSignatures.getOrDefault(extendingSig, new LinkedList<>());
                    for (ExprVar eVar : extendingSigVars) {
                        if (sVarsNames.contains(eVar.label)) {
                            List<String> currentVarsToRemove = toRemove.getOrDefault(extendingSig, new LinkedList<>());
                            currentVarsToRemove.add(eVar.label);
                            toRemove.put(extendingSig, currentVarsToRemove);
                            //remove(eVar.label, sVars);
                            modified = true;
                        }
                    }
                }
                if (modified) {
                    updateSignatureVariables(mergedSignatures, toRemove);
                }
                /*if (sVars.isEmpty() && modified)
                    signaturesValues.remove(s);
                else
                    signaturesValues.put(s, sVars);*/
            }
        }
        return mergedSignatures;
    }

    private static void updateSignatureVariables(Map<Sig, List<ExprVar>> signaturesValues, Map<Sig, List<String>> toRemove) {
        for (Sig s : toRemove.keySet()) {
            for (String varToRemove : toRemove.get(s)) {
                remove(varToRemove, signaturesValues.get(s));
            }
            if (signaturesValues.get(s).isEmpty())
                signaturesValues.remove(s);
        }
    }

    private static void remove(String label, List<ExprVar> vars) {
        ExprVar varToRemove = null;
        for (ExprVar v : vars) {
            if (v.label.compareTo(label) == 0) {
                varToRemove = v;
                break;
            }
        }
        if (varToRemove != null)
            vars.remove(varToRemove);
    }

    static List<Sig> extendingSignatures(Sig sig, Collection<Sig> allSigs) {
        List<Sig> extendingSigs = new LinkedList<>();
        if (!sig.isBuiltInSig()) {
            for (Sig otherSig : allSigs) {
                if (sig.equals(otherSig))
                    continue;
                if (extendsSignatures(otherSig, sig))
                    extendingSigs.add(otherSig);
            }
        }
        return extendingSigs;
    }

    private static List<Sig> getAllSigs(CompModule root) {
        return root.getAllReachableSigs();
    }

    public static List<Field> getAllFields(CompModule root) {
        List<Field> fields = new LinkedList<>();
        for (Sig s : getAllSigs(root)) {
            for (Field f : s.getFields()) {
                fields.add(f);
            }
        }
        return fields;
    }

    public static Optional<Func> getNoParametersFunctionFromModel(String functionName, String model, CompModule context) {
        for (CompModule m : context.getAllReachableModules()) {
            //search for specific model
            if (m.getModelName().compareTo(model) == 0) {
                //search for specific function
                for (Func f : m.getAllFunc()) {
                    if (removeAlias(f.label).compareTo(functionName) == 0 && f.decls.isEmpty())
                        return Optional.of(f);
                }
            }
        }
        return Optional.empty();
    }

    public static Browsable getPredicateOrAssertionCalled(Command command, CompModule context) {
        Browsable predicateOrAssertionCalled = null;
        if (command.nameExpr instanceof ExprVar) {
            String callee = ((ExprVar) command.nameExpr).label;
            for (Func pred : context.getAllFunc()) {
                if (pred.label.replace("this/", "").compareTo(callee) == 0) {
                    predicateOrAssertionCalled = pred;
                    break;
                }
            }
            if (predicateOrAssertionCalled == null) {
                for (Pair<String, Expr> assertion : context.getAllAssertions()) {
                    if (assertion.a.compareTo(callee) == 0) {
                        predicateOrAssertionCalled = assertion.b;
                        break;
                    }
                }
            }
        } else {
            predicateOrAssertionCalled = command.nameExpr;
        }
        return predicateOrAssertionCalled == null?null: (Browsable) predicateOrAssertionCalled.clone();
    }

    public static ExprVar internalNamedVarToAlloyNamedVar(ExprVar var) {
        ExprVar copy = ExprVar.make(null, internalAtomNotationToAlloyName(var.label), var.type());
        copy.setVarID(var.getVarID());
        return copy;
    }

    public static ExprVar alloyNamedVarToInternalNamedVar(ExprVar var) {
        ExprVar copy = ExprVar.make(null, alloyVarNameToInternalAtomNotation(var.label, true), var.type());
        copy.setVarID(var.getVarID());
        return copy;
    }

    private static String removeAlias(String key) {
        int lastSlash = key.lastIndexOf('/');
        if (lastSlash == -1)
            return key;
        return key.substring(lastSlash + 1);
    }

}

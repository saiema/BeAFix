package ar.edu.unrc.dc.mutation.util;

import ar.edu.unrc.dc.mutation.visitors.VarBoundReplacementVerifier;
import edu.mit.csail.sdg.ast.*;
import edu.mit.csail.sdg.ast.Sig.PrimSig;
import edu.mit.csail.sdg.ast.Type.ProductType;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

public final class TypeChecking {

    public static boolean canReplace(Expr target, Expr replacement, boolean strictCheck) {
        if (replacement instanceof ExprBad || replacement instanceof ExprBadCall || replacement instanceof ExprBadJoin)
            return false;
        if (replacement.errors != null && !replacement.errors.isEmpty())
            return false;
        return canReplace(target, replacement.type(), strictCheck);
    }

    public static boolean canReplace(Expr target, Type replacementType, boolean strictCheck) {
        //we should first check if we are checking a replacement inside a bounded expression for either
        //a ExprQt or a ExprLet
        Optional<Boolean> boundReplacementCheck = checkBoundReplacement(target, replacementType);
        if (boundReplacementCheck.isPresent())
            return boundReplacementCheck.get();
        Optional<Expr> nearestContext = getContext(target);
        if (strictCheck && !compatibleVariablesChecker(target, replacementType, true))
            return false;
        if (nearestContext.isPresent()) {
            Expr context = nearestContext.get();
            if (context instanceof ExprUnary) {
                ExprUnary contextAsUnary = (ExprUnary) context;
                return checkUnary(target, replacementType, contextAsUnary.op, strictCheck);
            } else if (context instanceof ExprBinary) {
                ExprBinary contextAsBinary = (ExprBinary) context;
                boolean lor = contextAsBinary.left.getID() == target.getID();
                return checkBinary(lor?contextAsBinary.right:contextAsBinary.left, replacementType, contextAsBinary.op, lor, strictCheck);
            } else if (context instanceof ExprQt) {
                //if target's parent is an ExprQt then we are changing the whole formula or a whole decl
                //since for the moment there are no operators that can change a whole decl, we must only check
                //for changes in the whole formula
                ExprQt qt = (ExprQt) context;
                if (qt.op.equals(ExprQt.Op.SUM))
                    return replacementType.is_int() || replacementType.is_small_int();
                else
                    return replacementType.is_bool;
            } else if (context instanceof ExprLet) {
                //if target's parent is an ExprLet then we are changing either the variable, the bound, or
                //the formula (the body of the let expression).
                //Since there are no operators that change variable names, and we previously checked if we are
                //modifying the bound, we must only check for the body of the let expression.
                return target.type().is_bool == replacementType.is_bool && target.type().arity() == replacementType.arity();
            } else if (context instanceof ExprList) {
                ExprList list = (ExprList) context;
                return checkExprList(target, list, replacementType);
            } else if (context instanceof ExprCall) {
                ExprCall call = (ExprCall) context;
                return checkExprCallArgument(target, call, replacementType, strictCheck);
            } else if (context instanceof ExprITE) {
                ExprITE ite = (ExprITE) context;
                if (ite.getID() == target.getID()) {
                    return replacementType.is_bool;
                }
                return true;
            } else {
                //we can only make a strict type check
                return target.type().equals(replacementType);
            }
        } else {
            //we can only make a strict type check
            return target.type().equals(replacementType);
        }
    }

    public static boolean checkExprCallArgument(Expr target, ExprCall call, Type replacementType, boolean strictCheck) {
        Func associatedFunc = call.fun;
        if (call.args.size() != associatedFunc.decls.size())
            throw new IllegalStateException("The call and the associated function have different number of arguments, but this should never happen");
        for (int fa = 0; fa < associatedFunc.decls.size(); fa++) {
            Decl formalArgument = associatedFunc.decls.get(fa);
            for (Expr formalArg : formalArgument.names) {
                if (formalArg.toString().compareTo(target.toString()) != 0)
                    continue;
                return compatibleVariablesChecker(formalArgument.expr.type(), replacementType, strictCheck);
            }
        }
        return true;
    }

    public static boolean checkExprList(Expr target, ExprList list, Type replacementType) {
        switch (list.op) {
            case DISJOINT: {
                if (list.args.get(0).getID() == target.getID())
                    return list.args.get(0).type().hasCommonArity(replacementType);
                else if (list.args.get(1).getID() == target.getID())
                    return list.args.get(1).type().hasCommonArity(replacementType);
                else
                    return list.args.get(2).type().hasCommonArity(replacementType);
            }
            case TOTALORDER: {
                if (list.args.get(0).getID() == target.getID() || list.args.get(1).getID() == target.getID())
                    return replacementType.hasArity(1);
                else
                    return replacementType.hasArity(2);
            }
            case AND:
            case OR:
                return replacementType.is_bool;
            default:
                throw new IllegalStateException("Unexpected value: " + list.op);
        }
    }

    public static boolean checkBinary(Expr x, Type replacementType, ExprBinary.Op op, boolean lor, boolean strictCheck) {
        switch (op) {
            case PLUSPLUS : {
                Type resType = x.type().unionWithCommonArity(replacementType);
                if (emptyOrNone(resType)) {
                    return false;
                }
                Optional<Expr> opParent = getContext(x);
                return opParent.map(o -> canReplace(o, resType, strictCheck)).orElse(true);
            }
            case DOMAIN : {
                Type xType = x.type();
                Type resType;
                if (lor) {
                    resType = xType.domainRestrict(replacementType);
                } else {
                    resType = replacementType.domainRestrict(xType);
                }
                if (emptyOrNone(resType)) {
                    return false;
                }
                Optional<Expr> opParent = getContext(x);
                return opParent.map(o -> canReplace(o, resType, strictCheck)).orElse(true);
            }
            case RANGE : {
                Type xType = x.type();
                Type resType;
                if (lor) {
                    resType = replacementType.rangeRestrict(xType);
                } else {
                    resType = xType.rangeRestrict(replacementType);
                }
                if (emptyOrNone(resType)) {
                    return false;
                }
                Optional<Expr> opParent = getContext(x);
                return opParent.map(o -> canReplace(o, resType, strictCheck)).orElse(true);
            }
            case IMINUS :
            case IPLUS :
            case MUL :
            case DIV :
            case REM :
            case LT :
            case LTE :
            case GT :
            case GTE :
            case SHL :
            case SHR :
            case SHA :
            case NOT_LT :
            case NOT_GT :
            case NOT_LTE :
            case NOT_GTE : {
                return replacementType.is_int() || replacementType.is_small_int();
            }
            case AND :
            case OR :
            case IFF :
            case IMPLIES :
                return replacementType.is_bool;
            case IN :
            case NOT_IN : {
                Type xType = x.type();
                if (lor) {
                    return replacementType.hasCommonArity(xType);
                } else {
                    return xType.hasCommonArity(replacementType);
                }
            }
            case PLUS :
            case MINUS :
            case INTERSECT :
            case EQUALS :
            case NOT_EQUALS : {
                Type xType = x.type();
                if (lor) {
                    return !emptyOrNone(replacementType.intersect(xType));
                } else {
                    return !emptyOrNone(xType.intersect(replacementType));
                }
            }
            case JOIN : {
                Type xType = x.type();
                Type resType;
                if (lor) {
                    resType = replacementType.join(xType);
                } else {
                    resType = xType.join(replacementType);
                }
                if (emptyOrNone(resType)) {
                    return false;
                }
                Optional<Expr> opParent = getContext(x);
                return opParent.map(o -> canReplace(o, resType, strictCheck)).orElse(true);
            }
            case ISSEQ_ARROW_LONE : {
                if (lor) {
                    if (replacementType.is_small_int())
                        return true;
                    if (replacementType.is_int())
                        return true;
                    return replacementType.size() > 0;
                }
                return true;
            }
            case ARROW :
            case ANY_ARROW_LONE :
            case ANY_ARROW_ONE :
            case ANY_ARROW_SOME :
            case LONE_ARROW_ANY :
            case LONE_ARROW_LONE :
            case LONE_ARROW_ONE :
            case LONE_ARROW_SOME :
            case SOME_ARROW_ANY :
            case SOME_ARROW_LONE :
            case SOME_ARROW_ONE :
            case SOME_ARROW_SOME :
            case ONE_ARROW_ANY :
            case ONE_ARROW_LONE :
            case ONE_ARROW_ONE :
            case ONE_ARROW_SOME : {
                Type xType = x.type();
                if (lor) {
                    return !emptyOrNone(replacementType.product(xType));
                } else {
                    return !emptyOrNone(xType.product(replacementType));
                }
            }
            default :
                return replacementType.is_small_int() || replacementType.is_int() || (replacementType.size() > 0);
        }
    }

    public static boolean checkUnary(Expr x, Type replacementType, ExprUnary.Op op, boolean strictCheck) {
        switch (op) {
            case NOT :
                return replacementType.is_bool;
            case NOOP : {
                Optional<Expr> opParent = getContext(x);
                return opParent.map(o -> canReplace(o, replacementType, strictCheck)).orElse(true);
            }
            case LONE :
            case NO :
            case ONE :
            case SOME :
            case CARDINALITY : {
                if (replacementType.is_small_int())
                    return true;
                if (replacementType.is_int())
                    return true;
                return replacementType.size() > 0;
            }
            case EXACTLYOF :
            case SETOF : {
                Type resType = Type.removesBoolAndInt(replacementType);
                if (emptyOrNone(resType))
                    return false;
                Optional<Expr> opParent = getContext(x);
                return opParent.map(o -> canReplace(o, resType, strictCheck)).orElse(true);
            }
            case LONEOF :
            case ONEOF :
            case SOMEOF : {
                Type unaryType = replacementType.extract(1);
                if (emptyOrNone(unaryType))
                    return false;
                Optional<Expr> opParent = getContext(x);
                return opParent.map(o -> canReplace(o, unaryType, strictCheck)).orElse(true);
            }
            case RCLOSURE :
            case CLOSURE : {
                Type resType = replacementType.closure();
                if (emptyOrNone(resType)) {
                    return false;
                }
                Optional<Expr> opParent = getContext(x);
                return opParent.map(o -> canReplace(o, resType, strictCheck)).orElse(true);
            }
            case TRANSPOSE : {
                Type resType = replacementType.transpose();
                if (emptyOrNone(resType)) {
                    return false;
                }
                Optional<Expr> opParent = getContext(x);
                return opParent.map(o -> canReplace(o, resType, strictCheck)).orElse(true);
            }
            case CAST2INT :
                return replacementType.hasArity(1);
            case CAST2SIGINT : {
                if (replacementType.is_small_int())
                    return true;
                if (replacementType.is_int())
                    return true;
            }
            default:
                return false;
        }
    }

    public static boolean compatibleVariablesChecker(Type toReplaceType, Type replacementType, boolean strictTypeChecking) {
        if (strictTypeChecking)
            return toReplaceType.equals(replacementType);
        Optional<Sig> toReplaceFirst = getFirst(toReplaceType);
        Optional<Sig> replacementFirst = getFirst(replacementType);
        Optional<Sig> toReplaceLast = getLast(toReplaceType);
        Optional<Sig> replacementLast = getLast(replacementType);
        if (!toReplaceFirst.isPresent() || !replacementFirst.isPresent())
            return false;
        if (!toReplaceLast.isPresent() || !replacementLast.isPresent())
            return false;
        return compatibleTypes(toReplaceFirst.get(), replacementFirst.get()) && compatibleTypes(toReplaceLast.get(), replacementLast.get());
    }

    public static boolean compatibleVariablesChecker(Expr toReplace, Type replacementType, boolean strictTypeChecking) {
        return compatibleVariablesChecker(toReplace.type(), replacementType, strictTypeChecking);
    }

    public static Expr getMayorExpression(Expr of) {
        Browsable current = of;
        while((current instanceof Expr) && isMinor(current)) {
            current = current.getBrowsableParent();
        }
        if (!(current instanceof Expr)) throw new IllegalStateException("current should be an Expr");
        return (Expr) current;
    }

    public static boolean emptyOrNone(Type joinedType) {
        if (joinedType.toString().equals(Type.EMPTY.toString()))
            return true;
        for (ProductType productType : joinedType) {
            if (productType.isEmpty())
                return true;
        }
        return false;
    }

    public static Type getType(Expr e) {
        //        if (e instanceof Field) {
        //            Field eAsField = (Field) e;
        //            return appendTypes(eAsField.sig.type(), e.type());
        //        }
        return e.type();
    }

    private static Optional<Boolean> checkBoundReplacement(Expr target, Type replacementType) {
        Optional<Expr> exprQtOrLet = getQrOrLet(target);
        if (exprQtOrLet.isPresent() && isReplacingBounds(target)) {
            Expr formula;
            List<Expr> varsToCheck = new LinkedList<>();
            if (exprQtOrLet.get() instanceof ExprQt) {
                ExprQt qt = (ExprQt) exprQtOrLet.get();
                formula = qt.sub;
                for (Decl d : qt.decls) {
                    if (d.expr.getID() == target.getID()) {
                        varsToCheck.addAll(d.names);
                        break;
                    }
                }
            } else {
                ExprLet let = (ExprLet) exprQtOrLet.get();
                formula = let.sub;
                varsToCheck.add(let.var);
            }
            if (varsToCheck.isEmpty())
                throw new IllegalStateException("There should be at least one variable to check");
            for (Expr var : varsToCheck) {
                VarBoundReplacementVerifier varBoundReplacementVerifier = new VarBoundReplacementVerifier(var, replacementType);
                if (!varBoundReplacementVerifier.visitThis(formula))
                    return Optional.of(Boolean.FALSE);
            }
            return Optional.of(Boolean.TRUE);
        }
        return Optional.empty();
    }

    private static Optional<Expr> getQrOrLet(Expr target) {
        Expr mayorExpr = getMayorExpression(target);
        Browsable current = target;
        while (current != mayorExpr && !(current instanceof ExprLet) && !(current instanceof  ExprQt)) {
            current = current.getBrowsableParent();
        }
        if ((current instanceof ExprLet) || (current instanceof ExprQt))
            return Optional.of((Expr)current);
        return Optional.empty();
    }

    private static boolean isReplacingBounds(Expr target) {
        Optional<Expr> qtOrLet = getQrOrLet(target);
        if (qtOrLet.isPresent()) {
            if (qtOrLet.get() instanceof ExprQt) {
                ExprQt qt = (ExprQt) qtOrLet.get();
                for (Decl d : qt.decls) {
                    if (d.expr.getID() == target.getID())
                        return true;
                }
            } else {
                ExprLet let = (ExprLet) qtOrLet.get();
                return let.expr.getID() == target.getID();
            }
        }
        return false;
    }

    private static Optional<Expr> getContext(Expr target) {
        if (isMinor(target)) {
            return Optional.of((Expr)target.getBrowsableParent());
        }
        return Optional.empty();
    }

    private static Optional<Sig> getFirst(Type t) {
        if (t.arity() < 1)
            return Optional.empty();
        Iterator<ProductType> it = t.iterator();
        ProductType first = it.hasNext() ? it.next() : null;
        if (first == null)
            return Optional.empty();
        PrimSig[] types = first.getAll();
        if (types.length == 0 || types[0] == null)
            return Optional.empty();
        return Optional.of(types[0]);
    }

    private static Optional<Sig> getLast(Type t) {
        if (t.arity() < 1)
            return Optional.empty();
        Iterator<ProductType> it = t.iterator();
        ProductType last = null;
        while (it.hasNext()) {
            last = it.next();
        }
        if (last == null)
            return Optional.empty();
        Sig.PrimSig[] types = last.getAll();
        if (types.length == 0 || types[types.length - 1] == null)
            return Optional.empty();
        return Optional.of(types[types.length - 1]);
    }

    private static boolean compatibleTypes(Sig a, Sig b) {
        return a.isSameOrDescendentOf(b) || b.isSameOrDescendentOf(a);
    }

    private static boolean isMinor(Browsable b) {
        if (b.getBrowsableParent() == null) return false;
        Browsable parent = b.getBrowsableParent();
        if (!(parent instanceof Expr)) return false;
        return !(parent instanceof Sig);
    }

}

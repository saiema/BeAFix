package ar.edu.unrc.dc.mutation.visitors;

import java.util.Iterator;
import java.util.List;

import ar.edu.unrc.dc.mutation.Mutator;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.Decl;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.ExprBinary;
import edu.mit.csail.sdg.ast.ExprBinary.Op;
import edu.mit.csail.sdg.ast.ExprCall;
import edu.mit.csail.sdg.ast.ExprConstant;
import edu.mit.csail.sdg.ast.ExprITE;
import edu.mit.csail.sdg.ast.ExprLet;
import edu.mit.csail.sdg.ast.ExprList;
import edu.mit.csail.sdg.ast.ExprQt;
import edu.mit.csail.sdg.ast.ExprUnary;
import edu.mit.csail.sdg.ast.ExprVar;
import edu.mit.csail.sdg.ast.Func;
import edu.mit.csail.sdg.ast.Sig;
import edu.mit.csail.sdg.ast.Type;
import edu.mit.csail.sdg.ast.Type.ProductType;
import edu.mit.csail.sdg.ast.VisitReturn;

public class QtBoundReplacementVerifier extends VisitReturn<Boolean> {

    /**
     * position of expression to search
     */
    private Expr var;
    private Type replacementType;


    public QtBoundReplacementVerifier(Expr var, Type replacementType) {
        this.var = var;
        this.replacementType = replacementType;
    }

    @Override
    public Boolean visit(ExprBinary x) throws Err {
        Boolean res = null;
        if (match(x.left, this.var)) {
            return checkBinary(x.right, replacementType, x.op, true);
        } else if (match(x.right, this.var)) {
            return checkBinary(x.left, replacementType, x.op, false);
        }
        if (x.left.accept(this) && x.right.accept(this))
            return Boolean.TRUE;
        return Boolean.FALSE;
    }

    private Boolean checkBinary(Expr x, Type replacementType, Op op, boolean lor) {
        switch (op) {
            case PLUSPLUS : {
                return !emptyOrNone(x.type().unionWithCommonArity(replacementType));
            }
            case DOMAIN : {
                if (lor) {
                    Type rightType = x.type();
                    return !emptyOrNone(rightType.domainRestrict(replacementType));
                } else {
                    Type leftType = x.type();
                    return !emptyOrNone(replacementType.domainRestrict(leftType));
                }
            }
            case RANGE : {
                if (lor) {
                    Type rightType = x.type();
                    return !emptyOrNone(replacementType.rangeRestrict(rightType));
                } else {
                    Type leftType = x.type();
                    return !emptyOrNone(leftType.rangeRestrict(replacementType));
                }
            }
            case IFF :
            case AND :
            case IMPLIES :
            case OR :
                return true;
            case IN :
            case PLUS :
            case MINUS :
            case INTERSECT : {
                if (lor) {
                    Type rightType = x.type();
                    return !emptyOrNone(replacementType.intersect(rightType));
                } else {
                    Type leftType = x.type();
                    return !emptyOrNone(leftType.intersect(replacementType));
                }
            }
            case JOIN : {
                if (lor) {
                    Type rightType = x.type();
                    return !emptyOrNone(replacementType.join(rightType));
                } else {
                    Type leftType = x.type();
                    return !emptyOrNone(leftType.join(replacementType));
                }
            }
            case DIV :
            case IMINUS :
            case IPLUS :
            case MUL :
                return true;
            case EQUALS :
            case NOT_EQUALS : {
                if (lor) {
                    Type rightType = x.type();
                    return !emptyOrNone(replacementType.intersect(rightType));
                } else {
                    Type leftType = x.type();
                    return !emptyOrNone(leftType.intersect(replacementType));
                }
            }
            case GT :
            case GTE :
            case LT :
            case LTE :
            case NOT_GT :
            case NOT_GTE :
            case NOT_IN :
            case NOT_LT :
            case NOT_LTE :
                return true;
            case REM :
            case SHA :
            case SHL :
            case SHR :
                return replacementType.is_int();
            case ISSEQ_ARROW_LONE : {
                if (lor) {
                    if (replacementType.is_small_int())
                        return true;
                    if (replacementType.is_int())
                        return true;
                    if (replacementType.size() > 0)
                        return true;
                    return false;
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
                if (lor) {
                    Type rightType = x.type();
                    return !emptyOrNone(replacementType.product(rightType));
                } else {
                    Type leftType = x.type();
                    return !emptyOrNone(leftType.product(replacementType));
                }
            }
            default :
                return false;

        }
    }

    @Override
    public Boolean visit(ExprList x) throws Err {
        int idx = 0;
        for (Expr e : x.args) {
            if (match(e, this.var)) {
                if (!checkExprList(x, idx, replacementType, x.op))
                    return Boolean.FALSE;
            } else {
                if (!e.accept(this))
                    return Boolean.FALSE;
            }
            idx++;
        }
        return Boolean.TRUE;
    }

    private Boolean checkExprList(ExprList list, int idxReplacement, Type replacementType, ExprList.Op op) {
        switch (op) {
            case AND :
            case OR :
                return true; //TODO: maybe improve in the future
            case TOTALORDER : {
                if (list.args.size() != 3) {
                    throw new IllegalStateException("Scanning an already invalid total order list");
                } else {
                    if (idxReplacement == 0 && replacementType.hasArity(1))
                        return false;
                    if (idxReplacement == 1 && replacementType.hasArity(1))
                        return false;
                    if (idxReplacement == 0 && replacementType.hasArity(2))
                        return false;
                }
            }
            case DISJOINT : {
                Type neighbourType = null;
                if (list.args.size() == 1) {
                    return true;
                } else if (list.args.size() - 1 == idxReplacement) {
                    neighbourType = list.args.get(idxReplacement - 1).type();
                } else {
                    neighbourType = list.args.get(idxReplacement + 1).type();
                }
                if (replacementType.size() < 2)
                    return false;
                return !emptyOrNone(neighbourType.pickCommonArity(replacementType));
            }
            default :
                return false;

        }
    }

    @Override
    public Boolean visit(ExprCall x) throws Err {
        int idx = 0;
        for (Expr e : x.args) {
            if (match(e, this.var)) {
                if (!checkExprCall(x, idx, replacementType))
                    return Boolean.FALSE;
            } else {
                if (!e.accept(this))
                    return Boolean.FALSE;
            }
            idx++;
        }
        return Boolean.TRUE;
    }

    private boolean checkExprCall(ExprCall call, int idxReplacement, Type replacementType) {
        Func calledFunc = call.fun;
        List<Decl> formalArgs = calledFunc.decls;
        int exploredIdx = 0;
        for (Decl d : formalArgs) {
            exploredIdx += d.names.size() - 1;
            if (idxReplacement <= exploredIdx) {
                //The affected variable is in this Decl
                Type formalArgType = d.expr.type();
                if (Mutator.compatibleVariablesChecker(formalArgType, replacementType, false)) {
                    return true;
                }
                break;
            }
        }
        return false;
    }

    @Override
    public Boolean visit(ExprConstant x) throws Err {
        return Boolean.TRUE;
    }

    @Override
    public Boolean visit(ExprITE x) throws Err {
        Boolean res = null;
        if (match(x.cond, this.var)) {
            return this.replacementType.is_bool;
        }
        if (!x.cond.accept(this))
            return Boolean.FALSE;
        if (!x.left.accept(this))
            return Boolean.FALSE;
        if (!x.right.accept(this))
            return Boolean.FALSE;
        return Boolean.TRUE;
    }

    @Override
    public Boolean visit(ExprLet x) throws Err {
        if (match(x.var, this.var)) //The variable we are checking has been masked
            return Boolean.TRUE;
        SearchExpr searcher = new SearchExpr(this.var) {

            @Override
            protected boolean match(Expr a, Expr b) {
                return this.match(a, b);
            }

        };
        if (searcher.visitThis(x.expr)) {
            //We altered the let variable type, we must continue our check with this new variable
            QtBoundReplacementVerifier verifier = new QtBoundReplacementVerifier(x.var, this.replacementType);
            return verifier.visitThis(x.sub);
        }
        if (!x.sub.accept(this))
            return Boolean.FALSE;
        return Boolean.TRUE;
    }

    @Override
    public Boolean visit(ExprQt x) throws Err {
        SearchExpr searcher = new SearchExpr(this.var) {

            @Override
            protected boolean match(Expr a, Expr b) {
                return this.match(a, b);
            }

        };
        for (Decl d : x.decls) {
            for (Expr var : d.names) {
                if (match(var, this.var))
                    return Boolean.TRUE; //The variable we are checking has been masked
            }
            if (searcher.visitThis(d.expr)) {
                //We altered the let variable type, we must continue our check with all variables in this Decl
                for (Expr var : d.names) {
                    QtBoundReplacementVerifier verifier = new QtBoundReplacementVerifier(var, this.replacementType);
                    if (!verifier.visitThis(x.sub))
                        return Boolean.FALSE;
                }
                return Boolean.TRUE;
            } else {
                if (!d.expr.accept(this))
                    return Boolean.FALSE;
            }
        }
        return x.sub.accept(this);
    }

    @Override
    public Boolean visit(ExprUnary x) throws Err {
        if (match(x.sub, this.var)) {
            return checkUnary(x.sub, this.replacementType, x.op);
        } else
            return x.sub.accept(this);
    }

    private boolean checkUnary(Expr x, Type replacementType, ExprUnary.Op op) {
        switch (op) {
            case NOOP :
            case NOT :
            case CAST2SIGINT :
            case CAST2INT :
                break;
            default : {
                if (replacementType.is_small_int())
                    break;
                if (replacementType.is_int())
                    break;
                if (replacementType.size() > 0)
                    break;
                return false;
            }

        }
        switch (op) {
            case NOT :
                return replacementType.is_bool;
            case NOOP :
                return true;
            case EXACTLYOF :
            case SETOF :
                return !emptyOrNone(Type.removesBoolAndInt(replacementType));
            case LONEOF :
            case ONEOF :
            case SOMEOF :
                return !emptyOrNone(replacementType.extract(1));
            case RCLOSURE :
            case CLOSURE :
                return !emptyOrNone(replacementType.closure());
            case TRANSPOSE :
                return !emptyOrNone(replacementType.transpose());
            case LONE :
            case NO :
            case ONE :
            case SOME :
                return true;
            case CAST2INT :
                return replacementType.hasArity(1);
            case CARDINALITY :
                return true;

            case CAST2SIGINT : {
                if (replacementType.is_small_int())
                    return true;
                if (replacementType.is_int())
                    return true;
            }
            default :
                return false;

        }
    }

    @Override
    public Boolean visit(ExprVar x) throws Err {
        return Boolean.TRUE;
    }

    @Override
    public Boolean visit(Sig x) throws Err {
        return Boolean.TRUE;
    }

    @Override
    public Boolean visit(Sig.Field x) throws Err {
        return Boolean.TRUE;
    }

    private boolean match(Expr a, Expr b) {
        return a.toString().compareTo(b.toString()) == 0;
    }

    private boolean emptyOrNone(Type joinedType) {
        if (joinedType.toString().equals(Type.EMPTY.toString()))
            return true;
        Iterator<ProductType> it = joinedType.iterator();
        while (it.hasNext()) {
            if (it.next().isEmpty())
                return true;
        }
        return false;
    }
}

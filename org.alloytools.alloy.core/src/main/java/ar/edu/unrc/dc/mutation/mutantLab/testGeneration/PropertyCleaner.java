package ar.edu.unrc.dc.mutation.mutantLab.testGeneration;

import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.*;

import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

public class PropertyCleaner extends VisitReturn<Expr> {

    private boolean clean = true;

    public Expr cleanExpression(Expr x) {
        return visitThis(cleanNoop(x));
    }

    private Expr visitThisFrom(Expr x, Expr from) {
        if (from instanceof ExprList && ((ExprList) from).op.equals(ExprList.Op.AND)) {
            if (clean && (x instanceof ExprUnary || x instanceof ExprBinary)) {
                return visitThis(x);
            } else if (clean) {
                clean = false;
                Expr ret = visitThis(x);
                clean = true;
                return ret;
            }
        } else if (from instanceof ExprUnary && ((ExprUnary)from).op.equals(ExprUnary.Op.NOOP)) {
            return visitThis(x);
        }
        clean = false;
        return visitThis(x);
    }

    @Override
    public Expr visit(ExprBinary x) throws Err {
        if (clean && x.op.equals(ExprBinary.Op.EQUALS)) { //check if it's an assignment of sig or field
            Expr left = cleanNoop(x.left);
            if (left instanceof Sig) {
                return null;
            }
            if (left instanceof Sig.Field) {
                return null;
            }
            if (left instanceof ExprBinary) {
               ExprBinary leftAsBinary = (ExprBinary) left;
               JoinExpressionGetSigAndFields joinExpressionGetSigAndFields = new JoinExpressionGetSigAndFields();
               List<Expr> sigsAndFields = joinExpressionGetSigAndFields.visitThis(leftAsBinary);
               if (sigsAndFields != null && !sigsAndFields.isEmpty())
                   return null;
            }
        }
        return x;
    }

    @Override
    public Expr visit(ExprList x) throws Err {
        if (clean && x.op.equals(ExprList.Op.AND)) {
            List<Expr> newArgs = new LinkedList<>();
            for (Expr arg : x.args) {
                Expr cleanedExpr = visitThisFrom(arg, x);
                if (cleanedExpr != null)
                    newArgs.add((Expr)cleanedExpr.clone());
            }
            if (newArgs.isEmpty())
                return null;
            if (newArgs.size() == 1)
                return newArgs.get(0);
            ExprList cleanedExprList = ExprList.make(null, null, x.op, newArgs);
            if (cleanedExprList.errors != null && !cleanedExprList.errors.isEmpty())
                throw new IllegalStateException("Bad expression while cleaning (" +
                        x.op.toString() +
                        "[ " + newArgs.stream().map(Expr::toString).collect(Collectors.joining(",")) + "]" +
                        ") : " + cleanedExprList.errors.stream().map(Throwable::toString).collect(Collectors.joining(","))
                );
            return cleanedExprList;
        }
        return x;
    }

    @Override
    public Expr visit(ExprCall x) throws Err {
        return x;
    }

    @Override
    public Expr visit(ExprConstant x) throws Err {
        return x;
    }

    @Override
    public Expr visit(ExprITE x) throws Err {
        return x;
    }

    @Override
    public Expr visit(ExprLet x) throws Err {
        return x;
    }

    @Override
    public Expr visit(ExprQt x) throws Err {
        throw new IllegalStateException("There shouldn't be any quantified expression");
    }

    @Override
    public Expr visit(ExprUnary x) throws Err {
        if (clean) {
            switch (x.op) {
                case NO: {
                    Expr sub = cleanNoop(x.sub);
                    if (sub instanceof Sig)
                        return null;
                    if (sub instanceof Sig.Field)
                        return null;
                    if (sub instanceof ExprBinary) {
                        ExprBinary subAsBinary = (ExprBinary) sub;
                        JoinExpressionGetSigAndFields joinExpressionGetSigAndFields = new JoinExpressionGetSigAndFields();
                        List<Expr> sigsAndFields = joinExpressionGetSigAndFields.visitThis(subAsBinary);
                        if (sigsAndFields != null && !sigsAndFields.isEmpty())
                            return null;
                    }
                    return sub;
                }
                case NOOP:
                    return visitThisFrom(cleanNoop(x.sub), x);
            }
        }
        return x;
    }

    @Override
    public Expr visit(ExprVar x) throws Err {
        return x;
    }

    @Override
    public Expr visit(Sig x) throws Err {
        return x;
    }

    @Override
    public Expr visit(Sig.Field x) throws Err {
        return x;
    }

    private Expr cleanNoop(Expr x) {
        if (x instanceof ExprUnary) {
            ExprUnary xAsUnary = (ExprUnary) x;
            if (xAsUnary.op.equals(ExprUnary.Op.NOOP))
                return cleanNoop(xAsUnary.sub);
        }
        return x;
    }

    private static final class JoinExpressionGetSigAndFields extends VisitReturn<List<Expr>> {

        @Override
        public List<Expr> visit(ExprBinary x) throws Err {
            if (x.op.equals(ExprBinary.Op.JOIN)) {
                return joinLists(visitThis(x.left), visitThis(x.right));
            } else {
                return emptyList();
            }
        }

        @Override
        public List<Expr> visit(ExprList x) throws Err {
            return null;
        }

        @Override
        public List<Expr> visit(ExprCall x) throws Err {
            return null;
        }

        @Override
        public List<Expr> visit(ExprConstant x) throws Err {
            return null;
        }

        @Override
        public List<Expr> visit(ExprITE x) throws Err {
            return null;
        }

        @Override
        public List<Expr> visit(ExprLet x) throws Err {
            return null;
        }

        @Override
        public List<Expr> visit(ExprQt x) throws Err {
            return null;
        }

        @Override
        public List<Expr> visit(ExprUnary x) throws Err {
            return null;
        }

        @Override
        public List<Expr> visit(ExprVar x) throws Err {
            return null;
        }

        @Override
        public List<Expr> visit(Sig x) throws Err {
            return singleElementList(x);
        }

        @Override
        public List<Expr> visit(Sig.Field x) throws Err {
            return singleElementList(x);
        }

        private List<Expr> singleElementList(Expr x) {
            List<Expr> result = new LinkedList<>();
            result.add(x);
            return result;
        }

        private List<Expr> emptyList() {
            return new LinkedList<>();
        }

        private List<Expr> joinLists(List<Expr> a, List<Expr> b) {
            if (b == null || b.isEmpty())
                return a;
            if (a == null || a.isEmpty())
                return b;
            List<Expr> joinedList = new LinkedList<>(a);
            for (Expr bExpr : b) {
                boolean addExpr = true;
                for (Expr jExpr : joinedList) {
                    if (bExpr.toString().compareTo(jExpr.toString()) == 0) {
                        addExpr = false;
                        break;
                    }
                }
                if (addExpr)
                    joinedList.add(bExpr);
            }
            return joinedList;
        }

    }

}

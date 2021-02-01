package ar.edu.unrc.dc.mutation.visitors;

import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.*;

import java.util.*;

import static ar.edu.unrc.dc.mutation.util.DependencyGraph.funcIsTrusted;

public class FunctionsCollector extends VisitReturn<Set<Func>> {

    private boolean bugged;
    private boolean nonBugged;

    private FunctionsCollector() {
        bugged = true;
        nonBugged = true;
    }

    public static FunctionsCollector allFunctionsCollector() {
        FunctionsCollector collector = new FunctionsCollector();
        collector.bugged = true;
        collector.nonBugged = true;
        return collector;
    }

    public static FunctionsCollector buggedFunctionsCollector() {
        FunctionsCollector collector = new FunctionsCollector();
        collector.bugged = true;
        collector.nonBugged = false;
        return collector;
    }

    public static FunctionsCollector nonBuggedFunctionCollector() {
        FunctionsCollector collector = new FunctionsCollector();
        collector.bugged = false;
        collector.nonBugged = true;
        return collector;
    }


    @Override
    public Set<Func> visitThis(Expr x) throws Err {
        return super.visitThis(x);
    }

    @Override
    public Set<Func> visit(ExprBinary x) throws Err {
        Set<Func> result = new TreeSet<>((Comparator.comparing(o -> o.label)));
        result.addAll(x.left.accept(this));
        result.addAll(x.right.accept(this));
        return result;
    }

    @Override
    public Set<Func> visit(ExprList x) throws Err {
        Set<Func> result = new TreeSet<>((Comparator.comparing(o -> o.label)));
        for (Expr e : x.args) {
            result.addAll(e.accept(this));
        }
        return result;
    }

    @Override
    public Set<Func> visit(ExprCall x) throws Err {
        Set<Func> result = new TreeSet<>((Comparator.comparing(o -> o.label)));
        if (acceptedFunction(x.fun)) {
            result.add(x.fun);
        }
        for (Expr arg : x.args) {
            result.addAll(arg.accept(this));
        }
        return result;
    }

    @Override
    public Set<Func> visit(ExprConstant x) throws Err {
        return new TreeSet<>((Comparator.comparing(o -> o.label)));
    }

    @Override
    public Set<Func> visit(ExprITE x) throws Err {
        Set<Func> result = new TreeSet<>((Comparator.comparing(o -> o.label)));
        result.addAll(x.cond.accept(this));
        result.addAll(x.left.accept(this));
        result.addAll(x.right.accept(this));
        return result;
    }

    @Override
    public Set<Func> visit(ExprLet x) throws Err {
        Set<Func> result = new TreeSet<>((Comparator.comparing(o -> o.label)));
        result.addAll(x.expr.accept(this));
        result.addAll(x.sub.accept(this));
        return result;
    }

    @Override
    public Set<Func> visit(ExprQt x) throws Err {
        Set<Func> result = new TreeSet<>((Comparator.comparing(o -> o.label)));
        for (Decl d : x.decls) {
            result.addAll(d.expr.accept(this));
        }
        result.addAll(x.sub.accept(this));
        return result;
    }

    @Override
    public Set<Func> visit(ExprUnary x) throws Err {
        Set<Func> result = new TreeSet<>((Comparator.comparing(o -> o.label)));
        result.addAll(x.sub.accept(this));
        return result;
    }

    @Override
    public Set<Func> visit(ExprVar x) throws Err {
        return new TreeSet<>((Comparator.comparing(o -> o.label)));
    }

    @Override
    public Set<Func> visit(Sig x) throws Err {
        Set<Func> result = new TreeSet<>((Comparator.comparing(o -> o.label)));
        x.findAllFunctions().forEach(f -> {
                    if (acceptedFunction(f)) result.add(f);
                }
        );
        return result;
    }

    @Override
    public Set<Func> visit(Sig.Field x) throws Err {
        Set<Func> result = new TreeSet<>((Comparator.comparing(o -> o.label)));
        x.findAllFunctions().forEach(f -> {
                if (acceptedFunction(f)) result.add(f);
            }
        );
        return result;
    }

    private boolean acceptedFunction(Func f) {
        if (bugged && nonBugged)
            return true;
        boolean isFuncBugged = !funcIsTrusted(f);
        return (bugged && isFuncBugged) || (nonBugged && !isFuncBugged);
    }

}

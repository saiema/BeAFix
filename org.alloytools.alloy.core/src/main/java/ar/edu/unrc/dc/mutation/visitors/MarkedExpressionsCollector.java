package ar.edu.unrc.dc.mutation.visitors;

import ar.edu.unrc.dc.mutation.mutantLab.Candidate;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.ast.*;
import edu.mit.csail.sdg.parser.CompModule;

import java.util.*;

public class MarkedExpressionsCollector extends VisitReturn<Optional<List<Expr>>> {

    private Candidate from;
    private CompModule ast;
    private boolean valid = false;

    public MarkedExpressionsCollector(Candidate from) {
        if (from == null)
            throw new IllegalArgumentException("from can't be null");
        if (!from.isValid())
            throw new IllegalArgumentException("from is not a valid candidate");
        if (from.getContext() == null)
            throw new IllegalArgumentException("candidate's related ast can't be null");
        this.from = from;
        this.ast = from.getContext();
    }

    public MarkedExpressionsCollector(CompModule ast) {
        this.ast = ast;
    }

    public Optional<List<Expr>> getMarkedExpressions() {
        valid = true;
        List<Expr> markedExpressions = new LinkedList<>();
        for (Pair<String, Expr> namedAssertion : ast.getAllAssertions()) {
            visitThis(namedAssertion.b).ifPresent(markedExpressions::addAll);
        }
        for (Func func : ast.getAllFunc()) {
            visitThis(func.getBody()).ifPresent(markedExpressions::addAll);
        }
        if (from != null)
            from.clearMutatedStatus();
        valid = false;
        if (!markedExpressions.isEmpty())
            return Optional.of(markedExpressions);
        return Optional.empty();
    }

    @Override
    public Optional<List<Expr>> visitThis(Expr x) throws Err {
        if (!valid)
            throw new IllegalStateException("The main method for this class is getMarkedExpressions");
        Optional<Expr> mutant = from != null?from.getMutatedExpr(x):Optional.empty();
        if (mutant.isPresent()) {
            from.markAsAlreadyMutated(x);
            return visitThis(mutant.get());
        }
        return super.visitThis(x);
    }

    @Override
    public Optional<List<Expr>> visit(ExprBinary x) throws Err {
        if (x.directMutGenLimit() > 0)
            return Optional.of(Collections.singletonList(x));
        List<Expr> markedExpressions = null;
        Optional<List<Expr>> markedExpressionsLeft = visitThis(x.left);
        if (markedExpressionsLeft.isPresent()) {
            markedExpressions = markedExpressionsLeft.get();
        }
        Optional<List<Expr>> markedExpressionsRight = visitThis(x.right);
        if (markedExpressionsRight.isPresent()) {
            if (markedExpressions == null)
                markedExpressions = markedExpressionsRight.get();
            else
                markedExpressions.addAll(markedExpressionsRight.get());
        }
        if (markedExpressions != null)
            return Optional.of(markedExpressions);
        return Optional.empty();
    }

    @Override
    public Optional<List<Expr>> visit(ExprList x) throws Err {
        if (x.directMutGenLimit() > 0)
            return Optional.of(Collections.singletonList(x));
        List<Expr> markedExpressions = new LinkedList<>();
        for (Expr arg : x.args) {
            visitThis(arg).ifPresent(markedExpressions::addAll);
        }
        if (!markedExpressions.isEmpty())
            return Optional.of(markedExpressions);
        return Optional.empty();
    }

    @Override
    public Optional<List<Expr>> visit(ExprCall x) throws Err {
        if (x.directMutGenLimit() > 0)
            return Optional.of(Collections.singletonList(x));
        List<Expr> markedExpressions = new LinkedList<>();
        for (Expr arg : x.args) {
            visitThis(arg).ifPresent(markedExpressions::addAll);
        }
        if (!markedExpressions.isEmpty())
            return Optional.of(markedExpressions);
        return Optional.empty();
    }

    @Override
    public Optional<List<Expr>> visit(ExprConstant x) throws Err {
        if (x.directMutGenLimit() > 0)
            return Optional.of(Collections.singletonList(x));
        return Optional.empty();
    }

    @Override
    public Optional<List<Expr>> visit(ExprITE x) throws Err {
        if (x.directMutGenLimit() > 0)
            return Optional.of(Collections.singletonList(x));
        List<Expr> markedExpressions = new LinkedList<>();
        visitThis(x.cond).ifPresent(markedExpressions::addAll);
        visitThis(x.left).ifPresent(markedExpressions::addAll);
        visitThis(x.right).ifPresent(markedExpressions::addAll);
        if (!markedExpressions.isEmpty())
            return Optional.of(markedExpressions);
        return Optional.empty();
    }

    @Override
    public Optional<List<Expr>> visit(ExprLet x) throws Err {
        if (x.directMutGenLimit() > 0)
            return Optional.of(Collections.singletonList(x));
        List<Expr> markedExpressions = new LinkedList<>();
        visitThis(x.var).ifPresent(markedExpressions::addAll);
        visitThis(x.expr).ifPresent(markedExpressions::addAll);
        visitThis(x.sub).ifPresent(markedExpressions::addAll);
        if (!markedExpressions.isEmpty())
            return Optional.of(markedExpressions);
        return Optional.empty();
    }

    @Override
    public Optional<List<Expr>> visit(ExprQt x) throws Err {
        if (x.directMutGenLimit() > 0)
            return Optional.of(Collections.singletonList(x));
        List<Expr> markedExpressions = new LinkedList<>();
        for (Decl d : x.decls) {
            for (Expr var : d.names) {
                visitThis(var).ifPresent(markedExpressions::addAll);
            }
            visitThis(d.expr).ifPresent(markedExpressions::addAll);
        }
        visitThis(x.sub).ifPresent(markedExpressions::addAll);
        if (!markedExpressions.isEmpty())
            return Optional.of(markedExpressions);
        return Optional.empty();
    }

    @Override
    public Optional<List<Expr>> visit(ExprUnary x) throws Err {
        if (x.directMutGenLimit() > 0)
            return Optional.of(Collections.singletonList(x));
        return visitThis(x.sub);
    }

    @Override
    public Optional<List<Expr>> visit(ExprVar x) throws Err {
        if (x.directMutGenLimit() > 0)
            return Optional.of(Collections.singletonList(x));
        return Optional.empty();
    }

    @Override
    public Optional<List<Expr>> visit(Sig x) throws Err {
        if (x.directMutGenLimit() > 0)
            return Optional.of(Collections.singletonList(x));
        return Optional.empty();
    }

    @Override
    public Optional<List<Expr>> visit(Sig.Field x) throws Err {
        if (x.directMutGenLimit() > 0)
            return Optional.of(Collections.singletonList(x));
        return Optional.empty();
    }
}

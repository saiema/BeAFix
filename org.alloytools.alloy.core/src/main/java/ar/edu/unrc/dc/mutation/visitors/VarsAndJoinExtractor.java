package ar.edu.unrc.dc.mutation.visitors;

import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.*;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

public class VarsAndJoinExtractor extends VisitReturn<Optional<List<Expr>>> {

    @Override
    public Optional<List<Expr>> visit(ExprBinary x) throws Err {
        List<Expr> result = new LinkedList<>();
        if (x.op.equals(ExprBinary.Op.JOIN))
            result.add(x);
        else {
            Optional<List<Expr>> leftResult = x.left.accept(this);
            leftResult.ifPresent(result::addAll);
            Optional<List<Expr>> rightResult = x.right.accept(this);
            rightResult.ifPresent(result::addAll);
        }
        if (!result.isEmpty())
            return Optional.of(result);
        return Optional.empty();
    }

    @Override
    public Optional<List<Expr>> visit(ExprList x) throws Err {
        List<Expr> result = new LinkedList<>();
        for (Expr arg : x.args) {
            Optional<List<Expr>> argResult = arg.accept(this);
            argResult.ifPresent(result::addAll);
        }
        if (!result.isEmpty())
            return Optional.of(result);
        return Optional.empty();
    }

    @Override
    public Optional<List<Expr>> visit(ExprCall x) throws Err {
        List<Expr> result = new LinkedList<>();
        for (Expr arg : x.args) {
            Optional<List<Expr>> argResult = arg.accept(this);
            argResult.ifPresent(result::addAll);
        }
        if (!result.isEmpty())
            return Optional.of(result);
        return Optional.empty();
    }

    @Override
    public Optional<List<Expr>> visit(ExprConstant x) throws Err {
        return Optional.empty();
    }

    @Override
    public Optional<List<Expr>> visit(ExprITE x) throws Err {
        List<Expr> result = new LinkedList<>();
        Optional<List<Expr>> condResult = x.cond.accept(this);
        Optional<List<Expr>> thenResult = x.left.accept(this);
        Optional<List<Expr>> elseResult = x.right.accept(this);
        condResult.ifPresent(result::addAll);
        thenResult.ifPresent(result::addAll);
        elseResult.ifPresent(result::addAll);
        if (!result.isEmpty())
            return Optional.of(result);
        return Optional.empty();
    }

    @Override
    public Optional<List<Expr>> visit(ExprLet x) throws Err {
        List<Expr> result = new LinkedList<>();
        Optional<List<Expr>> subResult = x.sub.accept(this);
        subResult.ifPresent(result::addAll);
        if (!result.isEmpty())
            return Optional.of(result);
        return Optional.empty();
    }

    @Override
    public Optional<List<Expr>> visit(ExprQt x) throws Err {
        return Optional.empty(); //for the moment will simply won't recollect expressions from a quantifier expression
    }

    @Override
    public Optional<List<Expr>> visit(ExprUnary x) throws Err {
        List<Expr> result = new LinkedList<>();
        Optional<List<Expr>> subResult = x.sub.accept(this);
        subResult.ifPresent(result::addAll);
        if (!result.isEmpty())
            return Optional.of(result);
        return Optional.empty();
    }

    @Override
    public Optional<List<Expr>> visit(ExprVar x) throws Err {
        return Optional.of(Arrays.asList(x));
    }

    @Override
    public Optional<List<Expr>> visit(Sig x) throws Err {
        return Optional.empty();
    }

    @Override
    public Optional<List<Expr>> visit(Sig.Field x) throws Err {
        return Optional.empty();
    }
}

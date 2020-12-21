package ar.edu.unrc.dc.mutation.visitors;

import ar.edu.unrc.dc.mutation.mutantLab.Candidate;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.*;

import java.util.Iterator;
import java.util.Optional;

public class ExprToStringNicePrint extends VisitReturn<Void> {

    private final StringBuilder sb;
    private int indent;
    private final Candidate candidate;

    public ExprToStringNicePrint() {this(null, 0);}

    public ExprToStringNicePrint(Candidate candidate) {
        this(candidate, 0);
    }

    public ExprToStringNicePrint(Candidate candidate, int startingIndentation) {
        sb = new StringBuilder();
        indent = 0;
        this.candidate = candidate;
        this.indent = startingIndentation;
    }

    public String getStringRepresentation() {
        return sb.toString();
    }

    public void visitPredicate(Func f) throws Err {
        if (f == null)
            throw new IllegalArgumentException("Func f is null");
        if (!f.isPred)
            throw new IllegalArgumentException("Func " + f.label + " is not a predicate");
        if (f.hasCommentPreviousLine())
            sb.append("--").append(f.getCommentPreviousLine()).append("\n");
        sb.append("pred ").append(f.label.replace("this/","")).append(" [");
        Iterator<Decl> declIterator = f.decls.iterator();
        while (declIterator.hasNext()) {
            Decl currentDecl = declIterator.next();
            Iterator<? extends ExprHasName> varNamesIterator =  currentDecl.names.iterator();
            while (varNamesIterator.hasNext()) {
                sb.append(varNamesIterator.next().label);
                if (varNamesIterator.hasNext())
                    sb.append(", ");
            }
            String type = currentDecl.expr.type().toString();
            if (currentDecl.expr.type().arity() == 1) {
                type = type.replaceAll("\\{","").replaceAll("}", "");
            }
            sb.append(" : ").append(type);
            if (declIterator.hasNext())
                sb.append(", ");
        }
        sb.append("]").append("{\n");
        indent++;
        addIndent();
        checkBlockAndVisit(f.getBody());
        indent--;
        sb.append("\n}\n");
        if (f.hasCommentNextLine())
            sb.append("--").append(f.getCommentNextLine()).append("\n");
    }

    public void visitFunction(Func f) throws Err {
        if (f == null)
            throw new IllegalArgumentException("Func f is null");
        if (f.isPred)
            throw new IllegalArgumentException("Func " + f.label + " is not a function");
        sb.append("fun ").append(f.label.replace("this/","")).append(" [");
        Iterator<Decl> declIterator = f.decls.iterator();
        while (declIterator.hasNext()) {
            Decl currentDecl = declIterator.next();
            Iterator<? extends ExprHasName> varNamesIterator =  currentDecl.names.iterator();
            while (varNamesIterator.hasNext()) {
                sb.append(varNamesIterator.next().label);
                if (varNamesIterator.hasNext())
                    sb.append(", ");
            }
            String type = currentDecl.expr.type().toString();
            if (currentDecl.expr.type().arity() == 1) {
                type = type.replaceAll("\\{","").replaceAll("}", "");
            }
            sb.append(" : ").append(type);
            if (declIterator.hasNext())
                sb.append(", ");
        }
        sb.append("] : ");
        visitThis(f.returnDecl);
        sb.append(" {\n");
        indent++;
        addIndent();
        checkBlockAndVisit(f.getBody());
        indent--;
        sb.append("\n}\n");
    }

    public void visitAssertion(Expr assertion, String assertionName) throws Err {
        if (assertion == null)
            throw new IllegalArgumentException("Assertion is null");
        if (assertionName == null)
            throw new IllegalArgumentException("Assertion's name is null");
        sb.append("assert ").append(assertionName).append(" {\n");
        indent++;
        addIndent();
        checkBlockAndVisit(assertion);
        indent--;
        sb.append("\n}\n");
    }

    public void visitFact(Expr fact, String factName) throws Err {
        if (fact == null)
            throw new IllegalArgumentException("Fact is null");
        if (factName == null)
            throw new IllegalArgumentException("Fact's name is null");
        sb.append("fact ").append(factName).append(" {\n");
        indent++;
        addIndent();
        checkBlockAndVisit(fact);
        indent--;
        sb.append("\n}\n");
    }

    @Override
    public Void visitThis(Expr x) throws Err {
        Expr xToVisit = x;
        if (candidate != null) {
            Optional<Expr> mutatedExpr = candidate.getMutatedExpr(x);
            if (mutatedExpr.isPresent()) {
                candidate.markAsAlreadyMutated(x);
                xToVisit = mutatedExpr.get();
            }
        }
        if (x.hasCommentsPreviousLine()) {
            sb.append("--").append(x.getCommentPreviousLine()).append("\n");
            addIndent();
        }
        if (x.hasCommentsBefore()) {
            sb.append("/*").append(x.getCommentBefore()).append("*/ ");
        }
        super.visitThis(xToVisit);
        if (x.hasCommentsAfter()) {
            sb.append(" /*").append(x.getCommentAfter()).append("*/");
        }
        return null;
    }

    @Override
    public Void visit(ExprBinary x) throws Err {
        visitWithParenthesis(x.left);
        sb.append(" ");
        sb.append(x.op.toString());
        sb.append(" ");
        visitWithParenthesis(x.right);
        return null;
    }

    @Override
    public Void visit(ExprList x) throws Err {
        if (x.op.equals(ExprList.Op.AND) && x.args.size() > 2)
            printAndList(x);
        else if (x.op.equals(ExprList.Op.AND) || x.op.equals(ExprList.Op.OR)) {
            Iterator<Expr> it = x.args.iterator();
            while(it.hasNext()) {
                visitWithParenthesis(it.next());
                if (it.hasNext())
                    sb.append(" ").append(x.op.equals(ExprList.Op.AND)?"&&":"||").append(" ");
            }
        } else {
            Iterator<Expr> it = x.args.iterator();
            if (x.op.equals(ExprList.Op.DISJOINT))
                sb.append("disj");
            else
                sb.append(x.op);
            sb.append(" [");
            while (it.hasNext()) {
                visitWithParenthesis(it.next());
                if (it.hasNext())
                    sb.append(", ");
            }
            sb.append("] ");
        }
        return null;
    }

    private void printAndList(ExprList x) throws Err {
        if (!x.op.equals(ExprList.Op.AND))
            throw new Error("Calling printAnd without an AND expression");
        if (x.args.size() <= 2)
            throw new Error("Calling printAnd with less than 3 expressions");
        sb.append("{\n");
        addIndent();
        Iterator<Expr> it = x.args.iterator();
        while(it.hasNext()) {
            Expr item = it.next();
            checkBlockAndVisit(item);
            if (!it.hasNext()) {
                sb.append("\n");
                addIndentFinalBrace();
            } else {
                sb.append("\n");
                addIndent();
            }
        }
        sb.append("}");
    }

    @Override
    public Void visit(ExprCall x) throws Err {
        sb.append(x.fun.label.replace("this/",""));
        sb.append("[");
        Iterator<Expr> it = x.args.iterator();
        while(it.hasNext()) {
            checkBlockAndVisit(it.next());
            if (it.hasNext())
                sb.append(", ");
        }
        sb.append("]");
        return null;
    }

    @Override
    public Void visit(ExprConstant x) throws Err {
        sb.append(x.toString());
        return null;
    }

    @Override
    public Void visit(ExprITE x) throws Err {
        visitWithParenthesis(x.cond);
        sb.append(" => ");
        visitWithParenthesis(x.left);
        sb.append(" else ");
        visitWithParenthesis(x.right);
        return null;
    }

    @Override
    public Void visit(ExprLet x) throws Err {
        sb.append("let ");
        visitThis(x.var);
        sb.append(" = {");
        visitThis(x.expr);
        sb.append("} | ");
        checkBlockAndVisit(x.sub);
        return null;
    }

    @Override
    public Void visit(ExprQt x) throws Err {
        if (!x.op.equals(ExprQt.Op.COMPREHENSION))
            sb.append(x.op.toString()).append(" ");
        Iterator<Decl> it = x.decls.iterator();
        while(it.hasNext()) {
            Decl d = it.next();
            Iterator<? extends ExprHasName> varsIt = d.names.iterator();
            if (d.disjoint != null)
                sb.append("disj ");
            while (varsIt.hasNext()) {
                visitThis(varsIt.next());
                if (varsIt.hasNext())
                    sb.append(", ");
            }
            sb.append(" : ");
            visitThis(stripQuantifierOp(d.expr));
            if (it.hasNext())
                sb.append(", ");
        }
        sb.append(" | ");
        checkBlockAndVisit(x.sub);
        return null;
    }

    @Override
    public Void visit(ExprUnary x) throws Err {
        if (unaryOpToString(x.op) == null) {
            visitThis(x.sub);
        } else {
            sb.append(unaryOpToString(x.op));
            sb.append(" ");
            if (x.op.equals(ExprUnary.Op.CARDINALITY))
                visitWithParenthesis(x.sub, '{', '}');
            else
                visitWithParenthesis(x.sub);
        }
        return null;
    }

    @Override
    public Void visit(ExprVar x) throws Err {
        sb.append(x.toString());
        return null;
    }

    @Override
    public Void visit(Sig x) throws Err {
        sb.append(x.toString());
        return null;
    }

    @Override
    public Void visit(Sig.Field x) throws Err {
        sb.append(x.toString());
        return null;
    }

    private void addIndent() {
        for (int i = 0; i < indent; i++)
            sb.append("\t");
    }

    private void addIndentFinalBrace() {
        for (int i = 0; i < indent - 1; i++)
            sb.append("\t");
    }

    private void checkBlockAndVisit(Expr x) {
        boolean addIndent = x instanceof ExprList && ((ExprList)x).op.equals(ExprList.Op.AND) && ((ExprList)x).args.size() > 2;
        if (addIndent)
            indent++;
        visitThis(x);
        if (addIndent)
            indent--;
    }

    private void visitWithParenthesis(Expr x) {
        visitWithParenthesis(x, '(', ')');
    }

    private void visitWithParenthesis(Expr x, char open, char close) {
        if (x instanceof ExprCall || x instanceof ExprConstant || x instanceof ExprVar || x instanceof Sig || x instanceof Sig.Field) {
            checkBlockAndVisit(x);
        } else if (x instanceof ExprUnary && unaryOpToString(((ExprUnary) x).op) == null) {
            checkBlockAndVisit(x);
        } else {
            sb.append(open);
            checkBlockAndVisit(x);
            sb.append(close);
        }
    }

    private String unaryOpToString(ExprUnary.Op op) {
        switch (op) {
            case SOMEOF:
            case SOME:
                return "some";
            case LONEOF:
            case LONE:
                return "lone";
            case ONEOF:
            case ONE:
                return "one";
            case SETOF: return "set";
            case EXACTLYOF: return "exactly";
            case NOT: return "!";
            case NO: return "no";
            case TRANSPOSE: return "~";
            case RCLOSURE: return "*";
            case CLOSURE: return "^";
            case CARDINALITY: return "#";
            case CAST2INT:
            case NOOP:
            case CAST2SIGINT:
                return null;
        }
        return null;
    }

    private Expr stripQuantifierOp(Expr qtBoundExpr) {
        if (qtBoundExpr instanceof ExprUnary) {
            ExprUnary qtBoundExprAsUnary = (ExprUnary) qtBoundExpr;
            switch (qtBoundExprAsUnary.op) {
                case SOMEOF:
                case LONEOF:
                case ONEOF:
                case SETOF:
                case EXACTLYOF:
                case NO:
                case SOME:
                case LONE:
                case ONE:
                case NOOP: return stripQuantifierOp(qtBoundExprAsUnary.sub);
                default: return qtBoundExprAsUnary;
            }
        } else {
            return qtBoundExpr;
        }
    }

}

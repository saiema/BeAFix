package ar.edu.unrc.dc.mutation.visitors;

import ar.edu.unrc.dc.mutation.mutantLab.Candidate;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.*;

import java.util.Iterator;
import java.util.Optional;

public class ExprToString extends VisitReturn<Void> {

    private StringBuilder sb;
    private int indent;
    private Candidate candidate;

    public ExprToString(Candidate candidate) {
        sb = new StringBuilder();
        indent = 0;
        this.candidate = candidate;
    }

    public String getStringRepresentation() {
        return sb.toString();
    }

    @Override
    public Void visitThis(Expr x) throws Err {
        Optional<Expr> mutatedExpr = candidate.getMutatedExpr(x);
        if (mutatedExpr.isPresent()) {
            candidate.markAsAlreadyMutated(x);
            return visitThis(mutatedExpr.get());
        }
        return super.visitThis(x);
    }

    @Override
    public Void visit(ExprBinary x) throws Err {
        addIndent();
        visitThis(x.left);
        sb.append(" ");
        sb.append(x.op.toString());
        sb.append(" ");
        visitThis(x.right);
        return null;
    }

    @Override
    public Void visit(ExprList x) throws Err {
        addIndent();
        Iterator<Expr> it = x.args.iterator();
        sb.append(x.op);
        sb.append(" [");
        while(it.hasNext()) {
            visitThis(it.next());
            if (it.hasNext())
                sb.append(",");
        }
        sb.append("] ");
        return null;
    }

    @Override
    public Void visit(ExprCall x) throws Err {
        addIndent();
        sb.append(x.fun.label);
        sb.append("(");
        Iterator<Expr> it = x.args.iterator();
        while(it.hasNext()) {
            visitThis(it.next());
            if (it.hasNext())
                sb.append(",");
        }
        sb.append("(");
        return null;
    }

    @Override
    public Void visit(ExprConstant x) throws Err {
        addIndent();
        sb.append(x.toString());
        return null;
    }

    @Override
    public Void visit(ExprITE x) throws Err {
        addIndent();
        sb.append("if (");
        visitThis(x.cond);
        sb.append(") {\n");
        indent++;
        visitThis(x.left);
        indent--;
        addIndent();
        sb.append("} else {\n");
        indent++;
        visitThis(x.right);
        indent--;
        sb.append("}\n");
        return null;
    }

    @Override
    public Void visit(ExprLet x) throws Err {
        addIndent();
        sb.append("let ");
        visitThis(x.var);
        sb.append(" = ");
        visitThis(x.expr);
        sb.append(" | ");
        visitThis(x.sub);
        return null;
    }

    @Override
    public Void visit(ExprQt x) throws Err {
        addIndent();
        sb.append(x.op.toString()).append(" ");
        Iterator<Decl> it = x.decls.iterator();
        while(it.hasNext()) {
            Decl d = it.next();
            Iterator<? extends ExprHasName> varsIt = d.names.iterator();
            while (varsIt.hasNext()) {
                visitThis(varsIt.next());
                if (varsIt.hasNext())
                    sb.append(",");
            }
            sb.append(" : ");
            visitThis(d.expr);
            if (it.hasNext())
                sb.append(",");
        }
        sb.append(" | ");
        int oldIndent = indent;
        indent = 0;
        visitThis(x.sub);
        indent = oldIndent;
        return null;
    }

    @Override
    public Void visit(ExprUnary x) throws Err {
        addIndent();
        sb.append(x.op.toString()).append(" ");
        int oldIndent = indent;
        indent = 0;
        visitThis(x.sub);
        indent = oldIndent;
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

}

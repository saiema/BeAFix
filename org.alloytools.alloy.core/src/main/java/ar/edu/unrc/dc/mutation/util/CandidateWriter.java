package ar.edu.unrc.dc.mutation.util;

import ar.edu.unrc.dc.mutation.mutantLab.Candidate;
import ar.edu.unrc.dc.mutation.mutantLab.MutantLab;
import ar.edu.unrc.dc.mutation.visitors.ExprToStringNicePrint;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.ast.*;
import edu.mit.csail.sdg.parser.CompModule;

import java.util.Iterator;

public class CandidateWriter {

    private final Candidate candidate;

    public CandidateWriter(Candidate candidate) {
        if (candidate == null)
            throw new IllegalArgumentException("Candidate can't be null");
        this.candidate = candidate;
    }

    StringBuilder sb;
    public String candidateStringRepresentation() {
        if (!MutantLab.getInstance().applyCandidateToAst(candidate)) {
            throw new IllegalStateException("Failed to apply repair to ast");
        }
        sb = new StringBuilder();
        CompModule root = candidate.getContext();
        for (Sig s : root.getAllSigs()) {
            writeSig(s);
            sb.append("\n");
        }
        for (Pair<String, Expr> fact : root.getAllFacts()) {
            ExprToStringNicePrint exprToString = new ExprToStringNicePrint(candidate, true);
            exprToString.visitFact(fact.b, fact.a);
            sb.append(exprToString.getStringRepresentation());
            sb.append("\n");
        }
        for (Func f : root.getAllFunc()) {
            if (f.isGenerated())
                continue;
            ExprToStringNicePrint exprToString = new ExprToStringNicePrint(candidate, true);
            if (f.isPred)
                exprToString.visitPredicate(f);
            else
                exprToString.visitFunction(f);
            sb.append(exprToString.getStringRepresentation());
            sb.append("\n");
        }
        for (Pair<String, Expr> assertions : root.getAllAssertions()) {
            ExprToStringNicePrint exprToString = new ExprToStringNicePrint(candidate, true);
            exprToString.visitAssertion(assertions.b, assertions.a);
            sb.append(exprToString.getStringRepresentation());
            sb.append("\n");
        }
        for (Command c : root.getAllCommands()) {
            if (c.isGenerated())
                continue;
            sb.append(c.toString()).append("\n");
        }
        sb.append("\n");
        if (!MutantLab.getInstance().undoChangesToAst()) {
            throw new IllegalStateException("Failed to undo repair to ast");
        }
        return sb.toString();
    }

    private void writeSig(Sig s) {
        if (s.builtin)
            sb.append("builtin ");
        if (s.isPrivate != null)
            sb.append("private ");
        if (s.isAbstract != null)
            sb.append("abstract ");
        if (s.isLone != null)
            sb.append("lone ");
        if (s.isOne != null)
            sb.append("one ");
        if (s.isSome != null)
            sb.append("some ");
        if (s.isMeta != null)
            sb.append("meta ");
        if (s.isEnum != null)
            sb.append("enum ");
        sb.append("sig").append(" ").append(s.toString());
        if (s instanceof Sig.PrimSig && ((Sig.PrimSig)s).parent != null && !((Sig.PrimSig)s).parent.builtin) {
            sb.append(" extends ").append(((Sig.PrimSig)s).parent.toString());
        } else if (s instanceof Sig.SubsetSig) {
            Iterator<Sig> parents = ((Sig.SubsetSig)s).parents.iterator();
            sb.append(" in ");
            while (parents.hasNext()) {
                sb.append(parents.next().toString());
                if (parents.hasNext()) {
                    sb.append(" + ");
                }
            }
        }
        sb.append(" {\n");
        indent++;
        Iterator<Decl> fields = s.getFieldDecls().iterator();
        while (fields.hasNext()) {
            writeField(fields.next());
            if (fields.hasNext())
                sb.append(",");
            sb.append("\n");
        }
        addIndentFinalBrace();
        sb.append("}");
        Iterator<Expr> facts = s.getFacts().iterator();
        if (facts.hasNext()) {
            sb.append(" {\n");
            while(facts.hasNext()) {
                ExprToStringNicePrint exprToString = new ExprToStringNicePrint(candidate, true, indent);
                exprToString.visitThis(facts.next());
                sb.append(exprToString.getStringRepresentation());
                sb.append("\n");
            }
            addIndentFinalBrace();
            sb.append("}\n");
        }
        indent--;
        sb.append("\n");
    }

    private void writeField(Decl f) {
        addIndent();
        Iterator<? extends ExprHasName> varNamesIterator =  f.names.iterator();
        while (varNamesIterator.hasNext()) {
            sb.append(varNamesIterator.next().label);
            if (varNamesIterator.hasNext())
                sb.append(", ");
        }
        sb.append(" : ").append(f.expr.toString());
    }


    private int indent;
    private void addIndent() {
        for (int i = 0; i < indent; i++)
            sb.append("\t");
    }

    private void addIndentFinalBrace() {
        for (int i = 0; i < indent - 1; i++)
            sb.append("\t");
    }

}

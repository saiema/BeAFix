package ar.edu.unrc.dc.mutation.op;

import static ar.edu.unrc.dc.mutation.Cheats.cheatedClone;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import ar.edu.unrc.dc.mutation.CheatingIsBadMkay;
import ar.edu.unrc.dc.mutation.Mutation;
import ar.edu.unrc.dc.mutation.Mutator;
import ar.edu.unrc.dc.mutation.Ops;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.ExprBinary;
import edu.mit.csail.sdg.ast.ExprConstant;
import edu.mit.csail.sdg.ast.ExprQt;
import edu.mit.csail.sdg.ast.ExprUnary;
import edu.mit.csail.sdg.parser.CompModule;

/**
 * Arithmetic Expression Constant Replacement based in
 * <a href="https://pitest.org/quickstart/mutators/#EXPERIMENTAL_CRCR">PIT CRCR
 * operator</a>
 * <p>
 * Given an arithmetic expression c, this operator will generate the following
 * mutations:
 *
 * <li>1</li>
 * <li>0</li>
 * <li>-1</li>
 * <li>-c</li>
 * <li>c+1</li>
 * <li>c-1</li>
 *
 */
public class AECR extends Mutator {

    public AECR(CompModule context) {
        super(context);
    }



    @Override
    public Optional<List<Mutation>> visit(ExprBinary x) throws Err {
        if (isArithmeticBinaryExpression(x)) {
            return mutations(x);
        }
        return EMPTY;
    }



    @Override
    public Optional<List<Mutation>> visit(ExprConstant x) throws Err {
        if (x.op.equals(ExprConstant.Op.NUMBER)) {
            return mutations(x);
        }
        return EMPTY;
    }



    @Override
    public Optional<List<Mutation>> visit(ExprUnary x) throws Err {
        if (isArithmeticUnaryExpression(x)) {
            return mutations(x);
        }
        return EMPTY;
    }

    @Override
    public Optional<List<Mutation>> visit(ExprQt x) throws Err {
        if (x.op.equals(ExprQt.Op.SUM)) {
            return mutations(x);
        }
        return EMPTY;
    }

    private boolean isArithmeticUnaryExpression(ExprUnary x) {
        switch (x.op) {
            case CARDINALITY :
            case CAST2INT :
            case CAST2SIGINT :
                return true;
            default :
                return false;
        }
    }

    private Optional<List<Mutation>> mutations(Expr x) throws Err {
        List<Mutation> mutations;
        try {
            mutations = generateMutants(x);
            if (!mutations.isEmpty()) {
                return Optional.of(mutations);
            }
        } catch (CheatingIsBadMkay e) {
            throw new Error("There was a problem obtaining mutations", e);
        }
        return EMPTY;
    }

    private List<Mutation> generateMutants(Expr x) throws CheatingIsBadMkay {
        List<Mutation> mutations = new LinkedList<>();
        if (isConstant(x)) {
            if (isNeg(x)) { //-c => c
                ExprConstant original = (ExprConstant) x;
                ExprConstant mutant = ExprConstant.Op.NUMBER.make(x.pos, Integer.parseInt(x.toString().substring(1)));
                mutations.add(new Mutation(whoiam(), original, mutant));
            } else {
                if (!isZero(x)) { //c => 0 ; c => -c
                    mutations.add(new Mutation(whoiam(), x, makeZero(x.pos)));
                    ExprConstant mutant = ExprConstant.Op.NUMBER.make(x.pos, Integer.parseInt("-" + x.toString()));
                    mutations.add(new Mutation(whoiam(), x, mutant));
                }
                if (!isOne(x)) //c => 1
                    mutations.add(new Mutation(whoiam(), x, makeOne(x.pos)));
            }
        } else { //E => 0 ; E => 1
            mutations.add(new Mutation(whoiam(), x, makeZero(x.pos)));
            mutations.add(new Mutation(whoiam(), x, makeOne(x.pos)));
        }
        if (!(isConstant(x) && isZero(x))) { //E => E + 1
            Expr original = cheatedClone(x, x.type());
            ExprBinary mutant = (ExprBinary) ExprBinary.Op.IPLUS.make(x.pos, x.closingBracket, original, makeOne(null));
            mutations.add(new Mutation(whoiam(), original, mutant));
        }
        if (!(isConstant(x) && isOne(x))) { //E => E - 1
            Expr original = cheatedClone(x, x.type());
            ExprBinary mutant = (ExprBinary) ExprBinary.Op.IMINUS.make(x.pos, x.closingBracket, original, makeOne(null));
            mutations.add(new Mutation(whoiam(), original, mutant));
        }
        return mutations;
    }

    private boolean isNeg(Expr x) {
        if (x instanceof ExprConstant) {
            return ((ExprConstant) x).toString().startsWith("-");
        }
        return false;
    }

    private boolean isConstant(Expr x) {
        return x instanceof ExprConstant;
    }

    private boolean isZero(Expr x) {
        if (isConstant(x)) {
            ExprConstant c = (ExprConstant) x;
            return c.toString().compareTo("0") == 0;
        }
        return false;
    }

    private boolean isOne(Expr x) {
        if (isConstant(x)) {
            ExprConstant c = (ExprConstant) x;
            return c.toString().compareTo("1") == 0;
        }
        return false;
    }

    private ExprConstant makeZero(Pos pos) {
        return ExprConstant.Op.NUMBER.make(pos, 0);
    }

    private ExprConstant makeOne(Pos pos) {
        return ExprConstant.Op.NUMBER.make(pos, 1);
    }

    @Override
    protected Ops whoiam() {
        return Ops.AECR;
    }

}

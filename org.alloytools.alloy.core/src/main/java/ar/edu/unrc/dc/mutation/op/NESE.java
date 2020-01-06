package ar.edu.unrc.dc.mutation.op;

import ar.edu.unrc.dc.mutation.Mutator;
import ar.edu.unrc.dc.mutation.Ops;
import edu.mit.csail.sdg.parser.CompModule;

/**
 * Non Empty Set Extender
 * <p>
 * Given an expression A, this operator will add, for each variable or join
 * expression {@code x}, {@code some x} => A
 * <p>
 * For example
 *
 * <pre>
 * some list': List | list'.rest = list and list'.element = e
 * </pre>
 *
 * Would be mutated to
 *
 * <pre>
 * some list': List | (some list'.rest && some list && some list'.element && some e) => list'.rest = list and list'.element = e
 * </pre>
 *
 */
public class NESE extends Mutator {

    public NESE(CompModule context) {
        super(context);
    }

    @Override
    protected Ops whoiam() {
        return Ops.NESE;
    }

}

package ar.edu.unrc.dc.mutation;

import edu.mit.csail.sdg.parser.CompModule;

public enum Ops {

                 AORB {

                     @Override
                     public boolean isImplemented() {
                         return true;
                     }

                     @Override
                     public Mutator getOperator(CompModule context) {
                         return new ar.edu.unrc.dc.mutation.op.AORB(context);
                     }
                 },
                 AORU {

                     @Override
                     public boolean isImplemented() {
                         return false;
                     }

                 },
                 ROR {

                     @Override
                     public boolean isImplemented() {
                         return true;
                     }

                     @Override
                     public Mutator getOperator(CompModule context) {
                         return new ar.edu.unrc.dc.mutation.op.ROR(context);
                     }
                 },
                 COR {

                     @Override
                     public boolean isImplemented() {
                         return true;
                     }

                     @Override
                     public Mutator getOperator(CompModule context) {
                         return new ar.edu.unrc.dc.mutation.op.COR(context);
                     }
                 },
                 BES {

                     @Override
                     public boolean isImplemented() {
                         return true;
                     }

                     @Override
                     public Mutator getOperator(CompModule context) {
                         return new ar.edu.unrc.dc.mutation.op.BES(context);
                     }
                 },
                 BEE {

                     @Override
                     public boolean isImplemented() {
                         return false;
                     }

                 },
                 JER {

                     @Override
                     public boolean isImplemented() {
                         return false;
                     }

                 },
                 RUOR {

                     @Override
                     public boolean isImplemented() {
                         return true;
                     }

                     @Override
                     public Mutator getOperator(CompModule context) {
                         return new ar.edu.unrc.dc.mutation.op.RUOR(context);
                     }
                 },
                 RUOI {

                     @Override
                     public boolean isImplemented() {
                         return true;
                     }

                     @Override
                     public Mutator getOperator(CompModule context) {
                         return new ar.edu.unrc.dc.mutation.op.RUOI(context);
                     }
                 };

    public abstract boolean isImplemented();

    public Mutator getOperator(CompModule context) {
        throw new UnsupportedOperationException("Operator not implemented");
    }

}

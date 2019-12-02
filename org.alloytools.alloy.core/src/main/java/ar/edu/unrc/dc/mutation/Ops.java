package ar.edu.unrc.dc.mutation;


public enum Ops {

                 AORB {

                     @Override
                     public boolean isImplemented() {
                         return true;
                     }

                     @Override
                     public Mutator getOperator() {
                         return new ar.edu.unrc.dc.mutation.op.AORB();
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
                     public Mutator getOperator() {
                         return new ar.edu.unrc.dc.mutation.op.ROR();
                     }
                 },
                 COR {

                     @Override
                     public boolean isImplemented() {
                         return true;
                     }

                     @Override
                     public Mutator getOperator() {
                         return new ar.edu.unrc.dc.mutation.op.COR();
                     }
                 },
                 BES {

                     @Override
                     public boolean isImplemented() {
                         return true;
                     }

                     @Override
                     public Mutator getOperator() {
                         return new ar.edu.unrc.dc.mutation.op.BES();
                     }
                 },
                 BEE {

                     @Override
                     public boolean isImplemented() {
                         return false;
                     }

                 },
                 PRV {

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
                     public Mutator getOperator() {
                         return new ar.edu.unrc.dc.mutation.op.RUOR();
                     }
                 },
                 RUOI {

                     @Override
                     public boolean isImplemented() {
                         return true;
                     }

                     @Override
                     public Mutator getOperator() {
                         return new ar.edu.unrc.dc.mutation.op.RUOI();
                     }
                 };

    public abstract boolean isImplemented();

    public Mutator getOperator() {
        throw new UnsupportedOperationException("Operator not implemented");
    }

}

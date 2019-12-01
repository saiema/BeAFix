package ar.edu.unrc.dc.mutation;


public enum Ops {

                 AORB {

                     @Override
                     public boolean isImplemented() {
                         return true;
                     }
                 },
                 AORU {

                     @Override
                     public boolean isImplemented() {
                         return true;
                     }
                 },
                 ROR {

                     @Override
                     public boolean isImplemented() {
                         return true;
                     }
                 },
                 COR {

                     @Override
                     public boolean isImplemented() {
                         return true;
                     }
                 },
                 BES {

                     @Override
                     public boolean isImplemented() {
                         return true;
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
                 },
                 RUOI {

                     @Override
                     public boolean isImplemented() {
                         return false;
                     }
                 };

    public abstract boolean isImplemented();

}

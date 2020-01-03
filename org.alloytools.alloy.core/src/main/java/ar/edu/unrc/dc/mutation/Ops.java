package ar.edu.unrc.dc.mutation;

import ar.edu.unrc.dc.mutation.op.AECR;
import ar.edu.unrc.dc.mutation.op.AORB;
import ar.edu.unrc.dc.mutation.op.BES;
import ar.edu.unrc.dc.mutation.op.COR;
import ar.edu.unrc.dc.mutation.op.MOR;
import ar.edu.unrc.dc.mutation.op.QTBER;
import ar.edu.unrc.dc.mutation.op.QTOR;
import ar.edu.unrc.dc.mutation.op.ROR;
import ar.edu.unrc.dc.mutation.op.RUOI;
import ar.edu.unrc.dc.mutation.op.RUOR;
import edu.mit.csail.sdg.parser.CompModule;

public enum Ops {

                 AORB {

                     @Override
                     public boolean isImplemented() {
                         return true;
                     }

                     @Override
                     public Mutator getOperator(CompModule context) {
                         return new AORB(context);
                     }
                 },
                 ROR {

                     @Override
                     public boolean isImplemented() {
                         return true;
                     }

                     @Override
                     public Mutator getOperator(CompModule context) {
                         return new ROR(context);
                     }
                 },
                 COR {

                     @Override
                     public boolean isImplemented() {
                         return true;
                     }

                     @Override
                     public Mutator getOperator(CompModule context) {
                         return new COR(context);
                     }
                 },
                 CUOI {

                     @Override
                     public boolean isImplemented() {
                         return false;
                     }

                     @Override
                     public Mutator getOperator(CompModule context) {
                         return new ar.edu.unrc.dc.mutation.op.CUOI(context);
                     }

                 },
                 BES {

                     @Override
                     public boolean isImplemented() {
                         return true;
                     }

                     @Override
                     public Mutator getOperator(CompModule context) {
                         return new BES(context);
                     }
                 },
                 BESOR {

                     @Override
                     public boolean isImplemented() {
                         return true;
                     }

                     @Override
                     public Mutator getOperator(CompModule context) {
                         return new ar.edu.unrc.dc.mutation.op.BESOR(context);
                     }
                 },
                 JER {

                     @Override
                     public boolean isImplemented() {
                         return true;
                     }

                     @Override
                     public Mutator getOperator(CompModule context) {
                         return new ar.edu.unrc.dc.mutation.op.JER(context);
                     }

                 },
                 JES {

                     @Override
                     public boolean isImplemented() {
                         return true;
                     }

                     @Override
                     public Mutator getOperator(CompModule context) {
                         return new ar.edu.unrc.dc.mutation.op.JES(context);
                     }

                 },
                 JEE {

                     @Override
                     public boolean isImplemented() {
                         return true;
                     }

                     @Override
                     public Mutator getOperator(CompModule context) {
                         return new ar.edu.unrc.dc.mutation.op.JEE(context);
                     }

                 },
                 RUOR {

                     @Override
                     public boolean isImplemented() {
                         return true;
                     }

                     @Override
                     public Mutator getOperator(CompModule context) {
                         return new RUOR(context);
                     }
                 },
                 RUOI {

                     @Override
                     public boolean isImplemented() {
                         return true;
                     }

                     @Override
                     public Mutator getOperator(CompModule context) {
                         return new RUOI(context);
                     }
                 },
                 MOR {

                     @Override
                     public boolean isImplemented() {
                         return true;
                     }

                     @Override
                     public Mutator getOperator(CompModule context) {
                         return new MOR(context);
                     }
                 },
                 QTOR {

                     @Override
                     public boolean isImplemented() {
                         return true;
                     }

                     @Override
                     public Mutator getOperator(CompModule context) {
                         return new QTOR(context);
                     }
                 },
                 QTBER {

                     @Override
                     public boolean isImplemented() {
                         return true;
                     }

                     @Override
                     public Mutator getOperator(CompModule context) {
                         return new QTBER(context);
                     }

                 },
                 AECR {

                     @Override
                     public boolean isImplemented() {
                         return true;
                     }

                     @Override
                     public Mutator getOperator(CompModule context) {
                         return new AECR(context);
                     }

                 },
                 MULTI { //this operator is used when compressing mutations

                     @Override
                     public boolean isImplemented() {
                         return false;
                     }
                 };

    public abstract boolean isImplemented();

    public Mutator getOperator(CompModule context) {
        throw new UnsupportedOperationException("Operator not implemented");
    }

}

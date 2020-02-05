package ar.edu.unrc.dc.mutation;

import ar.edu.unrc.dc.mutation.op.*;
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

                     @Override
                     public int getComplexity() {
                         return 2;
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

                     @Override
                     public int getComplexity() {
                         return 1;
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

                     @Override
                     public int getComplexity() {
                         return 1;
                     }

                 },
                 CUOI {

                     @Override
                     public boolean isImplemented() {
                         return true;
                     }

                     @Override
                     public Mutator getOperator(CompModule context) {
                         return new ar.edu.unrc.dc.mutation.op.CUOI(context);
                     }

                     @Override
                     public int getComplexity() {
                         return 1;
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

                     @Override
                     public int getComplexity() {
                         return 1;
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

                     @Override
                     public int getComplexity() {
                         return 2;
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

                     @Override
                     public int getComplexity() {
                         return 3;
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

                     @Override
                     public int getComplexity() {
                         return 3;
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

                     @Override
                     public int getComplexity() {
                         return 3;
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

                     @Override
                     public int getComplexity() {
                         return 2;
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

                     @Override
                     public int getComplexity() {
                         return 2;
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

                     @Override
                     public int getComplexity() {
                         return 2;
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

                     @Override
                     public int getComplexity() {
                         return 2;
                     }

                 },
                 QTOI {

                     @Override
                     public boolean isImplemented() {
                         return true;
                     }

                     @Override
                     public Mutator getOperator(CompModule context) {
                         return new QTOI(context);
                     }

                     @Override
                     public int getComplexity() {
                         return 2;
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

                     @Override
                     public int getComplexity() {
                         return 4;
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

                     @Override
                     public int getComplexity() {
                         return 2;
                     }

                 },
                 EMOR {

                     @Override
                     public boolean isImplemented() {
                         return true;
                     }

                     @Override
                     public Mutator getOperator(CompModule context) {
                         return new EMOR(context);
                     }

                     @Override
                     public int getComplexity() {
                         return 2;
                     }

                 },
                 NESE {

                     @Override
                     public boolean isImplemented() {
                         return true;
                     }

                     @Override
                     public Mutator getOperator(CompModule context) { return new NESE(context); }

                     @Override
                     public int getComplexity() {
                         return 4;
                     }

                 },
                 SSE {

                     @Override
                     public boolean isImplemented() {
                         return true;
                     }

                     @Override
                     public Mutator getOperator(CompModule context) { return new SSE(context); }

                     @Override
                     public int getComplexity() {
                         return 5;
                     }

                 },
                 MULTI { //this operator is used when compressing mutations

                     @Override
                     public boolean isImplemented() {
                         return false;
                     }

                     @Override
                     public int getComplexity() {
                         return 10;
                     }

                 };

    public abstract boolean isImplemented();

    public Mutator getOperator(CompModule context) {
        throw new UnsupportedOperationException("Operator not implemented");
    }

    public abstract int getComplexity();

}

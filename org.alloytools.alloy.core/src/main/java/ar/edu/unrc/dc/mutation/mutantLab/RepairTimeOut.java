package ar.edu.unrc.dc.mutation.mutantLab;

import java.util.TimerTask;

public class RepairTimeOut extends TimerTask {

    @Override
    public void run() {
        MutantLab.getInstance().timeout();
    }

}

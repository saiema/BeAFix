package ar.edu.unrc.dc.mutation.mutantLab;

import java.util.concurrent.TimeUnit;

public class RepairTimeOut {

    private static RepairTimeOut instance;
    public static void initialize(long timeBudget) {
        if (instance == null)
            instance = new RepairTimeOut(timeBudget);
        else
            throw new IllegalStateException("Instance already instantiated");
    }
    public static RepairTimeOut getInstance() {
        if (instance == null)
            throw new IllegalStateException("Run initialize first");
        return instance;
    }
    public static void destroyInstance() {
        if (instance == null)
            throw new IllegalStateException("No instance to destroy");
        instance = null;
    }

    private long startTime;
    private long timeBudget;
    private final boolean noTimeout;

    /**
     * Creates a new instance of {@code RepairTimeOut}
     *
     * @param timeBudget    :   the time budget
     * @param timeUnit      :   the time unit used to interpret the time budget
     */
    private RepairTimeOut(long timeBudget, TimeUnit timeUnit) {
        if (timeBudget < 0)
            throw new IllegalArgumentException("Time budget can't be a negative value (" + timeBudget + ")");
        noTimeout = timeBudget == 0;
        if (!noTimeout) {
            this.timeBudget = timeUnit.toNanos(timeBudget);
        }
    }

    /**
     * Creates an instance of {@code RepairTimeOut} using a time budget in milliseconds
     *
     * @param timeBudget    :   the time budget in milliseconds
     */
    private RepairTimeOut(long timeBudget) {
        this(timeBudget, TimeUnit.MILLISECONDS);
    }

    public void start() {
        this.startTime = System.nanoTime();
    }

    public boolean timeoutReached() {
        if (noTimeout)
            return false;
        long now = System.nanoTime();
        long elapsedTime = now - startTime;
        return elapsedTime >= timeBudget;
    }

}

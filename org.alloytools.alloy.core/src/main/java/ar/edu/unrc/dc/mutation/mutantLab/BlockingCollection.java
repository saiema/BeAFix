package ar.edu.unrc.dc.mutation.mutantLab;

import java.io.IOException;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.logging.FileHandler;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

public class BlockingCollection<V> {

    private static final Logger logger = Logger.getLogger(BlockingCollection.class.getName());

    static {
        try {
            // This block configure the logger with handler and formatter
            FileHandler fh = new FileHandler("BlockingCollection.log");
            logger.addHandler(fh);
            SimpleFormatter formatter = new SimpleFormatter();
            fh.setFormatter(formatter);
        } catch (SecurityException | IOException e) {
            e.printStackTrace();
        }
    }

    private List<V> internalCollection;
    private INSERTION_POLICY insertionPolicy;
    public enum INSERTION_POLICY {
        FIFO, FILO
    }
    private static final INSERTION_POLICY DEFAULT_INSERTION_POLICY = INSERTION_POLICY.FIFO;
    private long timeout = 0;
    private List<Object> decreaseAssociatedObjects;
    private List<Object> increaseAssociatedObjects;

    public BlockingCollection() {
        this(DEFAULT_INSERTION_POLICY, 0);
    }

    public BlockingCollection(INSERTION_POLICY insertionPolicy) {
        this(insertionPolicy, 0);
    }

    public BlockingCollection(long timeout) {
        this(DEFAULT_INSERTION_POLICY, timeout);
    }

    public BlockingCollection(INSERTION_POLICY insertionPolicy, long timeout) {
        if (insertionPolicy == null)
            throw new IllegalArgumentException("insertionPolicy can't be null");
        if (timeout < 0)
            throw new IllegalArgumentException("timeout can't be a negative value");
        this.insertionPolicy = insertionPolicy;
        internalCollection = new LinkedList<>();
        this.timeout = timeout;
        decreaseAssociatedObjects = new LinkedList<>();
        increaseAssociatedObjects = new LinkedList<>();
    }


    public synchronized void insertBulk(Collection<V> values) {
        logger.info("insertBulk with " + values.size() + " elements");
        for (V value : values) {
            switch (insertionPolicy) {
                case FIFO: {
                    internalCollection.add(internalCollection.size(), value);
                    break;
                }
                case FILO: {
                    internalCollection.add(0, value);
                    break;
                }
            }
        }
        logger.info("insertBulk finished");
        notifyAll();
        notifyAllIncreaseAssociatedObjects();
    }

    public synchronized void unlockAllWaitingThreads() {
        notifyAll();
    }

    public synchronized void insert(V value) {
        logger.info("inserting value");
        switch (insertionPolicy) {
            case FIFO: {
                internalCollection.add(internalCollection.size(), value);
                break;
            }
            case FILO: {
                internalCollection.add(0, value);
                break;
            }
        }
        logger.info("value inserted");
        notifyAll();
        notifyAllIncreaseAssociatedObjects();
    }

    private synchronized void notifyAllIncreaseAssociatedObjects() {
        logger.info("notifyAllIncreaseAssociatedObjects");
        for (Object o : increaseAssociatedObjects)
            synchronized (o) {
                o.notify();
            }
        logger.info("notifyAllIncreaseAssociatedObjects finished");
    }

    public synchronized void addIncreaseAssociatedObject(Object o) {
        if (o == null)
            throw new IllegalArgumentException("associated object can't be null");
        if (this == o)
            return;
        if (increaseAssociatedObjects.contains(o))
            return;
        increaseAssociatedObjects.add(o);
    }

    private synchronized void notifyAllDecreaseAssociatedObjects() {
        logger.info("notifyAllDecreaseAssociatedObjects");
        for (Object o : decreaseAssociatedObjects) {
            synchronized (o) {
                o.notify();
            }
        }
        logger.info("notifyAllDecreaseAssociatedObjects finished");
    }

    public synchronized void addDecreaseAssociatedObject(Object o) {
        if (o == null)
            throw new IllegalArgumentException("associated object can't be null");
        if (this == o)
            return;
        if (decreaseAssociatedObjects.contains(o))
            return;
        decreaseAssociatedObjects.add(o);
    }

    public synchronized Optional<V> current() throws InterruptedException {
        logger.info("current called");
        if (isEmpty()) {
            wait(timeout);
        }
        if (isEmpty()) {
            logger.info("returning empty");
            return Optional.empty();
        }
        logger.info("returning value at index 0");
        return Optional.of(internalCollection.get(0));
    }

    public synchronized Optional<V> getAndAdvance() throws InterruptedException {
        logger.info("getAndAdvance called");
        if (isEmpty()) {
            wait(timeout);
        }
        Optional<V> res;
        if (isEmpty()) {
            res = Optional.empty();
            logger.info("returning empty");
        } else {
            res = Optional.of(internalCollection.remove(0));
            logger.info("returning (and removing) value at index 0");
        }
        notifyAllDecreaseAssociatedObjects();
        return res;
    }

    public synchronized boolean isEmpty() {
        return internalCollection.isEmpty();
    }

    public synchronized int size() {
        return internalCollection.size();
    }

}

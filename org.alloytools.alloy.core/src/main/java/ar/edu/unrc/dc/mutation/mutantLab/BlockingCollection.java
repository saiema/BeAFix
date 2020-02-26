package ar.edu.unrc.dc.mutation.mutantLab;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

public class BlockingCollection<V> {

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
        for (V value : values) {
//            if (internalCollection.contains(value))
//                return;
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
        notify();
        notifyAllIncreaseAssociatedObjects();
    }

    public synchronized void insert(V value) {
//        if (internalCollection.contains(value))
//            return;
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
        notify();
        notifyAllIncreaseAssociatedObjects();
    }

    private synchronized void notifyAllIncreaseAssociatedObjects() {
        for (Object o : increaseAssociatedObjects)
            synchronized (o) {
                o.notify();
            }
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
        for (Object o : decreaseAssociatedObjects) {
            synchronized (o) {
                o.notify();
            }
        }
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
        if (isEmpty())
            wait(timeout);
        if (isEmpty())
            return Optional.empty();
        return Optional.of(internalCollection.get(0));
    }

    public synchronized Optional<V> getAndAdvance() throws InterruptedException {
        if (isEmpty())
            wait(timeout);
        Optional<V> res;
        if (isEmpty())
            res = Optional.empty();
        else
            res = Optional.of(internalCollection.remove(0));
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

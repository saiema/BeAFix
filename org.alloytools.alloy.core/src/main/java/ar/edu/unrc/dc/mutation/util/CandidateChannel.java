package ar.edu.unrc.dc.mutation.util;

import ar.edu.unrc.dc.mutation.mutantLab.Candidate;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

public class CandidateChannel {

    private final List<Candidate> internalCollection;
    private final List<Candidate> priorityCollection;

    public CandidateChannel() {
        internalCollection = new LinkedList<>();
        priorityCollection = new LinkedList<>();
    }

    public void addAll(Collection<Candidate> values) {
        internalCollection.addAll(values);
    }

    public void add(Candidate value) {
        internalCollection.add(value);
    }

    public void addToPriorityChannel(Candidate value) {
        priorityCollection.add(value);
    }

    public Optional<Candidate> current() {
        if (isEmpty()) {
            return Optional.empty();
        }
        if (!priorityCollection.isEmpty())
            return Optional.of(priorityCollection.get(0));
        return Optional.of(internalCollection.get(0));
    }

    public Optional<Candidate> next() {
        Optional<Candidate> res;
        if (isEmpty()) {
            res = Optional.empty();
        } else if (!priorityCollection.isEmpty()) {
            res = Optional.of(priorityCollection.remove(0));
        } else {
            res = Optional.of(internalCollection.remove(0));
        }
        return res;
    }

    public boolean isEmpty() {
        return internalCollection.isEmpty() && priorityCollection.isEmpty();
    }

    public int size() {
        return internalCollection.size() + priorityCollection.size();
    }

}

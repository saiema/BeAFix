package ar.edu.unrc.dc.mutation.util;

import ar.edu.unrc.dc.mutation.mutantLab.Candidate;

import java.util.*;

public class CandidateChannel {

    private final List<Candidate> internalCollection;
    //private final List<Candidate> priorityCollection;
    private final Map<Integer, List<Candidate>> priorityCollections;

    public CandidateChannel() {
        internalCollection = new LinkedList<>();
        //priorityCollection = new LinkedList<>();
        priorityCollections = new HashMap<>();
    }

    public void addAll(Collection<Candidate> values) {
        internalCollection.addAll(values);
    }

    public void add(Candidate value) {
        internalCollection.add(value);
    }

    public void addToPriorityChannel(Candidate value) {
        List<Candidate> list;
        if (priorityCollections.containsKey(value.getCurrentMarkedExpression())) {
            list = priorityCollections.get(value.getCurrentMarkedExpression());
        } else {
            list = new LinkedList<>();
            priorityCollections.put(value.getCurrentMarkedExpression(), list);
        }
        list.add(value);
    }

    public Optional<Candidate> current() {
        if (isEmpty()) {
            return Optional.empty();
        }
        if (!priorityCollections.isEmpty())
            return Optional.of(priorityCollections.get(maxMarkedExpression()).get(0));
        return Optional.of(internalCollection.get(0));
    }

    public Optional<Candidate> next() {
        Optional<Candidate> res;
        if (isEmpty()) {
            res = Optional.empty();
        } else if (!priorityCollections.isEmpty()) {
            res = Optional.of(nextFromPriority());//Optional.of(priorityCollection.remove(0));
        } else {
            res = Optional.of(internalCollection.remove(0));
        }
        return res;
    }

    private Candidate nextFromPriority() {
        if (priorityCollections.isEmpty())
            throw new IllegalStateException("Can't get next from priority collection since it's empty");
        int index = maxMarkedExpression();
        if (priorityCollections.get(index).isEmpty())
            throw new IllegalStateException("Empty priority list for index (" + index + ")");
        Candidate next = priorityCollections.get(index).remove(0);
        if (priorityCollections.get(index).isEmpty())
            priorityCollections.remove(index);
        return next;
    }

    private int maxMarkedExpression() {
        if (priorityCollections.isEmpty())
            throw new IllegalStateException("priority collections are empty");
        return priorityCollections.keySet().stream().max(Integer::compareTo).get();
    }

    public boolean isEmpty() {
        return internalCollection.isEmpty() && priorityCollections.isEmpty();
    }

    public int size() {
        return internalCollection.size() + priorityCollections.values().stream().mapToInt(List::size).reduce(0, Integer::sum);
    }

}

/* Alloy Analyzer 4 -- Copyright (c) 2006-2009, Felix Chang
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files
 * (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify,
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF
 * OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package edu.mit.csail.sdg.alloy4;

import java.io.Serializable;

/**
 * Immutable; stores a triplet of object references; triplet.equals() compares by
 * calling equals() on both components.
 * <p>
 * <b>Thread Safety:</b> Safe (since objects of this class are immutable).
 */

public final class Triplet<A, B, C> implements Serializable {

    /** This ensures the class can be serialized reliably. */
    private static final long serialVersionUID = 0;

    /** The first half of the triplet. */
    public final A            a;

    /** The second half of the triplet. */
    public final B            b;

    /** The third half of the triplet. */
    public final C            c;

    /** Constructs a new triplet object (a,b). */
    public Triplet(A a, B b, C c) {
        this.a = a;
        this.b = b;
        this.c = c;
    }

    /**
     * If "a" and "b" are both String, concatename them with a space; if only one
     * is, return it; else call toString() on them.
     */
    @Override
    public String toString() {
        if (a instanceof String) {
            if (b instanceof String)
                return a + " " + b;
            else
                return (String) a;
        }
        if (b instanceof String)
            return (String) b;
        if (a == null) {
            return (b != null) ? b.toString() : "";
        } else {
            return (b != null) ? (a + " " + b) : a.toString();
        }
    }

    /**
     * Returns a hashcode based on (a==null?0:a.hashCode()) and
     * (b==null?0:b.hashCode()).
     */
    @Override
    public int hashCode() {
        int i = (a == null) ? 0 : a.hashCode();
        int j = (b == null) ? 0 : b.hashCode();
        return i * 173123 + j;
    }

    /**
     * triplets (a1, b1) and (a2, b2) are equal iff (a1==null ? a2==null :
     * a1.equals(a2)) and (b1==null ? b2==null : b1.equals(b2))
     */
    @Override
    public boolean equals(Object that) {
        if (this == that)
            return true;
        if (!(that instanceof Triplet))
            return false;
        Triplet< ? , ? ,?> p = (Triplet< ? , ? ,?>) that;
        return (a == null ? p.a == null : a.equals(p.a)) && (b == null ? p.b == null : b.equals(p.b));
    }
}

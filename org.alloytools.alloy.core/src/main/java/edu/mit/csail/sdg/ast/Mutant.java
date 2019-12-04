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

package edu.mit.csail.sdg.ast;

import edu.mit.csail.sdg.alloy4.*;

import java.util.ArrayList;
import java.util.List;
import java.util.NoSuchElementException;

import static edu.mit.csail.sdg.alloy4.TableView.clean;

/**
 * Mutable; represents an expression that can be mutable for repair.
 *  */

public  class Mutant extends Browsable  {

    /**
     * The location in the original file where this mutant mark is declared;
     * never null.
     */
    public final Pos             pos;



    /** never null. */
    public  Expr            exprToMutate;



    public Mutant(Pos pos, Expr exprToMutate) throws Err {
        if (pos == null)
            pos = Pos.UNKNOWN;
        this.pos = pos;
        this.exprToMutate = exprToMutate;
        defineParentForComponents();
    }


    /**
     * Return the expression to mutate. <br>
     * If the user has not called setBody() to set the body, <br>
     * then the default body is "false" (if this is a predicate), <br>
     * or the empty set/relation of the appropriate arity (if this is a function).
     */
    public Expr getExpr() {
        return exprToMutate;
    }

    /**
     * Returns a human-readable description for this predicate/function
     */
    @Override
    public final String toString() {
        return ( "Mutant: ") + exprToMutate.toString();
    }

    /** {@inheritDoc} */
    @Override
    public final Pos pos() {
        return pos;
    }

    /** {@inheritDoc} */
    @Override
    public final Pos span() {
        return pos;
    }

    @Override
    public String getHTML() {
        return null;
    }

    @Override
    public List<? extends Browsable> getSubnodes() {
        return null;
    }


    @Override
    public void defineParentForComponents() {
        this.exprToMutate.setBrowsableParent(this);
        }
    }



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

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

/**
 * This abstract class represents a node that can be browsed in the graphical
 * parse tree viewer.
 */

public abstract class Browsable {

    private static int NEXT_ID     = 0;
    private int        mutGenLimit = 0;
    private int        ID          = NEXT_ID++;
    private Browsable  browsableParent;
    private static boolean freezeParents = false;
    private static int NEXT_ID_Env = 0;
    private int        ID_Env      = NEXT_ID_Env++;

    public void mutGenLimit(int m) {
        if (m < 0)
            throw new IllegalArgumentException("Can't use a negative value with mutGenLimit");
        this.mutGenLimit = m;
    }

    public int mutGenLimit() {
        Browsable current = this;
        while (current != null) {
            if (current.mutGenLimit > 0)
                return current.mutGenLimit;
            Browsable parent = current.getBrowsableParent();
            if (parent != null) {
                if (parent instanceof Sig || parent instanceof Sig.Field)
                    return 0;
            }
            current = parent;
        }
        return 0;
    }

    public boolean hasMutGenLimit() {
        return mutGenLimit() > 0;
    }

    public int getID() {
        return ID;
    }

    public void setID(int id) {
        this.ID = id;
    }

    public void newID() {
        this.ID = NEXT_ID++;
    }

    public int getIDEnv() {
        return this.ID_Env;
    }

    protected void setIDEnv(int id) {
        this.ID_Env = id;
    }

    @Override
    public int hashCode() {
        return getIDEnv();
    }

    @Override
    public boolean equals(Object o) {
        if (o == null)
            return false;
        if (!(o instanceof Browsable))
            return false;
        Browsable other = (Browsable) o;
        return this.getIDEnv() == other.getIDEnv();
    }

    public Browsable getBrowsableParent() {
        return this.browsableParent;
    }

    public void setBrowsableParent(Browsable p) {
        if (freezeParents) return;
        this.browsableParent = p;
    }

    public static void freezeParents() {
        freezeParents = true;
    }

    public static void unfreezeParents() {
        freezeParents = false;
    }

    /**
     * Returns a Pos object representing the position of this Expr.
     */
    public Pos pos() {
        return Pos.UNKNOWN;
    }

    /**
     * Returns a Pos object representing the entire span of this Expr and all its
     * subexpressions.
     */
    public Pos span() {
        return pos();
    }

    /**
     * Returns the description (as HTML) to show for this node.
     */
    public abstract String getHTML();

    /** Returns a list of subnodes for this node. */
    public abstract List< ? extends Browsable> getSubnodes();

    /**
     * Construct a Browsable node with the given HTML description and the given
     * single subnode.
     */
    public static final Browsable make(final Pos pos, final Pos span, final String html, Browsable subnode) {
        return make(pos, span, html, Util.asList(subnode));
    }

    /**
     * Construct a Browsable node with the given HTML description and the given
     * single subnode.
     */
    public static final Browsable make(final String html, Browsable subnode) {
        return make(Pos.UNKNOWN, Pos.UNKNOWN, html, Util.asList(subnode));
    }

    /**
     * Construct a Browsable node with the given HTML description and the given 0 or
     * more subnodes.
     */
    public static final Browsable make(final String html, final List< ? extends Browsable> subnodes) {
        return make(Pos.UNKNOWN, Pos.UNKNOWN, html, subnodes);
    }

    /**
     * Construct a Browsable node with the given HTML description and the given 0 or
     * more subnodes.
     */
    public static final Browsable make(final Pos pos, final Pos span, final String html, final List< ? extends Browsable> subnodes) {
        final ConstList< ? extends Browsable> constlist = ConstList.make(subnodes);
        Browsable newBrowsable = new Browsable() {

            @Override
            public Pos pos() {
                return pos;
            }

            @Override
            public Pos span() {
                return span;
            }

            @Override
            public String getHTML() {
                return html;
            }

            @Override
            public List< ? extends Browsable> getSubnodes() {
                return constlist;
            }

            @Override
            protected void defineParentForComponents() {
                for (Browsable n : getSubnodes()) {
                    n.setBrowsableParent(this);
                }
            }

            @Override
            public Object clone() {
                List<Browsable> subnodes = new LinkedList<>();
                for (Browsable s : getSubnodes()) {
                    subnodes.add((Browsable) s.clone());
                }
                Browsable clone = Browsable.make(pos, span, getHTML(), ConstList.make(subnodes));
                clone.setID(getID());
                clone.setIDEnv(getIDEnv());
                mutGenLimit(mutGenLimit());
                return clone;
            }

        };
        return newBrowsable;
    }

    /**
     * Display this node and its subnodes as a tree; if listener!=null, it will
     * receive OurTree.Event.SELECT events when nodes are selected.
     */
    public final JFrame showAsTree(Listener listener) {
        final OurTree tree = new OurTree(12) {

            private static final long serialVersionUID = 0;
            private final boolean     onWindows        = Util.onWindows();
            {
                do_start();
            }

            @Override
            public String convertValueToText(Object val, boolean selected, boolean expanded, boolean leaf, int row, boolean focus) {
                String c = ">";
                String x = (val instanceof Browsable) ? ((Browsable) val).getHTML() : Util.encode(String.valueOf(val));
                if (onWindows)
                    c = selected ? " style=\"color:#ffffff;\">" : " style=\"color:#000000;\">";
                return "<html><span" + c + x + "</span></html>";
            }

            @Override
            public List< ? > do_ask(Object parent) {
                if (parent instanceof Browsable)
                    return ((Browsable) parent).getSubnodes();
                else
                    return new ArrayList<Browsable>();
            }

            @Override
            public Object do_root() {
                return Browsable.this;
            }
        };
        tree.setBorder(new EmptyBorder(3, 3, 3, 3));
        final JScrollPane scr = new JScrollPane(tree, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scr.addFocusListener(new FocusListener() {

            @Override
            public void focusGained(FocusEvent e) {
                tree.requestFocusInWindow();
            }

            @Override
            public void focusLost(FocusEvent e) {
            }
        });
        final JFrame x = new JFrame("Parse Tree");
        x.setLayout(new BorderLayout());
        x.add(scr, BorderLayout.CENTER);
        x.pack();
        x.setSize(500, 500);
        x.setLocationRelativeTo(null);
        x.setVisible(true);
        if (listener != null)
            tree.listeners.add(listener);
        return x;
    }

    protected abstract void defineParentForComponents();

    @Override
    public abstract Object clone();
}

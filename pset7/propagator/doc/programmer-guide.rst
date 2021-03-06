======================================================================
		     Programming with Propagators
======================================================================

Scheme-Propagators, pending a better name, is a prototype
propagator-oriented programming language embedded in Scheme.
Scheme-Propagators is also the name of the only extant implementation.
The purpose of this document is to teach you how to write and run
propagator programs in Scheme-Propagators for fun and profit.

.. contents::

Audience
======================================================================

I assume you are very excited to try your hand at programming with
propagators, either on this system or on one you intend to build for
yourself using what I learned building this one as a guide.  Otherwise
why would you be reading the programming guide of a prototype
propagator programming language?  I will therefore expend no effort to
convince you that propagators are awesome.

I assume you read the `Art of the Propagator`_ paper and/or the
`Propagation Networks`_ PhD dissertation.  Otherwise, I fear you will
have no idea what I'm talking about in this document.  Be advised,
though, that the actual mechanics of the Scheme-Propagators system
have evolved somewhat since those were published.

.. _`Art of the Propagator`: http://dspace.mit.edu/handle/1721.1/44215
.. _`Propagation Networks`: http://dspace.mit.edu/handle/1721.1/49525

I assume you are familiar with the Scheme programming language.  If
you're not, you should go learn it --- it's a wonderful programming
language --- but I cannot teach it to you here; and Scheme-Propagators
is inseparably intertwined with Scheme, so you will need to know
Scheme both to get anything out of reading this document and to
program Scheme-Propagators.

I assume you are not afraid to read the source code.  This document is
meant to give you the overview and introduction, and get you started,
but Scheme-Propagators is a prototype.  You will need to look under
the covers eventually, both to understand in detail how things work,
and to do the sophisticated things I expect you may come to want to
do.  Scheme-Propagators is also a work in progress.  Documentation
goes stale, but the code is always right (especially when the test
suite passes).

Finally, I assume you know who I am.  Scheme-Propagators is enough a
prototype that I can make no guarantees about your mileage with it
unless you are in range to ask me questions.


Getting Started
======================================================================

Scheme-Propagators is implemented in `MIT/GNU Scheme`_, which you will
need in order to use it.  You will also need Scheme-Propagators
itself, which you can check out from the `MMP git archive`_.  Once you
have it, go to the ``propagator/`` directory, start up your Scheme and
load the main entry file with ``(load "load")``.  This gives you a
read-eval-print loop (traditionally called a REPL for short) for both
the Scheme-Propagators system and the underlying Scheme
implementation.  Check out the README for more on this.
TODO Real releases?  From a real web place?

.. _`MIT/GNU Scheme`: http://www.gnu.org/software/mit-scheme/
.. _`MMP git archive`: git@github.com:MIT-MMP/propagator.git

Once you've got your REPL, you can start typing away at it to create
propagator networks, give them inputs, ask them to do computations,
and look at the results.

Here's a little propagator example that adds two and three to get
five::

  (define-cell a)
  (define-cell b)
  (add-content a 3)
  (add-content b 2)
  (define-cell answer (e:+ a b))
  (run)
  (content answer) ==> 5

Each of the parenthesized phrases above are things to type into
the REPL, and the ``==> 5`` at the end is the result that Scheme
will print.  I omitted the results of all the other expressions
because they are not interesting.

Let's have a closer look at what's going on in this example,
to serve as a guide for more in-depth discussion later.
``define-cell`` is a Scheme macro for making and naming propagator
cells::

  (define-cell a)

creates a new cell and binds it to the Scheme variable ``a``.

::

  (define-cell b)

makes another one.  Then ``add-content`` is the Scheme procedure that
directly zaps some information into a propagator cell (all the
propagators use it to talk to the cells, and you can too).  So::

  (add-content a 3)

puts a ``3`` into the cell named ``a``, and::

  (add-content b 2)

puts a ``2`` into the cell named ``b``.  Now ``e:+`` (I'll explain
that naming convention later) is a Scheme procedure that creates
a propagator that adds, attaches it to the given cells as inputs,
and makes a cell to hold the adder's output and returns it.  So::

  (define-cell answer (e:+ a b))

creates an adding propagator, and also creates a cell, now called
``answer``, to hold the result of the addition.  Be careful!  No
computation has happened yet.  You've just made up a network, but it
hasn't done its work yet.  That's what the Scheme procedure ``run`` is
for::

  (run)

actually executes the network, and only when the network is done
computing does it give you back the REPL to interact with.  Finally
``content`` is a Scheme procedure that gets the content of cells::

  (content answer)

looks at what the cell named ``answer`` has now, which is ``5``
because the addition propagator created by ``e:+`` has had a chance to
do its job.  If you had forgotten to type ``(run)`` before typing
``(content answer)``, it would have printed out ``#(*the-nothing*)``,
which means that cell has no information about the value it is meant
to have.


Organizing Principle
======================================================================

The two major aspects of a propagator-oriented programming language,
at least as I currently envision such a thing, are propagator networks
and the partial information structures they manipulate.  It feels like
"programming" is primarily about creating propagator networks;
inventing and implementing good partial information types seems to be
more of a library writer's kind of job, though one you will most
likely also need to engage in if you want to get anything done with
Scheme-Propagators as it currently stands.  We will therefore talk
about making networks first, and about making partial information
types later.  Also, making propagator networks is more different from
the normal programming you are used to than is creating partial
information types, and so the way to do that is more of a new
"language".

The "read and syntax" phase of programming a network in
Scheme-Propagators is the "read and eval" phase of the host Scheme;
with the understanding that all Scheme variables that get bound to
cells are propagator variables, and all Scheme variables that get
bound to other Scheme objects are "syntax" from the perspective of
Scheme-Propagators.  Things that can live in cells are the first-class
entities of Scheme-Propagators, and other things from the host Scheme
are second-class as far as the Scheme-Propagators langauge is
concerned.

Scheme-Propagators therefore has a "macro system" that is much more
developed than the propagator language itself, because MIT/GNU Scheme
is a full programming language that has been around for decades, while
Scheme-Propagators is a prototype whose name hasn't even stabilized
yet.  Thus these "macros" are still needed for many purposes.  Perhaps
the most egregious example is the predefined procedures: in Scheme,
``+`` is a variable that's bound to a procedure, whereas in
Scheme-Propagators, the corresponding object ``e:+`` is a piece of
syntax (that is, the Scheme variable ``e:+`` is not bound to a cell
that holds a propagator abstraction that adds, and therefore is not a
variable of Scheme-Propagators, but is rather bound to a Scheme
procedure that directly makes a propagator that adds, and therefore is
Scheme-Propagators syntax.  More on this later).

Scheme-Propagators has no linguistic support for making new partial
information types.  Those are all programmed in the underlying Scheme,
with a procedural interface.  We will talk about how to do that later.


Making Propagator Networks
======================================================================

The ingredients of a propagator network are cells and propagators.
The cells' job is to remember things; the propagators' job is to
compute.  The analogy is that propagators are like the procedures of a
normal programming language, and cells are like the memory locations;
the big difference, of course, is that cells accumulate partial
information (which may involve arbitrary internal computations), and
can therefore have many propagators reading information from them and
writing information to them.

The two basic operations when making a propagator network are making
cells and attaching propagators to cells.  You already met one way to
make cells in the form of ``define-cell``; we will talk about more
later, but let's talk about propagators first.


Attaching Basic Propagators: p:foo and e:foo
----------------------------------------------------------------------

You attach propagators to cells by calling an appropriate
Scheme procedure that does that.  For example, the procedure ``p:+`` attaches
an adding propagator::

  (p:+ foo bar baz)

means attach a propagator that will add the contents of the cells
``foo`` and ``bar`` and write them into ``baz``.  This means that
henceforth, whenever either the ``foo`` cell or the ``bar`` cell gets
any new interesting information, the appropriate sum will eventually
get computed and written into ``baz``.

Note that this ``p:+`` is different from the ``e:+`` in the example at
the beginning.  This is a general naming convention.  ``p:`` stands
for "propagator".  A thing named ``p:foo`` is a Scheme procedure
(therefore Scheme-Propagators syntax) that attaches a propagator that
does the ``foo`` job to a full collection of cells, one for each input
to ``foo`` and one for the output from ``foo``.  The output cells
conventionally go last (though I am open to changing that).  In
principle the ``p:`` convention will work just as well for jobs that
have multiple outputs, but I don't actually have any of those in the
system at present.

In contrast, ``e:`` stands for "expression".  A thing named ``e:foo``
is a Scheme procedure (so Scheme-Propagators syntax) just like
``p:foo``, except that it makes a fresh cell for the output and
returns it (whereas ``p:foo`` does not return anything useful).  Here
are two different ways to write the same thing::

  (define-cell x)
  (define-cell y)
  (define-cell z)
  (p:* x y z)

and::

  (define-cell x)
  (define-cell y)
  (define-cell z (e:* x y))

Generally the ``e:`` procedures are much more convenient to use most
of the time, when some propagator is the only one that writes to its
output; and you can chain them in the familiar way

::

  (e:- w (e:* (e:+ x y) z))

but when you need to make a propagator that writes to a cell you
already have, such as when multiple propagators need to write to the
same cell, you need the ``p:`` versions.  For example, if you wanted
to be able to go back from ``z`` and one of ``x`` or ``y`` to the
other, rather than just from ``x`` and ``y`` to ``z``, you could write::

  (define-cell x)
  (define-cell y)
  (define-cell z (e:* x y))
  (p:/ z x y)
  (p:/ z y x)

and get a multidirectional constraint::

  (add-content z 6)
  (add-content x 3)
  (run)
  (content y) ==> 2


TODO Provide a list of available propagator constructors. (Don't forget binary-amb and company)

Attaching Propagator Constraints: c:foo and ce:foo
----------------------------------------------------------------------

Speaking of constraints, they are so useful that many are predefined,
and they have their own naming convention.  ``c:`` stands for
"constraining".  A thing named ``c:foo`` is the constraining analogue
of ``p:foo``, in that in addition to attaching a propagator that does
``foo`` to its cells, it also attaches ``foo-inverse`` propagators
that deduce "inputs" from "outputs".  For example, the product
constraint that we built in the previous section is available as
``c:*``::

  (define-cell x)
  (define-cell y)
  (define-cell z)
  (c:* x y z)

  (add-content z 12)
  (add-content y 4)
  (run)
  (content x) ==> 3
  
The ``c:`` procedures also have expression versions:::

  (define-cell x)
  (define-cell y)
  (define-cell z (ce:* x y))

``ce:foo`` is to ``c:foo`` as ``e:foo`` is to ``p:foo``.

Of course, not every operation has a useful inverse, so there are
fewer ``c:`` procedures defined than ``p:``.  For the complete list see TODO.

Constants and Literal Values
----------------------------------------------------------------------

Programs have embedded constants all the time, and propagator programs
are no different (except that constant values, like all other values,
can be partial).  We've already seen one way to put a
Scheme value into a propagator program: the ``add-content`` procedure
zaps a value straight into a cell.  This is generally encouraged at
the REPL, but frowned upon in actual programs.  It is much nicer (in
my current opinion) to use ``constant`` or ``p:constant`` (they're the
same) to make a propagator that will zap your value into your cell for
you::

  (define-cell thing)
  ((constant 5) thing)
  (content thing) ==> #(*the-nothing*)
  (run)
  (content thing) ==> 5

There is also an expression-oriented version, called, naturally,
``e:constant``::

  (define-cell thing (e:constant 5))
  (run)
  (content thing) ==> 5

In fact, inserting constants is so important, that there is one more
nicification of this: whenever possible, the system will convert a raw
constant (i.e. a non-cell Scheme object) into a cell, using
``e:constant``.
Some examples::

  (e:+ x 2)          ==>   (e:+ x (e:constant 2))
  (define-cell x 4)  ==>   (define-cell x (e:constant 4))
  (c:+ x y 0)        ==>   (c:+ x y (e:constant 0))

  (define-macro-propagator (p:double x y)
    (p:+ x x y))
  (p:double 4 z)     ==>   (p:double (e:constant 4) z)

Making Cells
----------------------------------------------------------------------

In order to have something to attach propagators to, you need to have
cells.  Cells are the memory locations of the Scheme-Propagators
language; Scheme variables whose bindings are cells correspond to
Scheme-Propagators variables (Scheme variables whose bindings are
other things look like syntax to Scheme-Propagators).  You've
already met one way to make cells::

  (define-cell x)

creates a Scheme variable named ``x`` and binds a cell to it.  The
underlying mechanism underneath this is the procedure ``make-cell``,
which creates a cell and lets you do whatever you want with it.  So
you could write::

  (define x (make-cell))

which would also make a Scheme variable named ``x`` and bind a cell to
it.  In fact, that is almost exactly what ``define-cell`` does, except
that ``define-cell`` attaches
some metadata to the cell it creates to make it easier to debug the
network (see below) and also does constant conversion (so ``(define-cell x
5)`` makes ``x`` a cell that will get a ``5`` put into it, whereas
``(define x 5)`` would just bind ``x`` to ``5``).

Just as Scheme has several mechanisms of making variables, so
Scheme-Propagators has corresponding ones.  Corresponding to Scheme's
``let``, Scheme-Propagators has ``let-cells``::

  (let-cells ((foo (e:+ x y))
              (bar (e:* x y)))
    ...)

will create the Scheme bindings ``foo`` and ``bar``, and bind them to
the cells made by ``(e:+ x y)`` and ``(e:* x y)``, respectively (this
code is only sensible if ``x`` and ``y`` are already bound to cells
(or subject to constant conversion)).  The new bindings will only be
visible inside the scope of the ``let-cells``, just like in Scheme;
but if you attach propagators to them, the cells themselves will
continue to exist and function as part of your propagator network.

One notable difference from Scheme: a cell in a propagator network,
unlike a variable in Scheme, has a perfectly good "initial state".
Every cell starts life knowing ``nothing`` about its intended
contents; where Scheme variables have to start life in a weird
"unassigned" state, ``nothing`` is a perfectly good partial
information structure.  This means that it's perfectly reasonable
for ``let-cells`` to make cells with no initialization forms::

  (let-cells (x y (foo (some thing))) ...)

creates cells named ``x`` and ``y``, which are empty and have
no propagators attached to them initially, and also a cell
named ``foo`` like above.  ``let-cells`` also recognizes the
usage::

  (let-cells ((x) (y) (foo (some thing))) ...)

by analogy with Scheme ``let``.

Corresponding to Scheme's ``let*``, Scheme-Propagators has ``let-cells*``.
``let-cells*`` is to ``let-cells`` what ``let*`` is to ``let``::

  (let-cells* ((x)
               (y (e:+ x x)))
    ...)

will make a cell named ``x`` and a cell named ``y`` with an adder both
of whose inputs are ``x`` and whose output is ``y``.

Now, ``let-cells`` and ``let-cells*`` are, like ``define-cell``,
basically a convenience
over doing the same thing in Scheme with ``let``, ``let*`` and ``make-cell``.
Also like ``define-cell``, ``let-cells`` and ``let-cells*`` do
constant conversion (so
in ``(let-cells ((x 3)) ...)``, ``x`` becomes a cell, not a Scheme object),
and attach metadata to the cells they bind.

Since ``let-cells`` is plural (where ``let`` was number-neutral), I
also defined ``let-cell`` for the case when you just want to make one
cell::

  (let-cell x ...)              ==>  (let-cells (x) ...)
  (let-cell (x (e:+ y z)) ...)  ==>  (let-cells ((x (e:+ y z))) ...)

Scheme-Propagators has no analogues of Scheme's ``letrec`` or named
``let`` syntax.  Any suggestions on what they would be like and for in
this world?

Finally, there is one more way to make cells that you've also already
met, but maybe didn't recognize.  All the ``e:`` and ``ce:``
procedures make and return cells to hold the "outputs" of their
underlying ``p:`` and ``c:`` variants.  These implicit cells are just
like the implicit memory locations that Scheme creates under the hood
for holding the return values of expressions before they get used by
the next expression or assigned to variables.

Making New Compound Propagators
======================================================================

So, you know the primitives (the supplied propagators) and the means
of combination (how to make cells and wire bunches of propagators up
into networks).  Now for the means of abstraction.  A procedure like
``p:+`` is like a wiring diagram with a few holes where it can be
attached to other structures.  Supply that procedure with cells,
and it makes an actual propagator for addition whose inputs and outputs
are those cells.  How do you make compound such procedures?

Well, you can always just use the underlying Scheme::

  (define (my-diagram x y z)
    (p:+ x y z)
    (p:- z y x)
    (p:- z x y))

Then ``my-diagram`` would be almost like ``p:+``, in that it would
also be a Scheme variable bound to a Scheme procedure that, if given
three cells, would construct some propagators attached to those cells.
``p:+`` does a little more than that basic job, however, so you should
use ``define-macro-propagator`` instead of ``define``::

  (define-macro-propagator (my-diagram x y z)
    (p:+ x y z)
    (p:- z y x)
    (p:- z x y))

makes a much nicer ``my-diagram`` that, in addition to doing the basic
job you would expect, also keeps track of metadata that is very helpful
for debugging (namely that the adder and two subtractors inside were
created by a ``my-diagram`` rather than just hanging out), and performs
constant conversion on its inputs, so you can write::

  (my-diagram x 3 z)  

and get

::

  (my-diagram x (e:constant 3) z)

The Scheme macro ``define-macro-propagator`` is called that because
the object it creates is not first-class in Scheme-Propagators.  At
the moment, Scheme-Propagators has no (stable) first-class
representation of wiring diagrams; so all abstraction is effectively
at the level of "macros", and ``define-macro-propagator`` is part of
that system.  But the only "macroness" about it, really, is that the
resulting ``my-diagram`` does not and cannot live in a cell.

Recursion
----------------------------------------------------------------------

Propagator abstractions defined by ``define-macro-propagator`` have
one flaw: they are expanded immediately when Scheme encounters them.
Therefore, they cannot be used to build recursive structures, because
the structure would be expanded infinitely far.  For this purpose,
there is ``define-compound-propagator``.  It's just like
``define-macro-propagator``, except that the expansion of the wiring
diagram represented by the resulting Scheme procedure is delayed until
some (however partial) information shows up on at least one of the
cells that the diagram is attached to.  For example, a diagram for
computing factorials::

  (define-compound-propagator (p:factorial n n!)
    (let-cells* ((done? (e:= 0 n))
		 (n-again (e:switch (e:not done?) n))
		 (n!-again (e:* n-again (e:factorial (e:- n-again 1)))))
      (conditional done? 1 n!-again n!)))

  (define e:factorial (functionalize p:factorial))

contains a call to itself; but attaching this to some cells will not
cause an immediate infinite regress because the internal ``factorial``
will only expand dynamically during the execution of the network, and
only if it has information to process (preventing spurious recursions
is what the ``switch`` and ``n-again`` is for).

Much the same effect can be achieved procedurally using the Scheme
procedure ``delayed-propagator-constructor``.

In principle, there is no propagator abstraction that you can express
with ``define-macro-propagator`` that you cannot express better with
``define-compound-propagator``.  However, I still advise
``define-macro-propagator`` where possible, because
``define-compound-propagator`` is more complex, and less stable.
Specifically, while it's pretty clear that ``define-macro-propagator``
is pretty much the right way to make a "propagator macro", it is not
at all clear whether ``define-compound-propagator`` is the right
implementation of the idea of "propagator closure".

Expressions
----------------------------------------------------------------------

The example diagram called ``my-diagram`` above should probably have
been named ``p:my-diagram``, because its expects to get all of its
boundary cells when called, and the Scheme procedure does not return
anything useful.  You can mechanically convert ``p:``-type procedures that
you define into ``e:``-type versions with the Scheme procedure
``functionalize``::

  (define e:my-diagram (functionalize p:my-diagram))
  (define-cell z (e:my-diagram x y))

will do what you expect.

Macrology
----------------------------------------------------------------------

Sometimes you will need to make something that looks more like a macro
to Scheme-Propagators than the things ``define-macro-propagator`` is
for.  After all, the procedures produced by
``define-macro-propagator`` will not only assume that their arguments
are all cells, but will actively coerce them into cells.  For extreme
cases there's always Scheme's ``define``; but sometimes you want the
debugging data provided by ``define-macro-propagator`` but not the
constant conversion.  A common use case is variable-arity network
diagrams.  You need a list of cells rather than a single cell, and you
want to use Scheme's ``map`` or ``for-each`` to do something to them,
but you still want the debugging aids that ``define-macro-propagator``
provides and ``define`` does not.  This is what
``define-propagator-syntax`` is for.  The classic example is
``require-distinct``::

  (define-propagator-syntax (require-distinct cells)
    (for-each-distinct-pair
     (lambda (c1 c2)
       (define-cell p)
       (=? c1 c2 p)
       (forbid p))
     cells))


Using Partial Information
======================================================================

Partial, accumulatable information is the other side of the coin of
multidirectional, nonsequential programming, so Scheme-Propagators is
all about partial information.  What do I mean by that?  Each "memory
location" of Scheme-Propagators, that is each cell, maintains not "a
value", but "all the information it has about a value".  Such
information may be as little as "I know absolutely nothing about my
value", as much as "I know everything there is to know about my value,
and it is ``x``", and many possible variations in between; and also
one not-in-between variation, which is "Stop the presses!  I know
there is a contradiction!"

All these various possible states of information are represented (per
force) as Scheme objects.  The Scheme object ``nothing`` represents
the information "I don't know anything".  This only takes a single
Scheme object, because not knowing anything is a single state of
knowledge.  Most Scheme objects represent "perfect, consistent"
information: the Scheme object ``5`` represents the information "I
know everything there is to know, and the answer is ``5``."  There are
also several Scheme types provided with the system that denote
specific other states of knowledge, and you can make your own.  For
example, objects of type ``interval?`` contain an upper bound and a
lower bound, and represent information of the form "I know by value is
between this real number and that one."

The way to get partial knowledge into the network is to put it into
cells with ``add-content`` or constant propagators.  For example::

  (define-cell x (make-interval 3 5))

produces a cell named ``x`` that now holds the partial information
``(make-interval 3 5)``, which means that its notional value is
between ``3`` and ``5``.

Partial information structures are generally built to be contagious,
so that once you've inserted a structure of a certain type into
the network, the normal propagators will generally produce answers
in kind, and, if needed, coerce their inputs into the right form
to co-operate.  For example, if ``x`` has an interval like above,

::

  (define-cell y (e:+ x 2))

will make an adder that will eventually need to add ``2`` to the
interval between ``3`` and ``5``.  This is a perfectly reasonable
thing to ask, because both ``2`` and ``(make-interval 3 5)`` are
states of knowledge about the inputs to that adder, so it ought to
produce the best possible representation of the knowledge it can
deduce about the result of the addition.  In this case, that would be
the interval between ``5`` and ``7``::

  (run)
  (content y)  ==>  #(interval 5 7)

The key thing about partial information is, of course, that it's
cumulative.  So if you also added some other knowledge to the ``y``
cell, it would need to merge with the interval that's there to
represent the complete knowledge available as a result::

  (add-content y (make-interval 4 6))
  (content y)  ==>  #(interval 5 6)

If incoming knowledge hopelessly contradicts the knowledge a cell
already has, it will complain::

  (add-content y 15)  ==>  Error

stop the network mid-stride, and give you a chance to examine the
situation so you can debug the program that led to it, using the
standard MIT Scheme debugging facilities.

TODO Documentation of provided partial information types

- nothing
- just a value
- intervals
- supported values
- truth maintenance systems
- cons cells (in flux)


Making New Kinds of Partial Information
======================================================================

There are three components to making your own types of partial
information.  The zeroth is to define the appropriate data structure,
of course.

Define your Merge Handlers
----------------------------------------------------------------------

The first is to teach cells how to merge your partial information
structure.  This you do by adding methods to the generic procedure
``merge``.  Method addition is done with the ``defhandler``
procedure::

  (defhandler operation handler arg-predicate ...)

The generic operations system is a predicate dispatch system.  Every
handler is keyed by a bunch of predicates that must accept the
arguments to the generic procedure in turn; if they do, that handler
is invoked.  For example, merging two intervals with each other
can be defined as::

  (defhandler merge intersect-intervals interval? interval?)

Two important things not to forget: First, if the incoming information
(second argument to the ``merge`` generic procedure) is redundant, you
must return identically the first argument, because cells check with
``eq?`` whether their information changed.  Presumably the
``intersect-intervals`` procedure above arranges this internally.
(The Scheme procedure ``eq?-standardizing`` is provided as a useful
combinator for this purpose -- type ``(pp eq?-standardizing)`` at a prompt
after loading the Scheme-Propagators system).  If you get this wrong,
your networks will tend to enter infinite loops.  Second, it is your
responsibility to make sure that your partial information structure
merges well with all other partial information structures that it can
encounter in a cell.  Intervals, for example, should handle raw
numbers, because knowing that something is exactly ``2`` is compatible
with knowing that it is between ``1`` and ``3``.  In the case of
intervals, I defined the procedure ``ensure-inside`` to either
return the number if it is in the interval, or return a contradiction
object if it is not, and attached it as a handler with

::

  (defhandler merge ensure-inside interval? number?)

  (defhandler merge
   (lambda (content increment)
     (ensure-inside increment content))
   number? interval?)

Speaking of which, ``merge`` is allowed to return a special object
called ``the-contradiction`` to indicate a complete contradiction
(that should result in an immediate error).

TODO Document the extant partial information structures and the
default mechanisms they use for interacting with others (namely the
``nothing? any?`` handlers, the ``flat?`` predicate, the general
bevaior of TMSes, maybe also the cons story).

Define your Contradiction Test
----------------------------------------------------------------------

There is a generic procedure called ``contradictory?`` to which you
can also attach handlers for your partial information structures.  The
``contradictory?`` procedure is called by cells on new merge results
every time they are created, and if it ever returns true, the cell
signals an error immediately.  For example, a strictly empty interval
implies an impossible state of knowledge::

  (defhandler contradictory? empty-interval? interval?)
  ;; N.B. empty-interval? is the handler; interval? is the only predicate

which means that every interval will be checked by the
``empty-interval?`` procedure to test whether it represents a
contradiction.

Augment the Propagators
----------------------------------------------------------------------

In addition to teaching cells how to support your partial information
type, you must also teach the appropriate propagators about it.  Every
primitive propagator that you expect to interact with your partial
information must know how to handle it.  The compound propagators are
ok because they just pass stuff along to the primitives they are
eventually composed of, but the primitives must be taught.

There are two mechanisms of doing this.  Most (TODO document which)
primitive propagators are actually generic Scheme functions
underneath, so you can add handlers to them just like you add handlers
to ``merge``.  See ``core/intervals.scm`` for an example of how this
is done with intervals.  Don't forget to teach the propagators what to
do if they encounter your partial information structure on one input
and a different one on another --- if both represent states of
knowledge about compatible ultimate values, it should be possible to
produce a state of knowledge about the results of the computation
(though in extreme cases that state of knowledge might be ``nothing``,
implying no new information produced by the propagator).

Also, most (TODO document which) primitive propagators are wrapped
with the ``nary-unpacking`` wrapper function around their underlying
generic operation.  This wrapper function is a poor man's
implementation of monads, so if your partial information structure is
essentially monadic, you can use this to teach all propagators how to
handle it.

Unfortunately, I understand neither partial information nor monads as
well as I would like, so this mechanism is a bit nasty.  To use it,
you must define methods for the generic procedures ``generic-unpack``
and ``generic-flatten``, which are a not-necessarily-good
decomposition of the usual monadic ``bind`` operation.  The ``bind``
is an ``unpack`` followed by a ``flatten``.  ``generic-unpack`` takes
your partial information structure and a function that wants the
goodie inside, is expected to call that function with whatever values
it wants, and to produce the result of the function, partial in the
way appropriate to your partial information.  Subsequently,
``generic-flatten`` is called on the result, to allow you to sanitize
it; for example, to turn a truth maintenance system that now
(directly) contains a truth maintenance system into just one single
truth maintenance system.

If this helps, the type signatures of ``generic-unpack`` and
``generic-flatten`` would be::

  generic-unpack: M a --> (a --> b) --> M b
  generic-flatten: M M a --> M a

except for two things: I tried to allow the underlying system to be a
bit sloppy with its types, and to rely on coercions to correct the
sloppiness; so the result is that a function being unpacked into is
free to return whatever it wants, and you are expected to take care of
it in ``generic-flatten``; and I tried to make the partial information
types compose, so the thing that's really going on is that there is
one big monad that you are adding to.  I don't know whether this is a
reimplementation of the monad transformers story, because no one has
ever explained that story to me in such a way that I got it.

In any case, this mechanism is something of a mess.  See
``core/supported-values.scm`` and ``core/truth-maintenance.scm`` for
examples of how it can be used; and maybe talk to me when you set out
to make a partial information structure.


Debugging
======================================================================

There is no stand-alone "propagator debugger"; if something goes
wrong, the underlying Scheme debugger is your friend.  Some effort
has, however, been expended on making your life easier.

In normal operation, Scheme-Propagators keeps track of some metadata
about the network that is running.  This metadata can be invaluable
for debugging propagator networks.  The specific data it tries to
track is:

- The names (non-unique but semantic) of all the cells and
  propagators.  This is in contast with the unique but non-semantic
  object hashes of all the cells and propagators that MIT Scheme
  tracks anyway.

- Which propagators are connected to which cells.

- Whether the connections are input, output, or both.

- The grouping structure of the propagator network, as defined
  by the call structure of the Scheme procedures that constructed it.

To make sure that your network tracks this metadata well, you should
use the high level interfaces to making cells, propagators, and
propagator constructors when possible (``define-cell``, ``let-cells``,
``define-macro-propagator``, ``propagatify``, etc).  Any gaps not
filled by use of these interfaces must either be accepted as gaps or
be filled by hand.

Perhaps the most spectacular use of the metadata facility is to
draw pictures of your propagator network.  Just type::

  (draw:show-graph)

at the REPL and watch what happens!  If the picture does not look like
the graph you thought you made, make sure the connection metadata is
collected appropriately, but then check your code to see whether you
miswired something.  If the picture contains useless gibberish in the
labels, make sure the names of things are correctly assigned and
tracked.  If ``dot`` crashes, maybe your network is too big for it.
For more on various pictures you can draw, look in the source comments
in ``extensions/draw.scm``.

Of course, in order to use the metadata for debugging, you must be
able to read it.  Inspection procedures using the metadata are provided:

name
  the name of an object, should it have one

cell?
  whether something is a cell or not

propagator?
  whether something is a propagator or not

propagator-inputs
  the inputs of a propagator (a list of cells)

propagator-outputs
  the outputs of a propagator (a list of cells)

neighbors
  the readers of a cell (a list of propagators)

cell-non-readers
  other propagators somehow associated with a cell (presumably ones that write to it)

cell-connections
  all propagators around a cell (the append of the neighbors
  and the non-readers)

network-group-of
  a metadata object representing the context in which
  the object being examined was created (see ``core/metadata.scm``
  to learn what you can do with them)

You can use these at least somewhat to wander around a network you are
debugging.  Be advised that both cells and propagators are represented
directly as Scheme procedures, and therefore do not print very nicely
at the REPL.

If you find yourself doing something strange that circumvents the
usual metadata tracking mechanisms, you can add the desired metadata
yourself.  All the metadata collection procedures are defined in
``core/metadata.scm``; they generally use the ``eq-properties``
mechanism in ``support/eq-properties.scm`` to track the metadata, so
you can use it to add more.  In particular, see the definition of, say,
``function->propagator-constructor`` or ``define-macro-propagator``
for examples of how this is done.


Making New Primitive Propagators
======================================================================

Direct Construction from Functions
----------------------------------------------------------------------

The fundamental, stable way to make your own primitive propagators is
the procedure ``function->propagator-constructor``.  It takes a Scheme
function, and makes a propagator construction procedure out of it that
makes a propagator that does the job implemented by that Scheme
function.  The propagator constructor in question takes one more
argument than the original function, the extra argument being the cell
into which to write the output.  So the result of
``function->propagator-constructor`` is a ``p:``-style procedure
(complete with (most of) the debugging information, and the constant
conversion).  For example, you might define::

  (define p:my-primitive (function->propagator-constructor do-it))

where ``do-it`` is the appropriate Scheme function.

Two things to pay attention to: ``function->propagator-constructor``
wraps the given function up into a propagator directly, and it is up
to the function itself to handle any interesting partial information
type that might come out of its argument cells.  Notably, ``nothing``
might show up in the arguments of that function when it is called.
Therefore, it may be appropriate the make the function itself generic,
and/or wrap it in ``nary-unpacking``.  For examples, check out how the
provided primitive propagators are implemented, in
``core/standard-propagators.scm`` (which refers to definitions made in
``core/generic-definitions.scm``).

The second thing is metadata.  ``function->propagator-constructor``
can supply all the metadata that the debugger uses except the name for
your function.  That you need to add yourself, with ``(name!
your-function 'some-name)`` (see ``core/generic-definitions.scm``).


Propagator Constructor Combinators
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Once you've made a ``p:``-style propagator constructor, you can turn
it into an ``:e``-style one automatically with ``functionalize``.  For
example, ``e:+`` is actually defined as::

  (define e:+ (functionalize p:+))

See ``core/expression-language.scm`` for more of that.

You can also delay the actual construction of your primitives
if you want with ``delayed-propagator-constructor``, though that's
really more useful with recursive compound propagators.


DWIM Propagator Construction
----------------------------------------------------------------------

All that wrapping in ``nary-unpacking``, and naming your propagator
functions with ``name!``, and calling ``functionalize`` to convert
them to ``e:``-style versions can get tedious.  This whole shebang
is automated by the ``propagatify`` macro::

  (propagatify eq?)

turns into

::

  (define p:eq?
   (function->propagator-constructor
    (nary-unpacking (name! eq? 'eq?))))
  (define e:eq? (functionalize p:eq?))

Use this with some caution; you may not always want ``nary-unpacking``,
and you may not always want to ``propagatify`` the raw Scheme function
instead of making a corresponding generic operator.
The macro is defined in ``core/expression-language.scm``, so that's
an example for you if you want to write variants (let me know if you
come across a good one).


Fully-manual Low-level Propagator Construction
----------------------------------------------------------------------

Finally, when the thing you want your propagator to do is so low-level and
interesting that it doesn't even correspond to a Scheme function,
there's always the ``propagator`` procedure.  This is the lowest level
interface to asking cells to notify a propagator when they change.
``propagator`` expects a list of cells that your propagator is
interested in, and a thunk that implements the job that propagator is
supposed to do.  The scheduler will execute your thunk from time to
time --- the only promise is that it will run at least once after the
last time any cell in the supplied neighbor list gains any new
information.  For example::

  (define (my-hairy-thing cell1 cell2)
    (propagator (list cell1 cell2)
      (lambda ()
        do-something-presumably-with-cell1-and-cell2)))

The ``propagator`` procedure being the lowest possible level, it has
no access to any useful sources of metadata, so you will need to
provide yourself any metadata you want to be able to access later.
For an example of how this facility is used, see the implementations
of ``function->propagator-constructor`` and
``delayed-propagator-constructor`` in ``core/core.scm``.


Miscellany
======================================================================

Implicit Cell Syntax
----------------------------------------------------------------------

A quirky little feature, called
``%%``.  This is a Scheme object, therefore Scheme-Propagators syntax,
for controlling the argument position of the implicit cell that an
``e:`` or ``ce:`` procedure will make and return.  Perhaps examples
are best::

  (e: foo bar)     <==>  (e: foo bar %%)

  (e: foo %% bar)  <==>  (let-cell new (p: foo new bar) new)

I borrowed this idea from Guy Steele's PhD thesis on constraint
languages, and it was a year between when I implemented it and
when I first used it.  The use case I do have is when I
want to make a new cell participate in an input position
in a constraint with some existing cells::

  (define-cell x)
  (define-cell z)
  (define-cell y (ce:+ x %% z))
  (add-content x 5)
  (add-content y 3)
  (run)
  (content z) ==> 8

Perhaps this use case could also be served by adding more
expression-style constraint procedures (namely ``ce:-``, which I do
not currently have), but then again maybe it's elegant.

Reboots
----------------------------------------------------------------------

The procedure ``initialize-scheduler`` wipes out an existing
propagator network and lets you start afresh::

  build lots of network
  ...
  (initialize-scheduler)
  (run) --- nothing happens; no propagators to run!

Compiling
----------------------------------------------------------------------

It turns out that ``make-cell`` and ``cell?`` are also MIT Scheme
primitives, so if you want to compile your Scheme-Propagators
code, be sure to put

::

  (declare (usual-integrations make-cell cell?))

at the top of your source files.  Also, of course, you need to be
suitably careful to make sure that the defined macros are available to
the syntaxer when it processes your file.  See
``support/auto-compilation.scm`` for how I do this, and, say,
``core/load.scm`` for how I use the compiler.

Scmutils
----------------------------------------------------------------------

The Scmutils_ system built by Gerald Jay Sussman for thinking about
physics can be very useful for many purposes.  Among other things,
it knows about units and dimensions, about symbolic algebra,
about solving systems of equations, etc.  Scheme-Propagators runs
in Scmutils just as well as in MIT Scheme; and some of the unit
tests in the self-test suite rely on Scmutils.

.. _Scmutils: http://groups.csail.mit.edu/mac/users/gjs/6946/linux-install.htm

Editing
----------------------------------------------------------------------

I edit code in Emacs.  Emacs of course has a Scheme mode; nothing more
need be said about that here.

If you are going to edit any parenthesized source code in Emacs,
`Paredit mode`_ is a godsend.

.. _`Paredit mode`: http://www.emacswiki.org/emacs/ParEdit

In addition to the above, I find it very useful to have my editor
highlight and indent some of the Scheme-Propagators macros I have
defined the same way as their Scheme analogues; notably
``define-macro-propagator`` and co, and ``let-cells``.  Sadly the
Emacs Scheme mode does not do this by default, so you need to tweak
the Emacs config to do that.  The file ``support/scm-propagators.el``
contains a dump of the relevant portion of my Emacs configuration.

Hacking
----------------------------------------------------------------------

Scheme-Propagators is obviously a work in progress.  Be aware that I
will continue to hack it to my heart's content.  Likewise, feel free
to hack it to yours --- let me know if you invent or implement
something interesting.

TODO Describe where in the source various constructs are defined?  So that
it is possible to mimic them (e.g. more primitive propagators) and/or
adapt them.

TODO
----------------------------------------------------------------------

- How do I want to section up the p:, e:, c:, and ce: ideas?
- Advertise the examples/ directory

Revision History of this Guide
----------------------------------------------------------------------

First written May 5, 2010 by Alexey Radul

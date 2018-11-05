# sts-haskell

**Current status:** Complete.

## Steiner Triple Systems

A Steiner Triple System over `n` points, called an `STS(n) = (V,E)`, is a *combinatorial design* with the following properties:

1. `V = {0, ..., n-1}` is the base set of thhe design, called the *points* (or less frequently, *vertices*) of the design.

2. `E` is a collection of triples (i.e. subsets if size 3) of `V`, called the *blocks*, *lines*, *edges*, or *triples* of the design. When it is unambiguous to do so, we write subsets in simplified notation, e.g. `{0, 2, 5} = 025`.

3. *Pair coverage property:* For every pair of elements `a, b` in `V`, there is a triple `T` in `E` that contains `a` and `b`.

## The Fano Plane: `STS(7)`

The `STS(7)` is the most famous example, as it is not only a Steiner triple system, but also a *projective finite geometry* known as the *Fano plane* (and indeed, the smallest possible suchh projective geometry). You can see a depiction of it below, where the differently coloured lines (and the central circle) indicate the triples, which are:

* `012`

* `034`

* `056`

* `135`

* `146`

* `236`

* `245`

![fano](docs/fano.png)

A further interesting fact about the Fano plane is that it is *self-dual,* i.e. if we "swap" the edges and the points of the design, we get a design with the same structure (i.e. one that is *isomorphic* to the original). In the following diagram, the blocks of the original design become the points of the dual design, and the points of the original design become the blocks of the dual design: the point 0 in the original design maps to the block `{012, 034, 056}` in the dual design, i.e. it consists of all the blocks of the original design (now points in the dual design) that contain 0.

![fano](docs/fano_dual.png)

One last interesting fact: any map `f:V -> V` over a Steiner triple system is called an *automorphism* if:

1. `f(V) = V`; and

2. `f(E) = {f(e) | e in E} = E`.

Automorphisms are *homomorphisms* (structure preserving maps) that are *isomorphisms* and *endomorphisms*, and that form a *group*. We will not explain all of these as it is beyond the scope of the `README.md` file. What is important to know is that, in the case of the Fano plane, we can use the automorphism group to show that there is one unique `STS(7)` up to isomorphism. (This is not always the case: there are 80 unique `STS(15)`.)

## Existence of `STS(n)`

A simple counting argument shows that an `STS(n)` exists if and only if `n = 1, 3 (mod 6)`. First we show that the condition is necessary, i.e. an `STS(n)` exists only if `n = 1, 3 (mod 6)`.

* The number of pairs that need to be covered are `n * (n-1) / 2`.

* Each triple covers exactly three pairs.

Thus, we must have that there are `T = (n * (n-1) / 2) / 3 = n * (n-1) / 6` triples.

Furthermore:

* Each point `p` must appear in triples with exactly `n-1` elements for all pairs to be covered.

* Each triple containing `p` covers exactly two pairs.

Thus, `n-1` must be even, implying that `n` is odd.

Thus, we can only have the cases that `n = 1, 3, 5 (mod 6)`, but if `n = 6k+5` for some natural number `k`, then the number of blocks is:

`n * (n-1)/6 = (6k+5)(6k+4)/6 = (36k^2 + 54k + 20)/6`

which is not an integer, eliminating that case.

The other two cases can be proven to be sufficient by constructions using *idempotent commutative quasigroups*, with the *Bose construction* giving us a `STS(6k+3)` for any `k ≥ 0`, and the *Skolem construction* giving us an `STS(6n+5)` for any `k ≥ 0`.

## Hill-Climbing Algorithm to produce `STS(n)`

There is an easy hill-climbing algorithm to generate `STS(n)`. The idea iteratively picks triples as follows:

1. If the `STS(n)` is not complete, there is some point `a ∈ V` such that `a` does not appear with every pair in some triple.

2. Thus, there are (at least) two elements, `b, c ∈ V` such that both `ab` and `ac` are in no triple.

3. If `bc` is in no triple, add triple `abc` and iterate.

4. If `bc` is in some triple, drop this triple and add triple `abc`.

It can be seen that the number of triples remains constant or increases, and in practice, this algorithm is able to quickly produce `STS(n)` even for large values of `n`.

## Project Goal

This project is an implementation of the hill-climbing algorithm to find `STS(n)` for `n = 1, 3 (mod 6)` using Haskell.

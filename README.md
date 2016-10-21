# Gaia

In Haskell, we use a lot of abstractions from mathematics in our
typeclasses. This is evident in our standard libraries to the
countless other great pieces of code which is written using Haskell.

However, typeclasses in Haskell have a limitation that mathematics does
not have, which is that for each type class, you can only have one
implementation per set of arguments. This is absolutely wrenching from the
point of view of mathematics, as some of the most fundamental structures
consist of a composition of the others, and the standard ways to encode
these structure usually involve implementing a new set of operators and
forcing the user to resupply these implementations which would be better
described as a lower level set of constraints on these objects.

The way I got around this problem is by taking too seriously a piece of
notation in mathematics referred to as the "carrier" of some structure.
Essentially what the carrier of, for instance, a group is the set which
the operation acts upon. For a group, we have no problems implementing
it in terms of our normal structure, but what if we want a ring?

What I wanted was to be able to reuse all of these abstractions, specifically
the ring and lattice hierarchy of abstraction, without recreating all of the
operators. I wanted you to be able to write

> type instance Multiplication Int = Mul
> type instance Addition Int = Add

and have the Ring instance be inferred for you based on the properties of Mul
and Add. I have accomplished this and now I'm putting it out here for people
to examine it and hopefully tell me what's wrong and what they would like to
see added or removed.

As such, issues and pull requests are encouraged. Thanks for reading.

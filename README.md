# Gaia

This library is meant to act as a logical basis for algebraic computation in Haskell,
and as such it offers many classes which are named in such a way that you can translate
between mathematical notation and Haskell notation as uniformly as possible. This is
done in a way that emphasizes user provided constraints: 

```Haskell

class Magma a where mul :: a -> a -> a
class Magma a => Idempotent a
class Magma a => Commutative a
class Magma a => Associative a
class Magma a => Unital a
class Magma a => Invertible a where inv :: a -> a -> a

```

These are accumulated in composite constraints and the notations which
refer to what these composite constraints define:

```Haskell

type Semigroup a = Associative a
type Monoid a = (Unital a, Semigroup a)
type Group a = (Invertible a, Monoid a)
type Abelian c a = (Commutative a, c a)

```

In this way you can specify what structure your algorithm processes, and then define it only using
those operations. When you've specified a process like that, it's as general as it can be,
and then you can operate over arbitrary data types and use facilities like rewrite rules and
specialization to achieve the efficiency you would have gotten if you had written the code
monotypically.

# Gaia

This library is a replacement for the standard Prelude which attempts to
rectify some of the contentions between the mathematical abstractions and
the ways in which they are often implemented in Haskell.

For instance, in Haskell we use heavily the notions of Semigroup and Monoid,
but really these are just instances of a more general structure, one which
indeed is rather structureless called a Magma. This is simply a type which
has some "multiplication" on it, as we often say. This just means that it
has a binary operation defined over it. In mathematics, we say that this
operation is over an arbitrary set, but in Haskell we say that it is over
a typed, which is why you won't hear me saying that it's "closed".

```Haskell
class Magma m where
  mul :: m -> m -> m

```

Now we have this abstraction, but we can implement this class for any type!
If this is true, why would we use a type class? This is the important part,
because this "mul" will basically sit at the base of our stack of abstractions
as we climb it. Let's see some other classes:

```Haskell
class Magma m => Semigroup m

(<>) :: Semigroup m => m -> m -> m

class Magma m => Commutative m

class Magma m => Neutral m where
  neutral :: m

class Magma m => Idempotent m

class Magma m => Invertible m where
  inv :: m -> m
```

We see that most of them don't have functions implemented, and Semigroup has a
function `(<>)` implemented externally of its type definition. We can clear that
up here:

```Haskell

(<>) = mul

```

We're reusing the operator now, but we're gaining some knowledge about the structure
we're working with. This is really going to be the strategy the whole way up, so I'll
jump to the Distributive abstraction, which is where it gets interesting again. A set
which is Distributive, by my terminology, has two operations over it which each have a
magma defined over them, one called Addition and the other Multiplication, such that
Multiplication distributes over Addition: `a * (b + c) = a * b + a * c`. In order to
accomplish this in Haskell, we need to take a step back and think, as we can't have two
different instances of Magma for any given class. That's when we have to think about
the mathematical concept of "carrier", which refers to the set associated with any given
magma, group, or algebraic structure. The user now must know that when he writes an instance
for Addition or Multiplication, the carriers of distribution as a mathematical structure, that
the magmas associated with each of these types distribute. This allows us to do no work in
defining the rest of the hierarchy, it will simply arise from the respoective properties of
the magmas involved. We can implement this as follows:

```Haskell

type family Addition a
type family Multiplication a


class (
    Coercible (Addition a) a
  , Coercible a (Addition a)
  , Coercible (Multiplication a) a
  , Coercible a (Multiplication a)
  , Magma (Addition a)
  , Magma (Multiplication a)
  ) => Distributive a where
  plus_ :: a -> a -> a
  plus_ a b = coerce (((coerce a) :: Addition a) `mul` (((coerce b) :: Addition a)))
  times_ :: a -> a -> a
  times_ a b = coerce (((coerce a) :: Multiplication a) `mul` (((coerce b) :: Multiplication a)))

(+) :: Distributive a => a -> a -> a
(+) = plus_

(*) :: Distributive a => a -> a -> a
(*) = times_

instance (
    Coercible (Addition a) a
  , Coercible a (Addition a)
  , Coercible (Multiplication a) a
  , Coercible a (Multiplication a)
  , Magma (Addition a)
  , Magma (Multiplication a)
  ) => Distributive a

```

After this, we define and instantiate other things such as Semirings:

```Haskell

class (
    Monoid (Addition a)
  , Monoid (Multiplication a)
  , Distributive a
  ) => Semiring a

instance (
    Monoid (Addition a)
  , Monoid (Multiplication a)
  , Distributive a
  ) => Semiring a

```

I think the point is made clear, and this is essentially the one innovation of this
library which I haven't seen elsewhere. I'd love to hear people's thoughts on this. 

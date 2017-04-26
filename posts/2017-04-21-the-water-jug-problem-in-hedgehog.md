---
title: The Water Jug Problem in Hedgehog
author: Ezequiel A. Alvarez
tags: haskell, hypothesis, hedgehog, quickcheck
---

A couple of weeks ago this [blog post](http://hypothesis.works/articles/how-not-to-die-hard-with-hypothesis/) over at the Hypothesis blog caught my eye. I had heard of TLA+ before, but the simplicity of the [linked solution](https://github.com/tlaplus/Examples/blob/master/specifications/DieHard/DieHard.tla) surprised me.

And the Python solution was very nice too, I totally have to bring Hypothesis to the office someday.

Anyway, some days later that same week, @jystic [announced the release of Hedgehog](https://www.reddit.com/r/haskell/comments/646k3d/ann_hedgehog_property_testing/), a property based testing library, much like QuickCheck but with a more modern approach.

Look, I'd never used QuickCheck or property testing for a real project before, so I was kind of surprised when I recognized some of the advantages @jystic said his library had. Specifically:

* No Arbitrary instances.
* * The amount of boilerplate needed to avoid orphan instances is tedious in general.
* Showing the values and code where a property fails.
* * The first *frictious* moment I had when using QuickCheck was exactly because of this.

So it was set. Let's try Hedgehog with the Water Jug problem.

## The problem

This is better explained in the Hypothesis post, but let's recapitulate: you have 2 water jugs that you can either fill or empty. One has 5 litter capacity and the other 3. You win by leaving 4 litters in the big one.

## The code

I didn't want many dependencies, so no state monads or funny folds. Just a good old finite state machine. The `fsm` function will take a **state** and a **step**, and return a new **state**.

We start by importing,

```haskell
module Jugs where

import           Hedgehog
import           Hedgehog.Internal.Property (TestLimit(..))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
```

and then defining our type for the problem:

```haskell
data Step = FillBig
          | FillSmall
          | EmptyBig
          | EmptySmall
          | SmallIntoBig
          | BigIntoSmall
          deriving (Show, Eq, Enum)
```

Those are all the different kinds of steps we'll have.

We will also need state to keep track of how much water our jugs have,

```haskell
data State = State
  { bigJug :: Int
  , smallJug :: Int
  } deriving (Show, Eq)
```

which by the way start out empty:

```haskell
initial :: State
initial = State { bigJug = 0, smallJug = 0}
```

Now, let's define what each step does:

```haskell
fsm :: State -> Step -> State
fsm s FillBig = s { bigJug = 5 }
fsm s FillSmall = s { smallJug = 3 }
fsm s EmptyBig = s { bigJug = 0 }
fsm s EmptySmall = s { smallJug = 0 }
fsm (State big small) SmallIntoBig =
    let big' = min 5 (big + small) in
    State { bigJug = big'
          , smallJug = small - (big' - big) }
fsm (State big small) BigIntoSmall =
    let small' = min 3 (big + small) in
    State { bigJug = big - (small' - small)
          , smallJug = small' }
```

That was easy. Let's also define a function that executes a list of steps from the initial state:

```haskell
execute :: [Step] -> State
execute = foldl fsm initial
```

Finally, the Hedgehog bits. We want to say: give me a random list of steps that has length between 0 and 20. We do it like so:

```haskell
steps :: Monad m => Gen m [Step]
steps = Gen.list (Range.linear 0 20) (Gen.enum FillBig BigIntoSmall)
```

Also we define our property, which we want to see proven false,

```haskell
prop_solution :: Property
prop_solution = withTests (TestLimit 5000) . property $ do
    s <- forAll steps
    let (State big small) = execute s
    assert $ big /= 4
```

also setting the TestLimit to 5000, so it doesn't give up too early.

## The solution

We can load this file using `stack ghci --package=hedgehog-0.1`, after adding hedgehog and any other missing deps to the global stack project (that is `~/.stack/global-project/stack.yaml`). Finally, we get:

```haskell
*Jugs λ> check prop_solution
  ✗ <interactive> failed after 3290 tests and 5 shrinks.

       ┏━━ Jugs.hs ━━━
    44 ┃ prop_solution :: Property
    45 ┃ prop_solution = withTests (TestLimit 5000) . property $ do
    46 ┃     s <- forAll steps
       ┃     │ [ FillBig
       ┃     │ , BigIntoSmall
       ┃     │ , EmptySmall
       ┃     │ , BigIntoSmall
       ┃     │ , FillBig
       ┃     │ , BigIntoSmall
       ┃     │ ]
    47 ┃     let (State big small) = execute s
    48 ┃     assert $ big /= 4
       ┃     ^^^^^^^^^^^^^^^^^
```

Isn't that pretty? It even has colors on my terminal.

Also, the solution is exactly the same the Hypothesis post had! (I saw other solutions, some even not optimal, but this one was the most common).

I think I'm gonna like Hedgehog.

In the meantime, I'll see about implementing the [generic solution](http://vigna.di.unimi.it/ftp/papers/Jugs.pdf) some day.

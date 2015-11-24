---
title: Tateti Tateti
author: Ezequiel A. Alvarez
tags: haskell,programming
---

In my eternal quest to learn Haskell I decided to implement a game in ncurses.

<img class="img-responsive" src="/files/tateti-tateti.png" width="400">

The code is over at [GitHub](https://github.com/alvare/tateti-tateti) if
anyone wants to check it out.

The game is "Ultimate Tic Tac Toe", a variant on
classic 3x3 Tic Tac Toe where inside each of the 9 cells there is another,
smaller cell, where actual gameplay takes place.
But there is a catch: the sub-board where the player plays is decided by the
playing position of the last player's move!

I came across this game a while ago in
[this blog post](http://mathwithbaddrawings.com/2013/06/16/ultimate-tic-tac-toe/).
The link also contains a better explanation of the rules, which *you should read* for any of this to make sense.

By the way, the main libraries I chose for this project are:

* [UI.NCurses](https://hackage.haskell.org/package/ncurses) for good old graphics
* [Lens.Simple](https://hackage.haskell.org/package/lens-simple) for lensing
* [Data.Array](https://hackage.haskell.org/package/array) as the main data structure for the board

## Some types for great programming

This is the main type:

```haskell
type Game a = StateT GameState Curses a
```

Which means `Game a` is a Monad that carries a state
of type `GameState`, another Monad inside named `Curses`, and returns some `a`.

Convenient fact: `Curses` is an instance of MonadIO which means we can use liftIO!

Then we have the game state:

```haskell
data GameState = GameState
    { _gPlayer :: Player
    , _gBoardState :: BoardState (BoardState (Maybe Player))
    , _gMode :: Mode
    , _gQuit :: Bool
    } deriving Show
```

This record holds most of the game mutable state, namely:

* who is currently playing
* the state of the board
* the game mode
* if they want to quit

The game mode is a simple sum type `data Mode = Free | Fixed`.
`Free` means the player can choose his/her next sub-board (for example in the
first turn), where `Fixed` means they can't (for example after a valid move
from the other player).

Now, notice that funny nested BoardState? Check out the type:

```haskell
data BoardState t = BoardState
    { _bsCells :: Array Position t
    , _bsPosition :: Position
    , _bsWinner :: Maybe Winner
    } deriving Show
```

A board is three things:

* a position (where the player currently is)
* a winner, maybe
* an array indexed by Positions populated by some thing `t`

Thus, the big board is just a board with an array of boards, where each one has
inside an array of `Maybe Player`, because either it's an empty cell or it has a
player's move.

All this arrays are indexed by the Position type:

```haskell
data Position = Position Vertical Horizontal
              deriving (Show, Eq, Ord, Ix)

data Vertical = T | M | B deriving (Show, Enum, Eq, Ord, Ix)

data Horizontal = L | C | R deriving (Show, Enum, Eq, Ord, Ix)
```

Where each letter stands for Top, Middle, Bottom (for Vertical) and Left, Center, Right (for Horizontal).

This means we can have a 3x3 array filled with ones as easily as:

```haskell
someAray :: Array Position Int
someArray = listArray (Position T L, Position B R) (repeat 1)
```

which you can index

```haskell
someArray ! (Position T R)
```

And of course, deriving `Ix` knows how to produce all the Position pairs!
Isn't that just lovely?

Finally just make some lenses:

```haskell
$(makeLenses ''GameState)
$(makeLenses ''BoardState)
```

I also took the liberty to add a lens for arrays (my first custom lens!):

```haskell
ax :: Ix i => i -> Lens (Array i a) (Array i a) a a
ax i = lens getter setter
  where
    getter = (! i)
    setter = (\arr v -> arr // [(i, v)])
```

## Imperative programming in the key of Monad

How does this look on action? Well, quite sexy I'd say. This is what happens when a player presses spacebar on a sub-board's cell:

```haskell
-- if the sub-board's cell is occupied return Nothing,
-- otherwise return the current sub-Position
actionPlayer :: Game (Maybe Position)
actionPlayer = do
    -- `use` applies a lens to the state, giving back it's contents
    pl <- use gPlayer

    -- of course, lenses compose
    pos <- use (gBoardState . bsPosition)

    -- `zoom` runs a State computation inside a piece of our bigger state,
    -- in this case, inside the sub-board at `pos`

    -- `bsAx p` is just `bsCells . ax p`
    zoom (gBoardState . bsAx pos) $ do

        pos' <- use bsPosition

        -- LambdaCase is surprisingly fun for State monads,
        -- you can get a field of the state and pattern match
        -- on it in a single line, just like in the imperative world
        -- (except the imperative world doesn't have pattern matching)
        use (bsAx pos') >>= \case

            -- sow now we are inside a cell in a sub-board and match:
            -- if the spot is already occupied, return Nothing
            Just _ -> return Nothing

            -- if the spot is free, assign the current player to it
            -- and return this position
            Nothing -> do
                bsAx pos' .= Just pl
                return $ Just pos'
```

Two things stand out:

This code looks very Python/Ruby/Javascript like. The syntax is different, but
there is less syntax too. I mean, we are super used to reading stuff
like `myObj['key'][thing].lol`, but that's just syntax.
On the other hand, things like `>>=` or `gBoardState . bsPosition` are functions.
You can check their types, you can pass them around and put them in a list.

(well yeah, `<-` is syntax too, but one that applies to every Monad ever not just
dictionaries)

The second thing is that `zoom` is great. With it you force a `do` block to have
access to only a part of the entire state, which of course makes your code
simpler to reason about and generally safer!

---

Drawing with ncurses was also pretty simple. For example, here we draw all
the board's crosses:

```haskell
drawCrosses :: GameState -> Colors -> Update ()
drawCrosses gs colors = do
    -- main cross
    drawCross 7 Nothing (0, 0)

    -- small crosses
    let offsets = [1, 1 + 8, 1 + 8 + 8]
        coords = (,) <$> offsets <*> offsets
        poss = range (Position T L, Position B R)
        winner p = gs ^. gBoardState . bsAx p . bsWinner
        color_ids = map (winner >=> return . colors . color) poss

    mapM_ (uncurry $ drawCross 1) $ zip color_ids coords
```

Where `drawCross` takes the size of a cell, a `Maybe ColorID` and a coordinate offset,
and just draws the horizontal and vertical lines.

Since smaller boards get colored when a player wins, we have to get their winners
and colors first.

Also `(,) <$> offsets <*> offsets` is funny. It builds a list of pairs
of all combinations of offsets.
Basically \\( \\{ (x, y) : x \\in S , y \\in S \\} \\)
with \\( S = \\{ 1, 1+8, 1+8+8 \\} \\) because each big-board cell
is 7 characters wide plus one for the line.

## The main loop

It looks like:

```haskell
-- I didn't want to add the windows and colors to the state nor use a Reader,
-- so I just used good old arguments
mainLoop :: Window -> Window -> Colors -> Game (Maybe Winner)
mainLoop w1 w2 colors = do

    -- we draw everything
    drawAll w1 w2 colors

    -- then, depending on the input
    parseInput w1 >>= \case
        -- if a movement key, move accordingly.
        -- remember: movement is just an update on the Position in the state
        -- this when rendered moves the cursor around
        Movement m -> movePlayer m

        -- if they pressed spacebar then
        Select -> use gMode >>= \case

            -- free mode: just lock into the sub-board
            Free -> do
                p <- use (gBoardState . bsPosition)
                use (gBoardState . bsAx p . bsWinner) >>= \case
                    -- unless board is already closed (has a winner)
                    Just _ -> return ()

                    -- board is open, enter
                    Nothing -> gMode .= Fixed

            -- fixed mode: lots of things
            Fixed -> actionPlayer >>= \case
                -- illegal action, do noting
                Nothing -> return ()

                -- legal action, `played_p` is where they played
                Just played_p -> do

                    -- calculate sub-board and big-board winners
                    p <- use (gBoardState . bsPosition)
                    gBoardState . bsAx p . bsWinner <~ innerWinner played_p

                    gBoardState . bsWinner <~ outerWinner p

                    -- switch players
                    gPlayer %= \x -> if x == X then O else X

                    -- move to next board
                    -- `%=` updates a field of the state with a function
                    gBoardState . bsPosition .= played_p

                    -- enter free mode if closed
                    use (gBoardState . bsAx played_p . bsWinner) >>= \case
                        Nothing -> return ()
                        Just _ -> gMode .= Free

        -- finally if quitted update the state
        Quit -> gQuit .= True

    use gQuit >>= \case
        -- now, if they quitted return
        True -> return Nothing

        -- otherwise, check if we have a winner and return that, or just loop
        False -> use (gBoardState . bsWinner) >>= \case
            Nothing -> mainLoop w1 w2 colors
            winner -> return winner
```

Well that's pretty big, but if you consider that's almost everything you need
for the main loop, it's quite interesting.

Also, see that last two lines? If you switch them around you introduce a bug, but don't fear, GHC will saves us (with -Wall enabled, which you should always do)
saying:

```bash
Warning:
    Pattern match(es) are overlapped
        In a case alternative: Nothing -> ...
```

I think that's super neat.

## Wrapping up

This was not my first time with a monad transformer, nor with
lenses, but I did learn some things. For instance, I could read some lens' errors!

While I can use lenses, the abstraction is quite ... abstract. But this time
I think I got a step closer to understanding it.

Testing was quite easy too. Since I could just spit the `GameState` to stderr (and pipe it to a file), I would start the game with a board full of moves!
All of this for free deriving Show and Read instances.

In the end, doing something actually playable is always fun :D

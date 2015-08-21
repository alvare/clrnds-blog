---
title: A Haskell space leak
author: Ezequiel A. Alvarez
tags: haskell, profiling
---

While working on [discogs2pg](https://github.com/alvare/discogs2pg), a tool for importing
[Discogs' data dumps](http://www.discogs.com/data/) into Postgre efficiently, I ran into a space leak.

So, in the beginning, things worked nice and in constant memory.

I opened the XML as a `Lazy.Bytestring`, `Text.XML.Expat.Tree` took that and gave me a lazy tree of nodes,
I made a list of records with those and finally, they were copied by `Database.PostgreSQL.Simple.Copy`
into de database.

The parsing and storing of the first XML file was done, so I move on to the next one, simplifying
and refactoring as needed, all while thinking "oh my Haskell is great, so so good".

## The problem

Suddenly, while trying the thing, I realized something was eating memory. My first suspect
was firefox, but `htop` quickly verified it was the parser.

I'm using [stack](https://github.com/commercialhaskell/stack), which is great
by the way, for the first time in a real project, and enabling profiling was super easy.

First I ran `stack build --executable-profiling --library-profiling` and it cried about `base` missing.
This was because I didn't have the profiling base library installed, something `apt` fixed right away.
Then I started trying different flags, since profiling in haskell is not really well documented,
even though it's really well supported.

I did

```bash
$ discogs2pg artists.xml +RTS -hy
$ hp2ps -c discogs.hp
$ evince discogs2pg.ps
```

and saw this, which may well be the epitomical example of a space leak:

<img src="/files/discogs2pg_memory_hy1.png">

So I [googled ARR_WORDS](http://stackoverflow.com/questions/7241470/what-is-arr-words-in-a-ghc-heap-profile)
and found it has to do with ByteStrings (I use a lot of those here because performance). The thing is,
I have no idea what they have to do with the space leak because I fixed it with a little change that
doesn't involve them.

But first, let me introduce you to the program.

## The program

The program ends in the `store` function which takes a list of things that implement Storable,
escapes them (generates some `ByteString.Builder`) and makes them `ByteString` for Postgre's COPY.
The class looks like this:

```haskell
class Storable a where
    getName :: [a] -> String
    getTables :: [a] -> [TableInfo]
    toRows :: a -> [Builder]
    avoid :: a -> Maybe String

data TableInfo = TableInfo
  { tableName :: String
  , tableColumns :: [String] }
```

Ignore the weird `[a] -> Stuff`, that's gonna change in the future. That just
says "a collection of 'a' has this attributes".

So, each Storable can become a `[Builder]` (one for each table) and
a list of them has a `[TableInfo]` (same size as the `[Builder]`), which is just a `String` (the table name)
and a `[String]` (the table columns). This is used in `store` like this:

```haskell
store :: (Show a, Storable a) => ConnectionInfo -> [a] -> IO ()
store conf values = do
    -- open a connection per table, TRUNCATE the table
    -- and start the COPY ... FROM STDIN
    conns <- forM (getTables values) $ \table -> do
        conn <- connectPostgreSQL conf
        begin conn
        _ <- execute_ conn $ fromString ("TRUNCATE " <> tableName table)
        copy_ conn $ toQuery table
        return conn

    -- then for each value
    forM_ values $ \val ->
        -- we get the [Builder]
        let rows = toRows val
        -- zip them with the connections list
        forM_ (zip conns rows) $ \(c, r) ->
            -- make them ByteString and send it to Postgre
            mapM_ (putCopyData c) (toChunks $ toLazyByteString r)

    -- finally, putCopyEnd for each connection, printing the table name
    -- in the process and the count of copied elements
    forM_ (zip (getTables values) $ conns) $ \(table, conn) -> do
        n <- putCopyEnd conn
        commit conn
        putStrLn $ tableName table <> " = " <> show n
```

Pretty imperative looking, but it works like a charm. Except for the space leak.

Now, the first thing I tried was refactoring the inner loop, avoiding currying and
composition but that didn't work.

I googled a while but nothing.

I was getting ready for making all my records strict, which I knew was
not the problem since it worked perfectly a day ago, and then fortunately
while looking around, I got rid of the duplicate `(getTables values)`
which was just bothering me ... and holy shit that was it!

## The fix

The fix is so simple and misterious it's not even funny:

```haskell
store :: (Show a, Storable a) => ConnectionInfo -> [a] -> IO ()
store conf values = do
    let tables = getTables values -- here

    conns <- forM tables $ \table -> do
        ...

    ...

    forM_ (zip tables conns) $ \(table, conn) -> do
        ...
```

Which generates this pretty graph:

<img src="/files/discogs2pg_memory_hy2.png">

Somehow a list of `String` was causing a 60MB space leak of ARR_WORDS.

Damn.

I love Haskell.

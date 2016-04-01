---
title: Scraping Dr. Who's IMDB ratings
author: Ezequiel A. Alvarez
tags: haskell, d3, viz, scraping, dr who
---

Dr. Who is a hell of a good show, it matches the right amount of sci-fi
with a young heart and an amazing production team.

I've been trying to watch season 9 since it came out,
but somehow I don't feel the show as I did before, so I thought,
is it me or did the quality change? Let's plot it and see!

<div id="viz"></div>
<div id="info">
<b>Season: </b><span id="info-season"></span>
<b>Episode: </b><span id="info-number"></span>
<b>Date: </b><span id="info-date"></span>

<b>Title: </b><span id="info-title"></span>

<b>Rating: </b><span id="info-rating"></span>
</div>

So, first I did some bash-ing of the episodes by season:

```bash
for x in {1..9}; do
    wget "http://www.imdb.com/title/tt0436992/episodes?season=$x";
done
```

Then, I wanted to try [scalpel](https://hackage.haskell.org/package/scalpel),
a Haskell scraper. It is a really cool thing, for example:

```haskell
title :: Scraper Text Text
title = attr "title" $ "a" @: ["itemprop" @= "name"]
```

matches an "a" tag with the "itemprop" attribute set to "name", and the extracts
the "title" attribute value.
[Here is the full gist.](https://gist.github.com/alvare/3f5221fe2b053265b0b935da8bc374f8)

Lastly I added some D3js pain (sorry JavaScript, you are awful) and I got the plot.

Now, a couple of things grab my attention:

### Linear regression is boring

I spent around an hour plotting the linear regression (it's the light-gray line),
but it turned out to be almost constant!

This kind-of-means that I was wrong, the show is neither "better" or "worse"
(for whatever that means in the [context of IMDB ratings](http://www.imdb.com/title/tt2975590/?ref_=fn_al_tt_3)).

Actually the slope of the regression is -0.008453710369452755 so yeah, not enough
for my elitist palate.

### Jam the lines

The lines cross the mean (which is exactly 8, by the way) a lot of times. Episode
ratings alternate a lot between almost-7★ and not-quite-9★. (If you are curious, the median gets crossed 40 times.)

For example, episode 9-9 (Sleep No More) rates at 6.2★, while the next one
(Face The Raven) jumps all the way up to 8.8★!
It's a difference of 2.6 stars, the largest one in consecutive episodes.
Even more, the previous one, episode 9-8 (The Zygon Inversion) is an 8.7★.

Make your own assumptions (because I won't make a single one, seriously, it doesn't mean anything).

### Season finales

Every season finale scores better than the first episode of the next season, which
is kinda obvious considering Dr. Who usually ends seasons in
a time-galatic war between pan-dimensional hamburgers and robo-bears.

But wait a minute, **except** for season 9!

There it is, I just made a hiatus on an exceptional low note.

Question answered.

Good bye.

<script src="/js/lodash.min.js"></script>
<script src="/js/dr-who-ratings.js"></script>
<style>
#viz {
    font: 13px sans-serif;
}

#info {
    font: 13px sans-serif;
}

.axis path,
.axis line {
    fill: none;
    stroke: #000;
    shape-rendering: crispEdges;
}

.y.axis path,
.x.axis path {
    display: none;
}

.line {
    fill: none;
    stroke-width: 1.5px;
}

.overlay {
    fill: none;
    pointer-events: all;
}

.focus {
    fill: none;
    stroke-width: 1px;
    stroke: black;
}
</style>

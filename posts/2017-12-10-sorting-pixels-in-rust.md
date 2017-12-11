---
title: Sorting pixels in Rust
author: Ezequiel A. Alvarez
tags: rust, image, pixels
---

Some time ago I decided to make something in Rust so
my snobbish commentaries regarding the language
would, at least, be honest.

Truthful to my style, and because it's usually the best way
to try a language, I decided to do something _industrialistic_,
something that takes command line arguments, has error
messages and is efficient. I find that this kind of project
leaves you with a bigger picture of the language, community and libraries
than doing just an exercise on it's features and ideas.

The project I'll go trough in this blog post is a pixel sorter.
It takes an image like this

<img class="img-responsive" src="/files/pepsi.png">

and turns it into this

<img class="img-responsive" src="/files/pepsi_sorted_red.png">

In this example, the pixels where sorted by _redness_ which is the default,
but it can sort them by many dimensions: red, green, blue, alpha, hue, saturation and lightness.

The code is over at [GitHub](https://github.com/alvare/imgsort), so feel free
to criticize/improve it.

# Cargo is love

Having worked with C++ a lot this year I think now I can really appreciate
how great `cargo` is, and `rustup` too. Dependency handling, which can take literally
between a day and a week in a new C++ project, is solved with 3 commands.
Just wanted to say that, and cheers to the `cargo` team!

Also, for reference, this was done in Rust nightly 1.24.

# First impressions

First of all, let me say I have but the deepest admiration to
the team behind Rust. The language is barely
7 years old but it already feels extremely robust, and can,
in occasion, feel really elegant.

On the other hand, getting my head around lifetimes was, I admit,
tricky, but even worse were traits!
I spent days just browsing trough the [image](https://docs.rs/image/0.18.0/image/) crate's
documentation, trying to get my head around all the possible ways to access the underlying pixel data.

Things like

```rust
impl<P, Container> GenericImage for ImageBuffer<P, Container>
where
    P: Pixel + 'static,
    Container: Deref<Target = [P::Subpixel]> + DerefMut,
    P::Subpixel: 'static,
  type Pixel = P;
```

would confuse me for a long time. Specially because of the mix between
types, traits and lifetime parameters that occur.

And I have a decent Haskell background, I should be ready for this kind of things ...

By the way, that is basically saying that, an `ImageBuffer` parameterized
over such `P` and `Container` types, implements the `GenericImage` trait and thus
has all the trait's methods.

On the other hand, this crate does a **lot** of things, and it manages to do that in an orderly,
pseudo type-safe fashion.

# From the top

Reading the `main` function in `main.rs` gives us a basic idea of the full program:

1. parse some options
2. try to open and decode the image file
3. sort it
4. guess the format of the output file (which could be different from the input file)
5. create the empty file
6. write to it

Every step except for 3 can fail, and in that case it prints the error and exits.

Pattern matching does look cool too, and it let's you declare and define
a variable on a conditional (something which C++ makes really complicated):

```rust
let img = match image::open(&opts.inpath) {
    Ok(f) => f,
    Err(err) => {
        eprintln!("{}", err);
        process::exit(1);
    }
};
```

Yes, I know this is the canonical either monad example, but Rust makes it
more elegant to do it like this (for me).

## Option parsing

This is done in `options.rs` and is extremely straight forward. All the logic is expressed
in one single expression:

```rust
let matches = App::new(env!("CARGO_PKG_NAME"))
    .version(env!("CARGO_PKG_VERSION"))
    .about(env!("CARGO_PKG_DESCRIPTION"))
    .arg(Arg::with_name("mode")
        .help("Sort dimension")
        .long("mode")
        .short("m")
        .takes_value(true)
        .default_value("red")
        .possible_values(&["red", "green", "blue", "alpha",
                           "hue", "sat", "lig"]))
    .arg(Arg::with_name("INFILE")
        .help("Input image")
        .required(true))
    .arg(Arg::with_name("OUTFILE")
        .help("Output image")
        .required(true))
    .get_matches_from(args);
```

I can't imagine it being simpler or clearer,
props to the team behind [clap](http://crates.io/crates/clap).

Also, the `env!` macros bring information over from the cargo environment,
again an excellent feature of the build system.

## Almost sorting

The sorting function's type signature looks like

```rust
fn sort_pixels(img: &image::DynamicImage, mode: &options::Mode) -> image::RgbaImage
```

The program starts by choosing which function to use when sorting the pixels.
This is (almost) very simple:

```rust
let key_fn = match *mode {
    options::Mode::Red => sorters::get_red,
    options::Mode::Green => sorters::get_green,
    options::Mode::Blue => sorters::get_blue,
    options::Mode::Alpha => sorters::get_alpha,
    options::Mode::Hue => sorters::get_hue,
    options::Mode::Saturation => sorters::get_sat,
    options::Mode::Lightness => sorters::get_lig,
};
```

Where the `sorters` namespace has functions of the type `(p: &&image::Rgba<u8>) -> u8`.

Now, what's up with `*mode`? This is actually quite interesting. We are matching
on a reference, thus we can either match on references (for example, `&options::Mode::Red`)
or dereference it. This language wart isn't a big deal,
but is nevertheless being looked at over at
[RFC 2005](https://github.com/rust-lang/rfcs/blob/master/text/2005-match-ergonomics.md).

Fun thing: the compiler error actually takes you there. Nice detail.

```
error: non-reference pattern used to match a reference (see issue #42640)
```

## Actually sorting

This is where I spent most of the time, and still couldn't get it to do
exactly what I wanted to do, though it works.

Here is the actual sorting code:

```rust
let buf2 = buf.clone();
let mut sorted_pixels: Vec<_> = buf2.pixels().collect();
sorted_pixels.sort_by_key(key_fn);

for (i, pixel) in buf.pixels_mut().enumerate() {
    *pixel = *sorted_pixels[i];
}
```

Here, `buf` is a `RgbImage = ImageBuffer<Rgb<u8>, Vec<u8>>`, and as hard as I tried
I couldn't get to sort that without copying it first into a temporary `buf2`
and then iterating over the original buffer, assigning each position the sorted
result's pixel.

I think it is possible, but I already spent too much time trying, so
if you kind reader know how, please contact me.

# Tests?

Testing in cargo is okay. It's cool to have testing directly built on the build tool,
but it's a little awkward to use, and extremely dependent on file and folder names.

For example you need to include the test module in main:

```rust
#[cfg(test)]
mod tests;
```

Nevertheless, it works like a charm.

# Wrapping up

For me, the most amazing bit was the tooling. I haven't tried the debugger (does it have one???)
but `cargo`, [docs.rs](https://docs.rs/) and `rustup` make it really simple and ergonomic to
work with other people's code, which for me is a crucial aspect of modern software development.

Safety was also not a problem. I had zero out-of-range-access errors
but the code is kind of high level, which makes it not so surprising.

Build times where quite short too: while an order of magnitude slower than C++,
think about this: This blog is made in Haskell, and I had to rebuild it from scratch
when starting to write this post because I had deleted the `~/.stack/` folder ... well,
it just finished building right now.

That's almost 2 hours.

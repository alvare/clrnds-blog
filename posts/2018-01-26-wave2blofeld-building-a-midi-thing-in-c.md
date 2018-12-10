---
title: "wave2blofeld: Building a MIDI thing in C++"
author: Ezequiel A. Alvarez
tags: c++, midi, wav, sysex, audio, blofeld
---

Last year's November I went to the amazing [DuraznoConf](duraznoconf.uy/),
which I must emphasize was an awesome experience, and during my free time there
I got to finally finish a project of mine which I had sitting around for too long:
[wave2blofeld](https://github.com/alvare/wave2blofeld).

Basically the Waldorf Blofeld, a lovely synthesizer I happen to own, has
something called [WaveTable synthesis](http://en.wikiaudio.org/Wavetable_synthesis)
which means it has literal wave tables (actually arrays though) that it
iterates trough and other synth stuff. In the Blofeld, each wave table has
64 waves, and each one of these waves has 128 samples.

Now the fun part: you can load your own wave tables!

The catch is that there is no nice official way to do it 😬

So in order to maximize compatibility, and because why not, I decided to do it
in C++ (or Rust, but the only MIDI lib I found was too beta for me).

`wave2blofeld` takes a 8192 samples WAV file and spits out to a MIDI file which you
can transfer to your Blofeld using SysEx Librarian, for example.

## Dependencies

Just run. Run as fast as you can (?)

Dependency management in C++ deserves it's
own dystopic novel. I tried a couple of solutions like [Buckaroo](https://buckaroo.pm/)
but guess what happens when you [search for midi](https://buckaroo.pm/search?q=midi).

Essentialy, you are on your own, so after losing more hours than I'd want I wrote
a 28 lines `Makefile` which works like a charm. The thing is, since C++ package management
is such a complicated issue, library writers usually end up doing their stuff as
easy to build as possible, having no dependencies and sometimes even only header files!

Take for example this line from TCLAP's home:

> The library is implemented entirely in header files making it easy to use and distribute with other software.

Having to build libraries without depending on anything is a serious drawback.

Anyway, for this thing I ended up using:

* [midifile](https://github.com/craigsapp/midifile) for, ehm, MIDI files
* [audiofile](https://github.com/craigsapp/midifile) for you can guess what
* [TCLAP](https://github.com/craigsapp/midifile) for parsing command line options

## Building

Last year I ended up having to learn CMake because my algorithms class teachers
weren't really good at it, so I went with it. It's fairly easy to get started and possibly
demential to learn in depth (the manual never ends). The whole thing
is only a dozen lines long:

```cmake
cmake_minimum_required(VERSION 3.0)
project(wave2blofeld)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_BUILD_TYPE Debug)

link_directories(deps/midifile-master/lib)

include_directories(deps/midifile-master/include)
include_directories(deps/audiofile-master)
include_directories(deps/tclap-1.2.1/include)

file(GLOB SOURCES "src/*.cpp" "deps/audiofile-master/*.cpp")

add_executable(wave2blofeld ${SOURCES})
target_link_libraries(wave2blofeld midifile)
```

It definitely beats having to write a Makefile 😂. Also, it is kind of readable.

With that out of the way, building is as easy as creating a `build` folder and
inside it running `cmake ..` followed by `make`.

## Actually programming

Building a MIDI file is not complicated, but a SysEx file ... that took me a while.

Basically MIDI has this System Extended message format, which manufacturers can use
for anything they like. Of course this means it's all done in binary.

Thankfully for reference I had [Jonas Norling's page](http://www.lysator.liu.se/~norling/blofeld.html)
which kindly offers the binary format explained and a reference implementation.

For starters, the Blofeld wants 128 samples of big-endian signed 21-bit integers each, that is
integers between -1048575 and 1048575. `audiofile` loads samples as floating point numbers
between -1 and 1, so each loop we normalize every sample and then load it in 7-bit triplets
[Edit: Jonas' post says "21-byte little-endian" numbers, which is weird because 21-bytes is a lot
and anyway little-endian didn't work for me, so I guess that's a typo].

```cpp
    for (int i = 0; i < 128; ++i){
        mm[8 + 3*i    ] = (samples[i + wave*128] >> 14) & 0x7f;
        mm[8 + 3*i + 1] = (samples[i + wave*128] >>  7) & 0x7f;
        mm[8 + 3*i + 2] = (samples[i + wave*128]      ) & 0x7f;
    }
```

Then it's a matter of following the spec, and building a 410 byte array
which is the SysEx message. We need to send a message for each of the 64 waves of the wave table.

Finally, a touch of iterators for adding every byte of the wave, which
makes a kind of checksum:

```cpp
    int checksum = std::accumulate(
        mm.begin()+7, mm.begin()+407, 0);
```

and done

```cpp
    mf.addEvent(0, 0, mm);
```

Also don't forget, you need a lamdba to make it really _C++11_:

```cpp
bool isValidName(std::string& s){
    return std::all_of(s.begin(), s.end(),
        [](char& c){
            return 0x20 <= c and c <= 0x7f;
        });
}
```

## Command line options

This was ok. It's not [Rust's clap](http://crates.io/crates/clap) or
[Haskell's optparse-applicative](https://hackage.haskell.org/package/optparse-applicative)
but it works. In short you need to build arguments like

```cpp
    TCLAP::ValueArg<unsigned int>
        slot("s", "slot", "Wavetable to write to. Between 80 and 118.", true, 0, "slot");
    cmd.add(slot);
```

and the library's "template magic" does some validation.

Notice how you need to add the argument to the `TCLAP::CmdLine cmd` object **unless**
it's a `TCLAP::SwitchArg` in which case you pass `cmd` as an argument. Why?
Because software 💦.

## Closing

In the end, it works like a charm. I also have plans to somehow add this functionality to
[WaveEdit](https://github.com/AndrewBelt/WaveEdit) which is super cool.

For now, you need to export the wave table from WaveEdit, which exports them as 64
waves of 256 samples each (not 128), and convert them using wave2blofeld's `-d`
switch which will subsample each sample. Not the best solution but hey it works.

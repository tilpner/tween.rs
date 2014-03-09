redox-tween
===========

*This repository contains code that has not been properly tested yet, continue
at the risk of doing stupid things while discovering parts of this library
don't work.*

## Introduction

Games and other applications often have a need to change values smoothly,
without the need for a fully-blown physics system. This is where
[Tweening][wikipedia] (Inbe **tween** ing) comes in. It allows to interpolate
between numerical values in a lightweight way.

[This YouTube video][youtube-juice] is an interesting example of how the
addition of animations (via tweens) can change the feeling of a game.

[wikipedia]: http://en.wikipedia.org/wiki/Inbetweening
[youtube-juice]: http://www.youtube.com/watch?v=Fy0aCDmgnxg

## Example

See [the examples][examples].

Compiling and running `images.rs` *should* create `.ppm` files of all basic
variations of tweens in `/tmp/`. If that directory doesn't exit for you, you
should change the hardcoded path. (Yes, it should be an argument)

More complex examples will follow.

[examples]: /examples

## Features

- Allows tweening of any type (via traits)
- Multiple easing equations:
 - Linear
 - Quad
 - Cubic
 - Quart
 - Quint
 - Sine
 - Circular
 - Back
 - Elastic
 - Bounce
- Easy to add own equations
- Easing modes `In`, `Out` and `InOut`
- Tween organization:
 - Sequential execution
 - Parallel execution
 - Pauses
 - Function execution
 - Repeated execution
- Three value access modes
 - via unsafe pointers
 - via `Cell`
 - via callback functions

## Todo

- Direct `task` launching
- Making tweens `Send`able?

## Feedback and contribution

Feedback in any form is strongly desired. Either email me at
`ubrccare.gvyy@tznvy.pbz` ([rot-13 that][rot13]), create an issue or ping me
on IRC (nick: flan3002).

[rot13]: http://www.rot13.com/

## Thanks

Many thanks to eddyb, cmr, bjz, kimundi, mindslight, hoverbear, sfackler, and
others from [mozilla/#rust][irc]!  Also thanks to lifthrasiir from reddit for
helping me with a `'static` problem.

[irc]: http://client00.chat.mibbit.com/?server=irc.mozilla.org&channel=%23rust

## Resources

For preview of the easing equations you may visit [easings.net][easings] for
an overview of different easings. Note that `Expo` is not implemented at time
of writing.

[easings]: http://easings.net/

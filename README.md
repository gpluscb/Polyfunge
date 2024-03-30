Haskell implementation of [polyfunge](https://gitlab.com/alice-lefebvre/polyfunge).

Polyfunge is an esoteric programming language where you send unsuspecting values through a maze of various unethical scientific experiments
such as redirection, cloning, annihilation through collision, jump pads, and fusion.
Maybe you'll even manage to compute digits of pi while you're at it.

Values start by moving down and are affected by the various blocks.
See [the specification](https://gitlab.com/alice-lefebvre/polyfunge/-/blob/main/readme.md) for details.

Example program computing the [Fibonacci sequence](https://en.wikipedia.org/wiki/Fibonacci_sequence), [courtesy of Alice](https://gitlab.com/alice-lefebvre/polyfunge/-/blob/main/examples/fibonacci.pf):

```
1
 x
v    <
::v 
pwz 
 w
 >|v  
  x
>  + ^
```

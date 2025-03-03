# Champ: Fast immutable HashMaps for Haskell

This is a Haskell implementation of immutable persistent HashMaps and HashSets 
based on the Compressed Hash Array-Mapped Prefix-trie ('CHAMP') structure.

These are similar to Hash Array-Mapped Tries ('HAMT') structures,
such as those implemented by [unordered-containers](https://hackage.haskell.org/package/unordered-containers)
but CHAMP maps/sets use significantly less memory and are significantly more performant.

## Basic Usage

_To be written_

## How does CHAMP work?

The basic improvement over HAMT is that we store _two_ 32-bit bitmaps.
together each pair of bits indicates 'not in here' (if both are zero), 'exists inline' (`01`) or 'exists in child' (`10`). (`11` is unused).
This then allows us to store all inline-children right next to each-other in one array (or rather: in a keys-array and a values-array)
and store all child-nodes in their own array.

Side note: In the original papers, a single array was used for both, but using separate arrays
works better with Haskell's runtime, because it allows us to make different choices
as to what _kind_ the key-type, value-type and child-nodes should be: Lazy, Strict (boxed), or fully unboxed.

Storing these inline key-value pairs separate from the nested children:
- allows us to get rid of intermediate boxes around the inline key-value pairs
- allows us to implement recursive algorithms as a tight loop over the inline key-value pairs (with great data locality, everything already in the cache line!), followed by a loop over the children

This greatly improves performance compared to HAMT implementations.

_TODO: Double-check memory calculations in this section_

Compared to `Data.HashMap.Lazy`, whose overhead for `n` key-value pairs is roughly `5*n + 3*log32(n)` because every key-value pair is wrapped in an intermediate [Leaf](https://hackage.haskell.org/package/unordered-containers-0.2.20/docs/Data-HashMap-Internal.html#t:Leaf) datatype.
 a `Champ.HashMapBL` only has an overhead of `2*n + 8*log32(n)`.

The same is true for a `Champ.HashMapBB`, but here we're also aware that the values are strict
(they are stored, just as the keys, as `UnboxedType`s). Therefore, GHC knows it never needs to force a thunk after reading.
This results in more optimized assembly when compiling.

But it does not stop there: Since we're able to decide whether we want to store the keys and/or values in boxed or unboxed fashion,
we can also get rid of the last two pointers of indirection (for types that support it).
When unboxing both keys and values, this drops down to only the `8*log32(n)` spine overhead.

Finally, we can also build a `HashSet` that shares the same underlying implementation
as `HashMap` without [paying for an extra word per stored element by storing a `()-pointer`](https://hackage.haskell.org/package/unordered-containers-0.2.20/docs/src/Data.HashSet.Internal.html#HashSet). 
For a `Champ.HashSetB` the overhead is `N+7*log32(n)`. For a `Champ.HashSetU` the overhead is again a simple `7*log32(n)`.

### Further Reading

- [Steindorfer, M. J. (2017). Efficient immutable collections (Doctoral dissertation, Universiteit van Amsterdam [Host]).](https://michael.steindorfer.name/publications/phd-thesis-efficient-immutable-collections)
- [Steindorfer, Michael J., and Jurgen J. Vinju. "Optimizing hash-array mapped tries for fast and lean immutable JVM collections." Proceedings of the 2015 ACM SIGPLAN International Conference on Object-Oriented Programming, Systems, Languages, and Applications. 2015.](https://michael.steindorfer.name/publications/oopsla15.pdf)
- Reference implementation: [Capsule](https://github.com/usethesource/capsule). That repository also contains many other links to publications and talks about the subject.

## Sponsors

Part of this work was sponsored by [Channable](https://www.channable.com/)

<a href="https://www.channable.com"><img src="https://www.channable.com/_next/image?url=https%3A%2F%2Fmedia.graphassets.com%2FcntRnbAcSrCmS1wLTWrb&w=256&q=100" height="48"  ></a>

---

## Development

* Q: In my IDE, HLS does not work for some modules, saying "can't execute `cpphs`".
  * A: Make sure `cpphs` is available in your path, for example using `cabal install cpphs` (and then restart your HLS). `cpphs` is a portable Haskell implementation of the C preproessor macro parser. (We use CPP to reduce boilerplate for the eight different kinds of hashmaps we support, and crucially rely on 'token pasting' which is done differently by different versions of `gcc`, `clang`, etc.).

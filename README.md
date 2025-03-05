# Champ: Fast immutable HashMaps for Haskell

This is a Haskell implementation of immutable persistent HashMaps and HashSets 
based on the Compressed Hash Array-Mapped Prefix-trie ('CHAMP') structure.

These are similar to Hash Array-Mapped Tries ('HAMT') structures,
such as those implemented by [unordered-containers](https://hackage.haskell.org/package/unordered-containers)
but CHAMP maps/sets use significantly less memory and are significantly more performant.

## Basic Usage

The `Champ` root module only contains commonly-used types, and (all or some of it) can be imported non-qualified.
The `Champ.HashMap` resp. `Champ.HashSet` modules are intended to be imported qualified:

```haskell
import Champ
import qualified Champ.HashMap
import qualified Champ.HashSet
```

If you're currently using `unordered-containers`' `Data.HashMap`/`Data.HashSet`, you might be able to switch out those types
for `Champ`'s by just switching which module you import.

`Champ` comes with many different types of `HashMap`, see the dedicated section below for details.
`HashMapBL` corresponds to a 'lazy hashmap' and `HashMapBB` to a 'strict hashmap'.

Some simple usage:

### Create a HashMap from a list:

```haskell
>>> x = Champ.HashMap.fromList [("a", 1), ("b", 2), ("c", 3)] :: HashMapBB
Champ.HashMap.fromList [("a", 1), ("b", 2), ("c", 3)]
```

If `OverloadedLists` is enabled, list literals can also immediately be used to construct hashmap constants.

### Insert, update, or delete individual elements

```haskell
>>> y = Champ.HashMap.insert "e" 42 x
Champ.HashMap.fromList [("a", 1), ("b", 2), ("c", 3), ("e", 42)]
```

```haskell
>>> y = Champ.HashMap.delete "b" x
Champ.HashMap.fromList [("a", 1), ("c", 3)]
```

```haskell
>>> y = Champ.HashMap.update "b" (+100) x
Champ.HashMap.fromList [("a", 1), ("b", 102), ("c", 3)]
```

### Checking membership

```haskell
>>> Champ.HashMap.member "b" x
True
>>> Champ.HashMap.member "z" x
False
```

### Size

```haskell
>>> Champ.HashMap.size x
3
```
**Champ hashmaps keep track of their size.**

This means that looking up the size takes O(1) constant-time,
rather than `O(n)` in the case of `unordered-containers`.

### Other

This is only the tip of the iceberg, there are a _lot_ of useful functions
to manipulate HashMaps and HashSets with!

### Types

There is not a single `HashMap` type.
You might notice that most functions have signatures of the shape `MapRepr keys vals k v => HashMap keys vals k v -> ...`.

This is because we have _many_ different kinds of hashmaps with different semantics for the key and value types.
Depending on whether you want lazy or strict semantics,
and whether your keys or values can be unboxed or not, you can choose one of the following:

| Type        | Key semantics     |  Value semantics    |  Notes/Constraints                         |
|-------------|-------------------|---------------------|--------------------------------------------|
| HashMapBL   | Strict boxed keys | Lazy values         |  Same semantics as 'Data.HashMap.Lazy'     |
| HashMapBB   | Strict boxed keys | Strict boxed values |  Same semantics as 'Data.HashMap.Strict'   |
| HashMapBU   | Strict boxed keys | Unboxed values      |  Requires 'Prim v'                         |
| HashMapBUl  | Strict boxed keys | Unlifted values     |  Requires 'PrimUnlifted v'                 |
| HashMapUL   | Unboxed keys      | Lazy values         |  Requires 'Prim k'                         |
| HashMapUB   | Unboxed keys      | Strict boxed values |  Requires 'Prim k'                         |
| HashMapUU   | Unboxed keys      | Unboxed values      |  Requires 'Prim k, Prim v'                 |
| HashMapUUl  | Unboxed keys      | Unlifted values     |  Requires 'Prim k, PrimUnlifted v'         |
| HashMapUlL  | Unlifted keys     | Lazy values         |  Requires 'PrimUnlifted k'                 |
| HashMapUlB  | Unlifted keys     | Strict boxed values |  Requires 'PrimUnlifted k'                 |
| HashMapUlU  | Unlifted keys     | Unboxed values      |  Requires 'PrimUnlifted k, Prim v'         |
| HashMapUlUl | Unlifted keys     | Unlifted values     |  Requires 'PrimUnlifted k, PrimUnlifted v' |

| Type       |  Value semantics      |  Notes/Constraints                         |
|------------|-----------------------|--------------------------------------------|
| HashSetB   | Strict boxed elements |  Same semantics as 'Data.HashSet' _(but much smaller)_ |
| HashSetU   | Unboxed elements      |  Requires 'Prim v'                         |
| HashSetUl  | Unlifted elements     |  Requires 'PrimUnlifted v'                 |


`HashMapBL` and `HashMapBB` (and `HashSetB`) are are the easiest/most flexible to work with,
since any normal Haskell type can be used as key or value type (and therefore these kinds of hashmaps can implement all typeclasses you'd expect).

On the other hand, by unlifting or even unboxing the keys and/or values, a lot of extra performance is gained.
However, this requires using e.g. `Champ.HashMap.map` and `Champ.HashMap.foldr` instead of `Functor`'s `fmap` or `Foldable`'s `foldr`
since adding constraints on the key resp. value types restricts what typeclasses can be implemented.

It is recommended in application code to pick a concrete map type to use,
because this will allow GHC to create an optimized implementation.

In library code, either pick a concrete type, or write it against a fully general `MapRepr keys vals k v => HashMap keys vals k v`
and add `SPECIALIZE` pragmas for each of the concrete types for optimal performance.

## How does CHAMP work?

The basic improvement over HAMT is that we store _two_ 32-bit bitmaps.
together each pair of bits indicates 'not in here' (if both are zero), 'exists inline' (`01`) or 'exists in child' (`10`). (`11` is unused).
This then allows us to store all inline-children together, and separately store all child-nodes together.
and store all child-nodes in their own array.

Side note: In the original paper, which focused on the Java Virtual Machine, a single array was used for both inline key-value-pairs and child-nodes. 
But for the Haskell (GHC) runtime, using three specialized arrays, one for the inline keys, one for the inline values, and one for the child-nodes works better,
because it allows us to make different choices as to what _kind_ the key-type, value-type and child-nodes should be: Lazy, Strict (boxed), or fully unboxed.

Storing these inline key-value pairs together rather than intermingled with nested children:
- allows us to get rid of intermediate boxes around the inline key-value pairs, greatly reducing memory usage
- allows us to implement recursive algorithms as _first_ a tight loop over the inline key-value pairs (with great data locality, everything already in the cache line!), and _then_ a loop over the children.

This greatly improves performance compared to HAMT implementations.

### Memory usage

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

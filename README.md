# Champ: Persistent HashMaps

This is a Haskell implementation of persistent HashMaps and HashSets 
based on the Compressed Hash Array-Mapped Prefix-trie ('CHAMP') structure.

These are similar to Hash Array-Mapped Tries ('HAMT') structures,
such as those implemented by [unordered-containers](https://hackage.haskell.org/package/unordered-containers)
but CHAMP maps/sets use significantly less memory and are significantly more performant.

## Basic Usage

_To be written_

## Further Reading

- [Steindorfer, M. J. (2017). Efficient immutable collections (Doctoral dissertation, Universiteit van Amsterdam [Host]).](https://michael.steindorfer.name/publications/phd-thesis-efficient-immutable-collections)
- [Steindorfer, Michael J., and Jurgen J. Vinju. "Optimizing hash-array mapped tries for fast and lean immutable JVM collections." Proceedings of the 2015 ACM SIGPLAN International Conference on Object-Oriented Programming, Systems, Languages, and Applications. 2015.](https://michael.steindorfer.name/publications/oopsla15.pdf)
- Reference implementation: [Capsule](https://github.com/usethesource/capsule). That repository also contains many other links to publications and talks about the subject.

## Sponsors

Part of this work was sponsored by [Channable](https://www.channable.com/)

<a href="https://www.channable.com"><img src="https://www.channable.com/_next/image?url=https%3A%2F%2Fmedia.graphassets.com%2FcntRnbAcSrCmS1wLTWrb&w=256&q=100" align="left" height="48"  ></a>

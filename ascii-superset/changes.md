- Drop support for `base` 4.11 (GHC 8.4)

- Drop support for `base` 4.12 (GHC 8.6)

- Modify documentation on the `Lift` class. Previously it indicated that the class was for converting ASCII into supersets of ASCII. The class's purpose is now restated as being a conversion from any character set to any other larger character set. The purpose is to indicate that the ASCII subset types defined in the `ascii-numbers` package may reasonably have `Lift` instances.

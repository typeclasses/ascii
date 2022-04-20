Removed dependency on `d10`. The `D10` type from the `d10` module is replaced with the new `Digit` type instead. This type better semantically represents an ASCII character rather than an abstract 0-9 number, and this allows us to more confidently define instances for it that befit its ASCII meaning. The constructor names now have the full word `Digit` in them rather than `D`, which is consistent with the constructor names in `ASCII.Char` and `ASCII.Hexadecimal`.

Renamed `D16` to `Word4`. This is now defined in a separate module, `ASCII.Word4`. The constructors are renamed from `D...` to `Number...`. The `Word4` type might someday belong in a separate package, because the concept can exist separately from ASCII.

Added `Lift` instances:

```haskell
instance DigitSuperset       x => Lift Digit     x
instance DigitStringSuperset x => Lift [Digit]   x
instance HexCharSuperset     x => Lift HexChar   x
instance HexStringSuperset   x => Lift [HexChar] x
```

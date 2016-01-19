rhcalc
======

rhcalc is a Reverse Polish Notation (RPN) calculator written in Haskell.

Usage
-----
Run `cabal install` to build, then `rhcalc`.

Features
--------
rhcalc performs exact computations on integers and fractions. The division of two integers is a fraction, not a decimal number.

Basic operations: + - * / ^, inverse (`inv`), negative (`n`), absolute value (`abs`), conversion from exact to decimal (`num`), rounding (`rnd`, `floor`, `ceil`).

Math functions: trigonometry (`cos`, `sin`, `tan`, angles in radians), `exp`, `log`, `sqrt`.

Logic operations (booleans) (not, and, or, xor, nand, nor) and tests (`==`, `<`, `>`, `/=`, `<=`, `>=`).
* `5 8 ==` -> `False`
* `True False xor` -> `True`

Unit conversions: some common length, volume, weight, and temperature units are included. rhcalc favors exact conversions using fractions most of the time.
Input: [value] [unit from] [unit to] convert
* `1 mile yd convert` -> `1760`
* `12 inch foot convert` -> `1`
* `1 gal l convert` -> `59148/15625`
* `1 gal l convert num` -> `3.785472`
* `212 dF dC convert` -> `100.0[...]`

Lists: rhcalc handles heterogenous lists and provides basic operations on them.
* `2 3 4 [] : : :` -> `[2,3,4]`
* `[2,3] 4 :` -> `[4,2,3]`
* `4 [2,3] :` -> `[4,2,3]`
* `1 5 range 6 8 range ++` -> `[1,2,3,4,5,6,7,8]`
* `[1,2,3,4,5,6,7,8] 3 drop` -> `[4,5,6,7,8]`
* `[4,5,6,7,8] 2 take` -> `[4,5]`
* `[4,5,6,7,8] init` -> `[4,5,6,7]`
* `[4,5,6,7] tail` -> `[5,6,7]`
* `[5,6,7] reverse` -> `[7,6,5]`

Stack operations
----------------
* `swap` exchanges the two elements at the top of the stack.
```
% 8 str
2: 8
1: str
% swap
2: str
1: 8
```

* `del` removes the top element from the stack.
```
2: str
1: 8
% del
1: str
```

* `dup` duplicates the first element on the stack.
```
1: str
% dup
2: str
1: str
```

* `rep` repeats the second element on the stack by the first. example: 5 8 rep removes 5 and 2 from the stack and puts back eight times 5.
```
% 5
1: 5
% 8 rep
(4 more on the stack)
4: 5
3: 5
2: 5
1: 5
```

* `get` puts the n-th element on the stack at the top.
```
3: 5
2: 2.5
1: 8
% 2 get
3: 5
2: 8
1: 2.5
% 3 get
3: 8
2: 2.5
1: 5
```

* `cls` clears the stack.
```
3: 8
2: 2.5
1: 5
% cls
```

* `type` puts on the stack the type of the top element.
```
1: 5
% type
2: 5
1: Integer
% 2.5 type
4: 5
3: Integer
2: 2.5
1: Real
```

Memory
------
rhcalc can store values and scripts in memory.

`42 answer store` stores `42` in the variable `answer`. When you type `answer` on the input, `42` is pushed on the stack.

Scripts use the RPN notation and are stored in memory as strings.
`"5 +" addfive store` stores the script `5 +` in the variable `addfive`.
Scripts are executed with `run`:
`8 addfive run` -> `13`

To delete a variable or a script from the memory, type its name *between* `"` and `clear`: `"addfive" clear`.

You can look at the state of the memory with `vars`:
`vars` -> `[("addfive","5 +"),("answer",42)]`

The memory is lost when you exit rhcalc.

"

Settings
--------
rhcalc supports a base setting to display integers on the stack in other bases (hexadecimal, binary, octal, or other bases you fancy).

`16 base` sets the base to 16 (hexadecimal), and `10 base` sets it back to decimal.

The `settings` command shows the current settings:
```
% settings
1: Decimal Rad
% 16 base
1: Decimal Rad
% settings
2: Decimal Rad
1: Hex Rad
```

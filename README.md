Data-Checked
============
This package provides a (phantom) type-indexed newtype evidence-wrapper for
values that are checked to satisfy the property associated with the type. 

Installation
------------
The usual:

	$ cabal install

Examples
--------

```
module Evens (Even, isEven, showEven, double, timesEven) where

import Data.Checked

data Even = Even

isEven :: Integral n => Property Even n
isEven = property Even even

showEven :: Show n => Checked' Even n -> String
showEven = show . checked

double :: Integral n => n -> Checked' Even n
double n = trustThat Even (n * 2)

timesEven :: Integral n => n -> Checked' Even n -> Checked' Even n
timesEven a = preserving' Even (*a)
```

A client importing Evens can only construct a Checked Even by using
Data.Check.check or Evens.double, both of which actually guarantee
evenness.

Combinators allow expressing more complex properties:

```
module SmallEvens (Small, small, showSmallEven, showSmallOrEven) where

import Evens import Data.Checked

data Small = Small

small :: (Num n, Ord n) => Property Small n
small = property Small (<100)

showSmallEven :: Show n => Checked' (I Small Even) n -> String
showSmallEven = show . checked

showSmallOrEven :: Show n => Checked' (U Small Even) n -> String
showSmallOrEven = show . checked
```

A client importing SmallEvens can do

```
module Client where

import Evens
import SmallEvens

import Data.Checked

t1 = fmap showSmallOrEven (check (small ~|| isEven) 2) -- Just "2"

t2 = fmap showSmallOrEven (check (small ~|| isEven) 3) -- Just "3"

t3 = fmap showSmallOrEven (check (small ~|| isEven) 100) -- Just "100"

t4 = fmap showSmallOrEven (check (small ~|| isEven) 101) -- Nothing

-- etc.

```

{-#LANGUAGE ViewPatterns #-}
{-| We are all familiar with infixes in mathematical notation, such as @\<a\> + \<b\>@, and we may also
    understand that the notation for factorials is a postfix @\<n\>!@, and we can figure out what
    prefixes would be (I can't think of one right now). We should also remember absolute-value
    notation @|\<n\>|@ which happens to be called a circumfix. There are even weirder operators though,
    like @\<p\> ? \<c\> : \<a\>@, whose patter to my knowledge has no special name.
    Still, there must be some commonality between these operators, right?

    We introduce the concept of a /distributed affix/, or /distfix/ for short, to encompass and generalize
    all of these concepts.
    A distfix is simply an alternating sequence of /parts/ and /slots/; note that a distfix can begin and
    end with either.
    So, the infix addition operator would be a slot, the part @+@ then another slot.
    The factorial operator is a slot, then the part @!@.
    Absolute value notation is the part @|@, then a slot, then the part @|@.
    The ternary conditional is a slot, the part @?@, another slot, then part @:@, then the last slot.
    We can even define uncommon operators like @\<@-slot-@|@-slot-@\>@ for inner product.
    Since we'll be defining plenty of distfixes as examples, let's write the slots of a distfix as underscores,
    like @_+_@, @if_then_else_@ and @\<_|_\>@.

    This module provides algorithms for recognizing distfixes in a list of items.
    We also provide algorithms for handling associativity and precedence when multiple distfixes might
    be involved.
    It's up to you to provide your own re-writing and recursion, however.

    TODO associativity and topology breakdown
    TODO priority
    TODO half-open-X means it is both open on the X and X-associative (if then else (if then else))



    FIXME draft

    The rules for calculating priority are these:
        
        * If both distfixes have the same associativity (left- or right-, but not non-associative),
            the one with the \"most significant\" keyword \"earliest\" has priority:
                for left-associative, most significant means first and earliest means leftmost;
                for right-associative, most significant means last, earliest means rightmost.
            If its a still a tie, then the one with the most keywords has priority.

        * If both distfixes are closed, then they must be non-overlapping, or one must contain the
        other.
            If they do not overlap, then the first has priority (by convention)
            If one nests within the other, the outer has priority.
            If they overlap exactly, then the one with the most keywords has priority.
        
        * If the distfixes are non-overlapping, then the first has priority (by convention).

        * Other pairs of matches have no priority distinction.
-}
module Language.Distfix
    (
    -- * Specifying Distfixes
      Distfix
    , distfix
    , Shape(..)
    -- * Finding Distfixes
    , findDistfix
    , findDistfixes
    -- * Results of Matching
    , Detection(..)
    , Detections(..)
    ) where

import Data.Maybe (catMaybes)
import Data.List (nub)
import Data.Sequence (Seq, viewl, viewr, ViewL(..), ViewR(..), (|>), (<|))
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Control.Applicative

-- |Abstract type of distfix patterns, see 'distfix' for a constructor.
data Distfix a = Distfix Shape [a -> Bool]

{-| Information on both topology and associativity.

    The two properties are merged into one datatype because choice of one limits choice of the
    other. The constructors should make the possibilities clear enough, but the module
    documentation better presents the reasoning involved.
-}
data Shape = 
      Closed -- ^No initial or final slot; never associative.
    | HalfOpenRight -- ^Final slot but no initial slot; always right-associative.
    | HalfOpenLeft -- ^Initial slot but no final slot; always left-associative.
    | OpenRight -- ^Initial and final slots; right-associativity specified.
    | OpenLeft -- ^Initial and final slots; left-associativity specified.
    | OpenNon -- ^Initial and final slots; non-associativity specified.
    deriving (Eq, Show)

{-| Construct a 'Distfix'.

    The list of predicates should recognize the parts of the distfix (in order).
-}
distfix :: Shape -> [a -> Bool] -> Distfix a
distfix shape [] = error "precondition error: a distfix must have at least one part"
distfix shape parts = Distfix shape parts

{-| Aggregates all information concerning the match of a distfix in some text.
-}
data Detection a = Detection {
      detectionShape :: Shape
    , detectionParts :: [a]
    , detectionBefore :: [a]
    , detectionSlots :: [[a]]
    , detectionAfter :: [a]
} deriving (Show)

{-| Collects the highest-priority (lowest precedence) distfixes found in some text.
-}
data Detections a = NoMatch
                   | OneMatch (Detection a)
                   | Ambiguous [Detection a]
    deriving (Show)

-- |Attempt to match one of multiple distfixes in a text.
findDistfixes :: [Distfix a] -> [a] -> Detections a
findDistfixes distfixes text = foldl mergeResults NoMatch allMatches
    where
    allMatches = catMaybes $ flip findDistfix text <$> distfixes

-- |Attempt to match a single distfix within a text.
findDistfix :: Distfix a -> [a] -> Maybe (Detection a)
findDistfix (Distfix shape parts) (Seq.fromList -> text) = case shape of
    HalfOpenRight   -> goR (False, True) Seq.empty Seq.empty parts text
    OpenRight       -> goR (True, True) Seq.empty Seq.empty parts text
    HalfOpenLeft    -> goL (True, False) Seq.empty Seq.empty (reverse parts) text
    OpenLeft        -> goL (True, True) Seq.empty Seq.empty (reverse parts) text
    Closed          -> goL (False, False) Seq.empty Seq.empty (reverse parts) text
    OpenNon -> let l = goL (True, True) Seq.empty Seq.empty (reverse parts) text
                   r = goR (True, True) Seq.empty Seq.empty parts text
               in case (l, r) of
                   (Just a, Just b) -> if sameMatch a b then l else Nothing
                   (Nothing, Nothing) -> Nothing
                   (Just _, Nothing) -> l
                   (Nothing, Just _) -> r

    where
    goR :: (Bool, Bool) -> Seq a -> Seq (Seq a) -> [a -> Bool] -> Seq a -> Maybe (Detection a)
    goR (_, needsAfter) parts slots [] (viewl -> EmptyL) =
        if needsAfter then Nothing else Just $ build shape parts (slots |> Seq.empty)
    goR _ parts slots [] text = Just $ build shape parts (slots |> text)
    goR _ parts slots rest (viewl -> EmptyL) = Nothing
    goR (needsBefore, needsAfter) parts slots (part:rest) text = do
        (before, the, after) <- findPartL needsBefore part text
        goR (True, needsAfter) (parts |> the) (slots |> before) rest after

    goL :: (Bool, Bool) -> Seq a -> Seq (Seq a) -> [a -> Bool] -> Seq a -> Maybe (Detection a)
    goL (needsBefore, _) parts slots [] (viewr -> EmptyR) = 
        if needsBefore then Nothing else Just $ build shape parts (Seq.empty <| slots)
    goL _ parts slots [] text = Just $ build shape parts (text <| slots)
    goL _ parts slots rest (viewl -> EmptyL) = Nothing
    goL (needsBefore, needsAfter) parts slots (part:rest) text = do
        (before, the, after) <- findPartR needsAfter part text
        goL (needsBefore, True) (the <| parts) (after <| slots) rest before

    sameMatch a b = (length <$> detectionSlots a) == (length <$> detectionSlots b)


mergeResults :: Detections a -> Detection a -> Detections a
mergeResults NoMatch new = OneMatch new
mergeResults (OneMatch a) new = case cmpDetections a new of
    LT -> OneMatch new
    GT -> OneMatch a
    EQ -> Ambiguous [a, new]
mergeResults (Ambiguous as) new = case nub (flip cmpDetections new <$> as) of
    [LT] -> OneMatch new
    [GT] -> Ambiguous as
    _ -> Ambiguous (new:as)

cmpDetections :: Detection a -> Detection a -> Ordering
cmpDetections a b = case (detectionShape a, detectionShape b) of
    (OpenLeft,      OpenLeft)      -> decideLeft
    (OpenLeft,      HalfOpenLeft)  -> decideLeft
    (HalfOpenLeft,  OpenLeft)      -> decideLeft
    (HalfOpenLeft,  HalfOpenLeft)  -> decideLeft
    (OpenRight,     OpenRight)     -> decideRight
    (OpenRight,     HalfOpenRight) -> decideRight
    (HalfOpenRight, OpenRight)     -> decideRight
    (HalfOpenRight, HalfOpenRight) -> decideRight
    (Closed,        Closed)        -> completelyBefore `joinCmp` decideClosed
    _                              -> completelyBefore
    where
    completelyBefore = case undefined of
        _ | aEnd <= bStart -> GT
          | bEnd <= aStart -> LT
          | otherwise -> EQ
    decideLeft = (bFirstPart `compare` aFirstPart) `joinCmp` mostParts
    decideRight = (aLastPart `compare` bLastPart) `joinCmp` mostParts
    decideClosed = case undefined of
        _ | aStart < bStart && bEnd <= aEnd -> GT -- a b b a, a b ba
          | bStart < aStart && aEnd <= bEnd -> LT -- b a a b, b a ab
          | aStart == bStart && aEnd < bEnd -> LT -- ba a b
          | aStart == bStart && bEnd < aEnd -> GT -- ab b a
          | aStart == bStart && aEnd == bEnd -> mostParts -- ab ab
          | otherwise -> EQ
    mostParts = length (detectionParts a) `compare` length (detectionParts b)
    (aStart, aFirstPart, aLastPart, aEnd) = detectionInterval a
    (bStart, bFirstPart, bLastPart, bEnd) = detectionInterval b

joinCmp :: Ordering -> Ordering -> Ordering
EQ `joinCmp` b = b
a `joinCmp` _ = a

detectionInterval :: Detection a -> (Int, Int, Int, Int)
detectionInterval x = ( lenBefore
                      , firstPart
                      , lastPart
                      , lenBefore + lenParts + lenSlots)
    where
    (firstPart, lastPart) = case detectionShape x of
        Closed -> (lenBefore, lenBefore + lenParts + lenSlots)
        HalfOpenLeft -> (lenBefore + lenHeadSlot, lenBefore + lenParts + lenSlots)
        HalfOpenRight -> (lenBefore, lenBefore + lenParts + lenInitSlots)
        _ -> (lenBefore + lenHeadSlot, lenBefore + lenParts + lenInitSlots)
    lenSlots = sum $ length <$> detectionSlots x
    lenParts = length $ detectionParts x
    lenHeadSlot = length . head $ detectionSlots x
    lenInitSlots = sum . (length <$>) . init $ detectionSlots x
    lenBefore = length $ detectionBefore x


findPartL :: Bool -> (a -> Bool) -> Seq a -> Maybe (Seq a, a, Seq a)
findPartL True p (viewl -> EmptyL) = Nothing
findPartL True p (viewl -> x :< xs) = case Seq.breakl p xs of
    (_, viewl -> EmptyL) -> Nothing
    (before, viewl -> part :< after) -> Just (x <| before, part, after)
findPartL False p xs = case Seq.breakl p xs of
    (_, viewl -> EmptyL) -> Nothing
    (before, viewl -> part :< after) -> Just (before, part, after)

findPartR :: Bool -> (a -> Bool) -> Seq a -> Maybe (Seq a, a, Seq a)
findPartR True p (viewr -> EmptyR) = Nothing
findPartR True p (viewr -> xs :> x) = case Seq.breakr p xs of
    (_, viewl -> EmptyL) -> Nothing
    (after, viewr -> before :> part) -> Just (before, part, after |> x)
findPartR False p xs = case Seq.breakr p xs of
    (viewr -> EmptyR, _) -> Nothing
    (after, viewr -> before :> part) -> Just (before, part, after)


build :: Shape -> Seq a -> Seq (Seq a) -> Detection a
build shape (toList -> parts) ((toList <$>) -> slots) = case shape of
    Closed -> Detection {
          detectionShape = shape
        , detectionParts = parts
        , detectionBefore = seqHead slots
        , detectionSlots = toList $ (seqInit . seqTail) slots
        , detectionAfter = seqLast slots
        }
    HalfOpenRight -> Detection {
          detectionShape = shape
        , detectionParts = parts
        , detectionBefore = toList $ seqHead slots
        , detectionSlots = toList $ seqTail slots
        , detectionAfter = []
        }
    HalfOpenLeft -> Detection {
          detectionShape = shape
        , detectionParts = parts
        , detectionBefore = []
        , detectionSlots = toList $ seqInit slots
        , detectionAfter = toList $ seqLast slots
        }
    _ -> Detection {
          detectionShape = shape
        , detectionParts = parts
        , detectionBefore = []
        , detectionSlots = toList slots
        , detectionAfter = []
        }


seqHead = flip Seq.index 0
seqTail = Seq.drop 1
seqInit xs = Seq.take (Seq.length xs - 1) xs
seqLast xs = Seq.index xs (Seq.length xs - 1)
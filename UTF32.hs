{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module UTF32
  ( UTF32
  -- * Creation and elimination
  , char
  , toString
  , empty
  -- * Basic interface
  , cons
  , snoc
  , append
  , uncons
  , head
  , last
  , tail
  , init
  , null
  , length
  -- * Transformations
  , map
  , reverse
  -- * Folds
  , foldl
  , foldl1
  , foldl'
  , foldl1'
  , foldr
  , foldr1
  -- ** Special folds
  , concat
  , concatMap
  , any
  , all
  , maximum
  , minimum
  -- * Construction
  -- ** Scans
  , scanl
  , scanl1
  , scanr
  , scanr1
  -- ** Generation and unfolding
  , unfoldr
  , unfoldrN
  , unlines
  , unwords
  -- * Substrings
  -- ** Breaking strings
  , take
  , drop
  , takeWhile
  , dropWhile
  , splitAt
  , span
  , break
  , stripPrefix
  , stripSuffix
  , lines
  , words
  -- * Searching
  , filter
  , find
  , partition
  -- * Indexing
  , index
  , findIndex
  -- * Zipping
  , zipWith
  -- * Foreign
  , withCString
  , withCStringLen
  , withEncodedCString
  ) where

import Control.Applicative hiding (Alternative(..))
import Control.Exception (bracket)
import Control.Monad (Monad(..), when)
import Data.Bool (Bool(..), (&&), otherwise)
import Data.Char (Char, isSpace)
import Data.Eq (Eq(..))
import Data.Function ((.), ($), flip)
import qualified Data.Foldable as Foldable
import Data.Int (Int)
import Data.Maybe (Maybe(..))
import Data.Ord ((>))
import Data.Semigroup (Monoid(..), Semigroup(..))
import Data.String (IsString(..), String)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import qualified Data.Vector.Storable.Mutable as MVector
import Foreign.C.String (CString, CStringLen)
import Foreign.ForeignPtr (castForeignPtr)
import GHC.Exts (IsList(..))
import GHC.IO.Buffer
       ( Buffer, BufferState(..), bufferAdd, bufferAvailable, bufferElems
       , bufR, emptyBuffer, isEmptyBuffer )
import GHC.IO.Encoding (getForeignEncoding)
import GHC.IO.Encoding.Types
       ( BufferCodec(..), CodingProgress(..), TextEncoding(..)
       , TextEncoder )
import Prelude (Num(..))
import System.IO (IO)
import Text.Parsec (Stream)
import qualified Text.Parsec as Parsec

newtype UTF32 = UTF32 { unUTF32 :: Vector Char }
  deriving Eq

instance IsList UTF32 where
  type Item UTF32 = Char
  fromList chars = UTF32 (Vector.fromList chars)
  fromListN len chars = UTF32 (Vector.fromListN len chars)
  toList (UTF32 str) = Vector.toList str

instance IsString UTF32 where
  fromString str = UTF32 (Vector.fromList str)

instance Semigroup UTF32 where
  (<>) (UTF32 stra) (UTF32 strb) = UTF32 ((Vector.++) stra strb)

instance Monoid UTF32 where
  mempty = UTF32 Vector.empty
  mappend = (<>)

instance Monad m => Stream UTF32 m Char where
  uncons str = pure (uncons str)

-- * Creation and elimination

char :: Char -> UTF32
char c = UTF32 (Vector.singleton c)

toString :: UTF32 -> String
toString (UTF32 str) = Vector.toList str

empty :: UTF32
empty = mempty

-- * Basic interface

cons :: Char -> UTF32 -> UTF32
cons c (UTF32 str) = UTF32 (Vector.cons c str)

snoc :: UTF32 -> Char -> UTF32
snoc (UTF32 str) c = UTF32 (Vector.snoc str c)

append :: UTF32 -> UTF32 -> UTF32
append = (<>)

uncons :: UTF32 -> Maybe (Char, UTF32)
uncons (UTF32 str)
  | Vector.null str = Nothing
  | otherwise = Just (Vector.head str, UTF32 (Vector.tail str))

head :: UTF32 -> Char
head (UTF32 str) = Vector.head str

last :: UTF32 -> Char
last (UTF32 str) = Vector.last str

tail :: UTF32 -> UTF32
tail (UTF32 str) = UTF32 (Vector.tail str)

init :: UTF32 -> UTF32
init (UTF32 str) = UTF32 (Vector.init str)

null :: UTF32 -> Bool
null (UTF32 str) = Vector.null str

length :: UTF32 -> Int
length (UTF32 str) = Vector.length str

-- * Transformations

map :: (Char -> Char) -> UTF32 -> UTF32
map f (UTF32 str) = UTF32 (Vector.map f str)

reverse :: UTF32 -> UTF32
reverse (UTF32 str) = UTF32 (Vector.reverse str)

-- * Folds

foldl :: (a -> Char -> a) -> a -> UTF32 -> a
foldl f z (UTF32 str) = Vector.foldl f z str

foldl' :: (a -> Char -> a) -> a -> UTF32 -> a
foldl' f z (UTF32 str) = Vector.foldl' f z str

foldl1 :: (Char -> Char -> Char) -> UTF32 -> Char
foldl1 f (UTF32 str) = Vector.foldl1 f str

foldl1' :: (Char -> Char -> Char) -> UTF32 -> Char
foldl1' f (UTF32 str) = Vector.foldl1' f str

foldr :: (Char -> a -> a) -> a -> UTF32 -> a
foldr f z (UTF32 str) = Vector.foldr f z str

foldr1 :: (Char -> Char -> Char) -> UTF32 -> Char
foldr1 f (UTF32 str) = Vector.foldr1 f str

-- ** Special folds

concat :: [UTF32] -> UTF32
concat strs = UTF32 (Vector.concat (unUTF32 <$> strs))

concatMap :: (Char -> UTF32) -> UTF32 -> UTF32
concatMap f (UTF32 str) = UTF32 (Vector.concatMap (unUTF32 . f) str)

any :: (Char -> Bool) -> UTF32 -> Bool
any f (UTF32 str) = Vector.any f str

all :: (Char -> Bool) -> UTF32 -> Bool
all f (UTF32 str) = Vector.all f str

maximum :: UTF32 -> Char
maximum (UTF32 str) = Vector.maximum str

minimum :: UTF32 -> Char
minimum (UTF32 str) = Vector.minimum str

-- * Construction

-- ** Scans

scanl :: (Char -> Char -> Char) -> Char -> UTF32 -> UTF32
scanl f z (UTF32 str) = UTF32 (Vector.scanl f z str)

scanl1 :: (Char -> Char -> Char) -> UTF32 -> UTF32
scanl1 f (UTF32 str) = UTF32 (Vector.scanl1 f str)

scanr :: (Char -> Char -> Char) -> Char -> UTF32 -> UTF32
scanr f z (UTF32 str) = UTF32 (Vector.scanr f z str)

scanr1 :: (Char -> Char -> Char) -> UTF32 -> UTF32
scanr1 f (UTF32 str) = UTF32 (Vector.scanr1 f str)

-- ** Generation and unfolding

unfoldr :: (a -> Maybe (Char, a)) -> a -> UTF32
unfoldr f z = UTF32 (Vector.unfoldr f z)

unfoldrN :: Int -> (a -> Maybe (Char, a)) -> a -> UTF32
unfoldrN n f z = UTF32 (Vector.unfoldrN n f z)

unlines :: [UTF32] -> UTF32
unlines [] = empty
unlines ls = concat (flip snoc '\n' <$> ls)

unwords :: [UTF32] -> UTF32
unwords [] = empty
unwords ws = Foldable.foldr1 (\wd rest -> wd <> cons ' ' rest) ws

-- * Substrings

-- ** Breaking strings

take :: Int -> UTF32 -> UTF32
take n (UTF32 str) = UTF32 (Vector.take n str)

drop :: Int -> UTF32 -> UTF32
drop n (UTF32 str) = UTF32 (Vector.drop n str)

takeWhile :: (Char -> Bool) -> UTF32 -> UTF32
takeWhile f (UTF32 str) = UTF32 (Vector.takeWhile f str)

dropWhile :: (Char -> Bool) -> UTF32 -> UTF32
dropWhile f (UTF32 str) = UTF32 (Vector.dropWhile f str)

splitAt :: Int -> UTF32 -> (UTF32, UTF32)
splitAt n (UTF32 str) =
  let (pref, suff) = Vector.splitAt n str in (UTF32 pref, UTF32 suff)

span :: (Char -> Bool) -> UTF32 -> (UTF32, UTF32)
span test (UTF32 str) =
  let (pref, suff) = Vector.span test str in (UTF32 pref, UTF32 suff)

break :: (Char -> Bool) -> UTF32 -> (UTF32, UTF32)
break test (UTF32 str) =
  let (pref, suff) = Vector.break test str in (UTF32 pref, UTF32 suff)

-- | Return the suffix of the second string if its prefix matches the entire first string.
stripPrefix :: UTF32 -> UTF32 -> Maybe UTF32
stripPrefix (UTF32 tgt) (UTF32 str)
  | len > Vector.length str = Nothing
  | otherwise =
      let
        (pref, suff) = Vector.splitAt len str
      in
        if pref == tgt then Just (UTF32 suff) else Nothing
  where
    len = Vector.length tgt

stripSuffix :: UTF32 -> UTF32 -> Maybe UTF32
stripSuffix (UTF32 tgt) (UTF32 str)
  | len_tgt > len_str = Nothing
  | otherwise =
      let
        (pref, suff) = Vector.splitAt (len_str - len_tgt) str
      in
        if suff == tgt then Just (UTF32 pref) else Nothing
  where
    len_tgt = Vector.length tgt
    len_str = Vector.length str

lines :: UTF32 -> [UTF32]
lines str
  | null str = []
  | otherwise =
      case break (== '\n') str of
        (l, r)
          | null r -> [l]
          | otherwise -> l : lines (tail r)

words :: UTF32 -> [UTF32]
words str =
  case break isSpace (dropWhile isSpace str) of
    (l, r)
      | null l -> []
      | null r -> [l]
      | otherwise -> l : words (tail r)

-- * Searching

filter :: (Char -> Bool) -> UTF32 -> UTF32
filter f (UTF32 str) = UTF32 (Vector.filter f str)

find :: (Char -> Bool) -> UTF32 -> Maybe Char
find f (UTF32 str) = Vector.find f str

partition :: (Char -> Bool) -> UTF32 -> (UTF32, UTF32)
partition f (UTF32 str) =
  let (satis, unsat) = Vector.partition f str
  in (UTF32 satis, UTF32 unsat)

-- * Indexing

index :: UTF32 -> Int -> Char
index (UTF32 str) n = (Vector.!) str n

findIndex :: (Char -> Bool) -> UTF32 -> Maybe Int
findIndex f (UTF32 str) = Vector.findIndex f str

-- * Zipping

zipWith :: (Char -> Char -> Char) -> UTF32 -> UTF32 -> UTF32
zipWith f (UTF32 stra) (UTF32 strb) = UTF32 (Vector.zipWith f stra strb)

-- * Foreign

withCString :: UTF32 -> (CString -> IO a) -> IO a
withCString str worker = do
  enc <- getForeignEncoding
  withEncodedCString enc True str $ \(p, _) -> worker p

withCStringLen :: UTF32 -> (CStringLen -> IO a) -> IO a
withCStringLen str worker = do
  enc <- getForeignEncoding
  withEncodedCString enc False str worker

withEncodedCString
  :: TextEncoding  -- ^ Encoding of CString to create
  -> Bool  -- ^ Null terminate?
  -> UTF32  -- ^ String to encode
  -> (CStringLen -> IO a)  -- ^ Worker that can safely use the allocated memory
  -> IO a
withEncodedCString (TextEncoding {..}) nullTerminate str worker =
  bracket mkTextEncoder close $ \encoder -> do
    let
      (fp, sz) = Vector.unsafeToForeignPtr0 (unUTF32 str)
      from = bufferAdd sz (emptyBuffer fp sz ReadBuffer)
    fillBufferAndCall encoder nullTerminate from worker

fillBufferAndCall
  :: TextEncoder dstate
  -> Bool
  -> Buffer Char
  -> (CStringLen -> IO a)
  -> IO a
fillBufferAndCall encoder nullTerminate from worker = do
  let len = bufferElems from
  dst <- MVector.new len
  go (from, dst) (from, toBuffer dst)
  where
    toBuffer v =
      let (p, len) = MVector.unsafeToForeignPtr0 v in
        emptyBuffer (castForeignPtr p) len WriteBuffer

    go (src, dst) (_from, _to) = do
      (why, _from, _to) <- encode encoder _from _to
      if isEmptyBuffer _from
        then do
          dst' <- if nullTerminate && bufferAvailable _to == 0
                  then MVector.grow dst 1
                  else pure dst
          when nullTerminate $ MVector.write dst' (bufR _to) 0
          MVector.unsafeWith dst' $ \p ->
            worker (p, bufferElems _to)
        else
          case why of
            -- These conditions are equally bad
            -- since the input was truncated/invalid
            InputUnderflow  -> recover encoder _from _to >>= go (src, dst)
            InvalidSequence -> recover encoder _from _to >>= go (src, dst)
            OutputUnderflow -> do
              dst' <- MVector.grow dst (MVector.length dst)
              go (src, dst') (src, toBuffer dst')

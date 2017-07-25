{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.UTF32 where

import Control.Monad.IO.Class (liftIO)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Foreign.C.String
       ( withCString, withCStringLen, withCWString, withCWStringLen )
import GHC.Exts (IsList(..))
import qualified Language.C.Inline as C
import UTF32 (UTF32)
import qualified UTF32

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

C.include "<string.h>"
C.include "<wchar.h>"

fromText :: Text -> UTF32
fromText txt = fromListN (Text.length txt) (Text.unpack txt)

toText :: UTF32 -> Text
toText utf = fromListN (UTF32.length utf) (UTF32.toString utf)

equiv :: UTF32 -> Text -> PropertyT IO ()
equiv utf txt = do
  assert $ toText utf == txt
  assert $ utf == fromText txt

genShortString :: Gen String
genShortString = Gen.string (Range.linear 0 2048) Gen.unicode

genLongString :: Gen String
genLongString = Gen.string (Range.linear 0 4096) Gen.unicode

genLongString1 :: Gen String
genLongString1 = Gen.string (Range.linear 1 4096) Gen.unicode

genStrings :: Gen [String]
genStrings = Gen.list (Range.linear 0 16) genShortString

genStrings1 :: Gen [String]
genStrings1 = Gen.list (Range.linear 1 16) genShortString

prop_char :: Property
prop_char = property $ do
  c <- forAll $ Gen.unicode
  let utf = UTF32.char c
      txt = Text.singleton c
  equiv utf txt

prop_toString :: Property
prop_toString = property $ do
  str <- forAll $ Gen.string (Range.linear 0 4096) Gen.unicode
  let utf = fromString str
      txt = fromString str
  assert $ UTF32.toString utf == str
  equiv utf txt

prop_empty :: Property
prop_empty = property $ equiv UTF32.empty Text.empty

prop_cons :: Property
prop_cons = property $ do
  c <- forAll $ Gen.unicode
  str <- forAll genLongString
  let utf = UTF32.cons c (fromString str)
      txt = Text.cons c (fromString str)
  equiv utf txt

prop_snoc :: Property
prop_snoc = property $ do
  c <- forAll $ Gen.unicode
  str <- forAll genLongString
  let utf = UTF32.snoc (fromString str) c
      txt = Text.snoc (fromString str) c
  equiv utf txt

prop_append :: Property
prop_append = property $ do
  stra <- forAll genShortString
  strb <- forAll genShortString
  let utf = UTF32.append (fromString stra) (fromString strb)
      txt = Text.append (fromString stra) (fromString strb)
  equiv utf txt

prop_uncons :: Property
prop_uncons = property $ do
  str <- forAll genLongString1
  case UTF32.uncons (fromString str) of
    Nothing -> assert $ Text.uncons (fromString str) == Nothing
    Just (u, utfs) -> do
      let Just (t, txts) = Text.uncons (fromString str)
      assert $ u == t
      equiv utfs txts

prop_head :: Property
prop_head = property $ do
  str <- forAll genLongString1
  let u = UTF32.head (fromString str)
      t = Text.head (fromString str)
  assert $ u == t

prop_last :: Property
prop_last = property $ do
  str <- forAll genLongString1
  let u = UTF32.last (fromString str)
      t = Text.last (fromString str)
  assert $ u == t

prop_tail :: Property
prop_tail = property $ do
  str <- forAll genLongString1
  let utf = UTF32.tail (fromString str)
      txt = Text.tail (fromString str)
  equiv utf txt

prop_init :: Property
prop_init = property $ do
  str <- forAll genLongString1
  let utf = UTF32.init (fromString str)
      txt = Text.init (fromString str)
  equiv utf txt

prop_null :: Property
prop_null = property $ do
  str <- forAll genLongString
  assert $ UTF32.null (fromString str) == Text.null (fromString str)

prop_length :: Property
prop_length = property $ do
  str <- forAll genLongString
  assert $ UTF32.length (fromString str) == Text.length (fromString str)

prop_concat :: Property
prop_concat = property $ do
  strs <- forAll genStrings
  let utf = UTF32.concat (fromString <$> strs)
      txt = Text.concat (fromString <$> strs)
  equiv utf txt

prop_maximum :: Property
prop_maximum = property $ do
  str <- forAll genLongString1
  let u = UTF32.maximum (fromString str)
      t = UTF32.maximum (fromString str)
  assert $ u == t

prop_minimum :: Property
prop_minimum = property $ do
  str <- forAll genLongString1
  let u = UTF32.minimum (fromString str)
      t = UTF32.minimum (fromString str)
  assert $ u == t

prop_unlines :: Property
prop_unlines = property $ do
  strs <- forAll genStrings
  let utfs = fromString <$> strs
      txts = fromString <$> strs
      utf = UTF32.unlines utfs
      txt = Text.unlines txts
  equiv utf txt

prop_unwords :: Property
prop_unwords = property $ do
  strs <- forAll genStrings
  let utfs = fromString <$> strs
      txts = fromString <$> strs
      utf = UTF32.unwords utfs
      txt = Text.unwords txts
  equiv utf txt

prop_stripPrefix :: Property
prop_stripPrefix = property $ do
  strs <- forAll genStrings1
  let utfs@(u : _) = fromString <$> strs
      txts@(t : _) = fromString <$> strs
      utf = UTF32.stripPrefix u (UTF32.concat utfs)
      txt = Text.stripPrefix t (Text.concat txts)
  assert $ (toText <$> utf) == txt
  assert $ utf == (fromText <$> txt)

prop_stripSuffix :: Property
prop_stripSuffix = property $ do
  strs <- forAll genStrings1
  let utfs@(u : _) = fromString <$> strs
      txts@(t : _) = fromString <$> strs
      utf = UTF32.stripSuffix u (UTF32.concat (reverse utfs))
      txt = Text.stripSuffix t (Text.concat (reverse txts))
  assert $ (toText <$> utf) == txt
  assert $ utf == (fromText <$> txt)

prop_withCString :: Property
prop_withCString = property $ do
  str <- forAll genLongString
  cmp <-
    liftIO $ withCString str
    $ \pstr -> UTF32.withCString (fromString str)
    $ \putf ->
        [C.exp|
          int { strcmp( $(char* pstr), $(char* putf) ) }
        |]
  assert $ cmp == 0

prop_withCStringLen :: Property
prop_withCStringLen = property $ do
  str <- forAll genLongString
  cmp <-
    liftIO $ withCStringLen str
    $ \(pstr, slen) -> UTF32.withCStringLen (fromString str)
    $ \(putf, ulen) -> do
        let len = toEnum (min slen ulen)
        [C.exp|
          int { strncmp( $(char* pstr), $(char* putf), $(int len) ) }
          |]
  assert $ cmp == 0

prop_withCWString :: Property
prop_withCWString = property $ do
  str <- forAll genLongString
  cmp <-
    liftIO $ withCWString str
    $ \pstr -> UTF32.withCWString (fromString str)
    $ \putf ->
        [C.exp|
          int { wcscmp( $(wchar_t* pstr), $(wchar_t* putf) ) }
        |]
  assert $ cmp == 0

prop_withCWStringLen :: Property
prop_withCWStringLen = property $ do
  str <- forAll genLongString
  cmp <-
    liftIO $ withCWStringLen str
    $ \(pstr, slen) -> UTF32.withCWStringLen (fromString str)
    $ \(putf, ulen) -> do
        let len = toEnum (min slen ulen)
        [C.exp|
          int { wcsncmp( $(wchar_t* pstr), $(wchar_t* putf), $(int len) ) }
          |]
  assert $ cmp == 0

tests :: IO Bool
tests = checkSequential $$(discover)

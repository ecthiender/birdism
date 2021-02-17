-- | Exports the most common functions, data types used from across base. They are annoying to
-- import everytime and they ubiquitous in Haskell code.
module Birdism.Common
  ( module M
  , debugTrace
  , tshow
  , sleep
  , txtToBS
  , txtToLBS
  )
  where

import           Control.Applicative         as M (Alternative (..))
import           Control.Arrow               as M (first, second, (&&&), (***), (<<<), (>>>))
-- import           Control.Monad.Base                as M
import           Control.Monad.Except        as M
import           Control.Monad.Identity      as M
import           Control.Monad.Reader        as M
import           Control.Monad.State.Strict  as M
import           Control.Monad.Writer.Strict as M (MonadWriter (..), WriterT (..), execWriterT,
                                                   runWriterT)
import           Data.Bool                   as M (bool)
import           Data.Data                   as M (Data (..))
import           Data.Either                 as M (lefts, partitionEithers, rights)
import           Data.Foldable               as M (asum, foldrM, for_, toList, traverse_)
import           Data.Function               as M (on, (&))
import           Data.Functor                as M (($>), (<&>))
import           Data.Hashable               as M (Hashable)
import           Data.HashMap.Strict         as M (HashMap)
import           Data.HashSet                as M (HashSet)
import           Data.List                   as M (find, findIndex, foldl', group, intercalate,
                                                   intersect, lookup, sort, sortBy, sortOn, union,
                                                   unionBy, (\\))
-- import           Data.List.NonEmpty                as M (NonEmpty (..))
import           Data.Maybe                  as M (catMaybes, fromMaybe, isJust, isNothing,
                                                   listToMaybe, mapMaybe, maybeToList)
-- import           Data.Monoid                       as M (getAlt)
import           Data.Ord                    as M (comparing)
import           Data.Semigroup              as M (Semigroup (..))
-- import           Data.Sequence                     as M (Seq)
import           Data.String                 as M (IsString)
import           Data.Text                   as M (Text)
import           Data.Traversable            as M (for)
import           Data.Word                   as M (Word64)
import           GHC.Generics                as M (Generic)
import           Prelude                     as M hiding (fail, init, lookup)
import           Text.Read                   as M (readEither, readMaybe)

import qualified Control.Concurrent          as Conc
import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy        as BL
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as TE

debugTrace :: (MonadIO m, Show a) => Maybe String -> a -> m ()
debugTrace prefix actual =
  liftIO $ putStrLn $ "[DEBUG] " <> maybe "" (<> ": ") prefix <> show actual

tshow :: Show a => a -> Text
tshow = T.pack . show

sleep :: MonadIO m => Int -> m ()
sleep seconds = liftIO $ Conc.threadDelay (1000 * 1000 * seconds)

txtToBS :: Text -> B.ByteString
txtToBS = TE.encodeUtf8

txtToLBS :: Text -> BL.ByteString
txtToLBS = BL.fromStrict . TE.encodeUtf8

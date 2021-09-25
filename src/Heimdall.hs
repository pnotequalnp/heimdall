module Heimdall where

import Control.Monad ((>=>))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Store qualified as S
import Data.Text (Text)
import GHC.Generics (Generic)

data Entry = Entry
  { _login :: Text,
    _password :: Text,
    _urls :: [Text],
    _extraFields :: Map Text Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (S.Store)

data Namespace = Leaf Entry | Namespace (Map Text Namespace)
  deriving stock (Generic, Show)
  deriving anyclass (S.Store)

type Path = [Text]

findNamespace :: Namespace -> Path -> Maybe Namespace
findNamespace ns = \case
  [] -> Just ns
  p : ps -> case ns of
    Leaf _ -> Nothing
    Namespace m -> M.lookup p m >>= flip findNamespace ps

findEntry :: Namespace -> Path -> Maybe Entry
findEntry ns =
  findNamespace ns >=> \case
    Leaf e -> Just e
    Namespace _ -> Nothing

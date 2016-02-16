module Types where

import Data.Text (Text)
import Network.IMAP.Types (IMAPConnection)
import Control.Concurrent.STM.TVar (TVar)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (Connection)
import Data.DeriveTH

type ErrorMessage = String

-- |Selected state specifies the selected inbox name
data ConnState = Selected !Text | Authenticated
  deriving (Show, Eq)

data ConnResource = ConnResource {
  imapConnection :: IMAPConnection,
  imapConnState :: TVar ConnState,
  dbConnection :: Connection
}

data MessageJob = MessageDelete [UUID]
                | MessageFetch !Text !Integer
                | GroupedFetch !Text [Integer]
                deriving (Show, Eq)

data UpdateType = Delete | Update | Creation
  deriving (Show)

data ResourceType = Message
  deriving (Show)

data ProgressNotification = ProgressError {
    resourceType :: !ResourceType,
    errorMessage :: !Text
  }
  | ProgressNotification {
    resourceId :: !UUID,
    resourceType :: !ResourceType,
    updateType :: !UpdateType
  }
  deriving (Show)

$(derive makeIs ''MessageJob)

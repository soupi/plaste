{- | Module describing the database interaction

For functions that return `Transaction <result>`:

- If it only reads use `runReadTransaction` or `readQuery` in an Action context
- If it writes as well use `runWriteTransaction` or `writeQuery` in an Action context

Queries and Insert/Upsert queries are the interesting ones.

-}

module Web.Plaste.Database where

import Web.Plaste.Types

import Web.Spock.Config
import qualified Web.Spock as S
import qualified Web.Spock.Config as SC
import qualified Hasql.Connection as Sql
import qualified Hasql.Query as Sql
import qualified Hasql.Session as Sql hiding (query)
import qualified Hasql.Decoders as SqlD
import qualified Hasql.Encoders as SqlE
import qualified Hasql.Transaction as Sql
import qualified Hasql.Transaction.Sessions as Sql

import Data.Int
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T


-- | Run a transaction that only reads data from the database
--   in the context of a spock action
readQuery :: Sql.Transaction a -> Action v (Either Sql.Error a)
readQuery = S.runQuery . Sql.run . runReadTransaction

-- | Run a transaction that can read and write data from/to the database
--   in the context of a spock action
writeQuery :: Sql.Transaction a -> Action v (Either Sql.Error a)
writeQuery = S.runQuery . Sql.run . runWriteTransaction

-- | Run a transaction that only reads data from the database
runReadTransaction :: Sql.Transaction a -> Sql.Session a
runReadTransaction = Sql.transaction Sql.Serializable Sql.Read

-- | Run a transaction that can read and write data from/to the database
runWriteTransaction :: Sql.Transaction a -> Sql.Session a
runWriteTransaction = Sql.transaction Sql.Serializable Sql.Write

-----------
-- Query --
-----------


getPlasteById :: Int64 -> Sql.Transaction (Maybe T.Text)
getPlasteById uid = Sql.query uid $
  Sql.statement
    "select plaste_text from plastes where plaste_id = $1"
    (SqlE.value SqlE.int8)
    (SqlD.maybeRow $ SqlD.value SqlD.text)
    True

getLatest :: Sql.Transaction (Maybe Int64)
getLatest = Sql.query () $
  Sql.statement
    "select plaste_id from plastes order by plaste_id desc limit 1"
    (SqlE.unit)
    (SqlD.maybeRow $ SqlD.value SqlD.int8)
    False

------------
-- Insert --
------------


newPlaste :: T.Text -> Sql.Transaction (Either T.Text Int64)
newPlaste txt = do
  Sql.query txt $
    Sql.statement
      "insert into plastes (plaste_text) values ( $1 )"
      (SqlE.value SqlE.text)
      SqlD.unit
      True

  r <- getLatest
  case r of
    Nothing ->
      pure $ Left "Internal error: Could not find plaste after insert."
    Just pid -> do
      Sql.query pid $
        Sql.statement
          "delete from plastes where plaste_id < $1 - 10000"
          (SqlE.value SqlE.int8)
          SqlD.unit
          True
      pure $ pure pid

---------------------
-- Connection pool --
---------------------

-- | A connection pool for hasql
--   The numbers are fairly random. Sorry about that.
hasqlPool :: Sql.Settings -> SC.ConnBuilder Sql.Connection
hasqlPool connstr = SC.ConnBuilder
  { cb_createConn = either (error . show . fmap BSC.unpack) id <$> Sql.acquire connstr
  , cb_destroyConn = Sql.release
  , cb_poolConfiguration = PoolCfg 10 1000 30
  }

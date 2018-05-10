{- | Defining a few types for the application


-}

module Web.Plaste.Types
  ( module Web.Plaste.Types
  )
where


import Web.Spock
import Hasql.Connection (Connection)

-- | Our session is empty
data MySession
  = EmptySession
  deriving (Show, Eq, Ord)

-- | The state is the app configuration and won't change
data AppState = EmptyState
  deriving (Eq)

-- | Convenient context type for Spock
type App ctx = SpockCtxM ctx Connection MySession AppState ()

-- | Convenient action type for Spock
type Action ctx a = SpockActionCtx ctx Connection MySession AppState a

module Network.Matrix.Bot.JSON ( aesonOptions ) where

import           Data.Aeson        ( Options, omitNothingFields )
import           Data.Aeson.Casing ( aesonPrefix, snakeCase )

aesonOptions :: Options
aesonOptions = (aesonPrefix snakeCase) { omitNothingFields = True }

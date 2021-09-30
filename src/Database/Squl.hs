module Database.Squl
    ( Select(..)
    , Insert(..)
    , Update(..)
    , Delete(..)

    -- Types
    , Table(..)
    , Order(..)
    , Group(..)
    , AggFunc(..)
    , Returning(..)
    , QueryString(..)
    , Field(..)
    , SelectTable(..)
    , ConflictTarget(..)

    -- DSL query types
    , select
    , insert
    , update
    , delete

    -- query modificators
    , fields
    , distinct
    , where_
    , inner
    , left
    , right
    , leftOuter
    , rightOuter
    , join
    , on
    , group
    , subQuery
    , order
    , limit
    , offset
    , onConflict
    , any
    , for
    , always
    , doNothing
    , returning
    , noTable

    -- condition
    , withQuery
    , ($&)
    , ($|)

    -- fields constructors
    , SelectFields(..)
    , InsertFields(..)
    , UpdateFields(..)
    , Assign(..)
    , Value(..)
    , query
    , agg
    , raw
    , set
    , allAgg
    , all
    , from

    -- reexport
    , NonEmpty(..)
    , (<|)
    , (&)
    ) where

import Database.Squl.Types
import Data.List.NonEmpty  (NonEmpty(..), (<|))
import Data.Function       ((&))
import Prelude      hiding (all, any)

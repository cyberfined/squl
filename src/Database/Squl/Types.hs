{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Database.Squl.Types
    ( Select(..)
    , Insert(..)
    , Update(..)
    , Delete(..)
    , Distinct(..)
    , JoinTable(..)
    , JoinType(..)
    , Join(..)
    , On(..)
    , SubQuery(..)
    , Condition(..)
    , SelectFields(..)
    , InsertFields(..)
    , UpdateFields(..)
    , ConflictTarget(..)
    , ConflictAction(..)
    , ConflictCondition(..)
    , SelectTable(..)

    --  ByteString newtypes
    , Table(..)
    , Order(..)
    , Group(..)
    , AggFunc(..)
    , OnConflict(..)
    , Returning(..)
    , QueryString(..)
    , Field(..)

    -- Int newtypes
    , Limit(..)
    , Offset(..)

    -- DSL query types
    , select
    , insert
    , update
    , delete

    -- query modificators
    , noTable
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
    , subQuery
    , group
    , order
    , limit
    , offset
    , onConflict
    , any
    , for
    , always
    , doNothing
    , returning

    -- condition
    , withQuery
    , ($&)
    , ($|)

    -- fields constructors
    , Assign(..)
    , Value(..)
    , query
    , agg
    , raw
    , set
    , all
    , allAgg
    , from
    ) where

import Data.ByteString.Lazy (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.String          (IsString(..))
import Prelude       hiding (Either(..), all, any)

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.List.NonEmpty         as NonEmpty

newtype Table = Table { unTable :: ByteString } deriving IsString

data SelectTable
    = SelectTable !ByteString
    | NoTable

instance IsString SelectTable where
    fromString = SelectTable . BS.pack

noTable :: SelectTable
noTable = NoTable

data Select = Select
    { selectTable      :: !SelectTable
    , selectFields     :: !(NonEmpty SelectFields)
    , selectDistinct   :: !Distinct
    , selectCondition  :: !(Maybe Condition)
    , selectJoin       :: ![JoinTable]
    , selectGroup      :: ![Group]
    , selectSubQueries :: ![SubQuery]
    , selectOrder      :: ![Order]
    , selectLimit      :: !(Maybe Limit)
    , selectOffset     :: !(Maybe Offset)
    }

data Insert = Insert
    { insertTable      :: !Table
    , insertFields     :: !InsertFields
    , insertOnConflict :: !(Maybe OnConflict)
    , insertReturning  :: !(Maybe Returning)
    }

data Update = Update
    { updateTable     :: !Table
    , updateFields    :: !UpdateFields
    , updateCondition :: !(Maybe Condition)
    , updateJoin      :: ![JoinTable]
    , updateReturning :: !(Maybe Returning)
    }

data Delete = Delete
    { deleteTable     :: !Table
    , deleteCondition :: !(Maybe Condition)
    , deleteJoin      :: ![JoinTable]
    , deleteReturning :: !(Maybe Returning)
    }

data JoinTable = JoinTable
    { joinType      :: !JoinType
    , joinTable     :: !Table
    , joinCondition :: !Condition
    }

data Assign = !Field := !Value

data Value 
    = Query !Select
    | Expr !ByteString

query :: Select -> Value
query = Query

instance IsString Value where
    fromString = Expr . BS.pack

newtype Field = Field { unField :: ByteString } deriving IsString

data InsertFields
    = InsertFields !(NonEmpty Assign)
    | FieldsFrom !(NonEmpty Field) !Select

newtype UpdateFields = UpdateFields { unUpdateFields :: NonEmpty Assign }

newtype AggFunc = AggFunc ByteString deriving IsString

data SelectFields
    = SelectFields !(Maybe AggFunc) !(NonEmpty ByteString)
    | SelectAll !(Maybe AggFunc)

data Distinct = Indistinct | Distinct

newtype Group = Group ByteString deriving IsString

data SubQuery = SubQuery
    { combination :: !QueryString
    , getSubQuery :: !Select
    }

newtype Order = Order ByteString deriving IsString

newtype Limit = Limit { unLimit :: Int }

newtype Offset = Offset { unOffset :: Int }

data OnConflict = OnConflict
    { conflictTarget    :: !ConflictTarget
    , conflictAction    :: !ConflictAction
    , conflictCondition :: !ConflictCondition
    }

data ConflictTarget
    = ConflictTarget !ByteString
    | AnyTarget

instance IsString ConflictTarget where
    fromString = ConflictTarget . BS.pack

data ConflictAction
    = DoNothing
    | Perform !(NonEmpty Assign)

data ConflictCondition
    = Always
    | ConflictCondition !Condition

newtype Returning = Returning { unReturning :: ByteString } deriving IsString

data JoinType
    = Inner
    | Left
    | Right
    | LeftOuter
    | RightOuter

newtype QueryString = QueryString { unQueryString :: ByteString } deriving IsString

data Condition
    = Body !QueryString
    | WithQuery !QueryString !Select
    | And !Condition !Condition
    | Or !Condition !Condition

withQuery :: QueryString -> Select -> Condition
withQuery = WithQuery

infixr 5 $&
($&) :: Condition -> Condition -> Condition
($&) = And

infixr 6 $|
($|) :: Condition -> Condition -> Condition
($|) = Or

instance IsString Condition where
    fromString = Body . QueryString . BS.pack

select :: SelectTable -> Select
select table = Select table (all :| []) Indistinct Nothing [] [] [] [] Nothing Nothing

insert :: Table -> InsertFields -> Insert
insert table fs = Insert table fs Nothing Nothing

update :: Table -> UpdateFields -> Update
update table fs = Update table fs Nothing [] Nothing

delete :: Table -> Delete
delete table = Delete table Nothing [] Nothing

all :: SelectFields
all = SelectAll Nothing

allAgg :: AggFunc -> SelectFields
allAgg = SelectAll . Just

fields :: NonEmpty SelectFields -> Select -> Select
fields fld q = q { selectFields = fld }

class SelectList a r | r -> a where
    selectList :: Maybe AggFunc -> NonEmpty a -> r

instance SelectList ByteString SelectFields where
    selectList func = SelectFields func . NonEmpty.reverse

instance SelectList a r => SelectList a (a -> r) where
    selectList func xs x = selectList func (NonEmpty.cons x xs)

agg :: SelectList ByteString r => AggFunc -> ByteString -> r
agg func f = selectList (Just func) (f :| [])

raw :: SelectList ByteString r => ByteString -> r
raw f = selectList Nothing (f :| [])

class UpdateList a r | r -> a where
    updateList :: NonEmpty a -> r

instance UpdateList Field (NonEmpty Field) where
    updateList = NonEmpty.reverse

instance UpdateList Assign InsertFields where
    updateList = InsertFields . NonEmpty.reverse

instance UpdateList Assign UpdateFields where
    updateList = UpdateFields . NonEmpty.reverse

instance UpdateList Assign ConflictAction where
    updateList = Perform . NonEmpty.reverse

instance UpdateList a r => UpdateList a (a -> r) where
    updateList xs x = updateList (NonEmpty.cons x xs)

set :: UpdateList a r => a -> r
set f = updateList (f :| [])

from :: Select -> NonEmpty Field -> InsertFields
from = flip FieldsFrom

distinct :: Select -> Select
distinct q = q { selectDistinct = Distinct }

class Conditional q where
    where_ :: Condition -> q -> q

instance Conditional Select where
    where_ cond q = q { selectCondition = appendCondition cond (selectCondition q) }

instance Conditional Update where
    where_ cond q = q { updateCondition = appendCondition cond (updateCondition q) }

instance Conditional Delete where
    where_ cond q = q { deleteCondition = appendCondition cond (deleteCondition q) }

appendCondition :: Condition -> Maybe Condition -> Maybe Condition
appendCondition cond = Just . maybe cond (cond$&)

type JoinFunc q = Join -> Table -> On -> Condition -> q -> q

class Joinable q where
    joinGeneric :: JoinType -> JoinFunc q

instance Joinable Select where
    joinGeneric typ _ table _ cond q = q { selectJoin = joinQuery : selectJoin q }
      where joinQuery = JoinTable typ table cond

instance Joinable Update where
    joinGeneric typ _ table _ cond q = q { updateJoin = joinQuery : updateJoin q }
      where joinQuery = JoinTable typ table cond

instance Joinable Delete where
    joinGeneric typ _ table _ cond q = q { deleteJoin = joinQuery : deleteJoin q }
      where joinQuery = JoinTable typ table cond

inner, left, right, leftOuter, rightOuter :: Joinable q => JoinFunc q
inner      = joinGeneric Inner
left       = joinGeneric Left
right      = joinGeneric Right
leftOuter  = joinGeneric LeftOuter
rightOuter = joinGeneric RightOuter

data Join = Join
data On = On

join :: Join
join = Join

on :: On
on = On

subQuery :: QueryString -> Select -> Select -> Select
subQuery comb sub q = q { selectSubQueries = SubQuery comb sub : selectSubQueries q }

group :: Group -> Select -> Select
group grp q = q { selectGroup = grp : selectGroup q }

order :: Order -> Select -> Select
order ord q = q { selectOrder = ord : selectOrder q }

limit :: Int -> Select -> Select
limit lim q = q { selectLimit = Just $ Limit lim }

offset :: Int -> Select -> Select
offset off q = q { selectOffset = Just $ Offset off }

onConflict :: ConflictTarget -> ConflictCondition -> ConflictAction -> Insert -> Insert
onConflict tgt cnd act q = q { insertOnConflict = Just conf }
  where conf = OnConflict tgt act cnd

any :: ConflictTarget
any = AnyTarget

for :: Condition -> ConflictCondition
for = ConflictCondition

always :: ConflictCondition
always = Always

doNothing :: ConflictAction
doNothing = DoNothing

class Returnable q where
    returning :: Returning -> q -> q

instance Returnable Insert where
    returning ret q = q { insertReturning = Just ret }

instance Returnable Update where
    returning ret q = q { updateReturning = Just ret }

instance Returnable Delete where
    returning ret q = q { deleteReturning = Just ret }

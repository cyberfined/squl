{-# LANGUAGE RecordWildCards #-}

module Database.Squl.Builder.Common
    ( Buildable(..)
    ) where

import Database.Squl.Types hiding (query)
import Data.ByteString.Builder
import Data.List.NonEmpty         (NonEmpty(..))
import Prelude             hiding (Either(..))

class Buildable q where
    buildQuery :: q -> Builder

instance Buildable Select where
    buildQuery = buildSelect

instance Buildable Insert where
    buildQuery = buildInsert

instance Buildable Update where
    buildQuery = buildUpdate

instance Buildable Delete where
    buildQuery = buildDelete

buildSelect :: Select -> Builder
buildSelect Select{..} = "SELECT "
                       <> showDistinct
                       <> showFields selectFields
                       <> showSelect
                       <> buildJoins selectJoin
                       <> buildWhere selectCondition
                       <> showGroupBy
                       <> showSubQueries
                       <> showOrderBy
                       <> showLimit
                       <> showOffset
  where showSelect = case selectTable of
            SelectTable table -> " FROM " <> lazyByteString table
            NoTable           -> ""
        showDistinct = case selectDistinct of
            Distinct   -> "DISTINCT "
            Indistinct -> ""
        showTable = case selectTable of
            SelectTable table -> lazyByteString table
            NoTable           -> ""
        showFields = intercalate ", " . fmap showAggFields
        showAggFields = \case
            SelectAll aggFunc ->
                showAggFunc (showTable <> ".*") aggFunc
            SelectFields aggFunc fs ->
                showAggFunc (intercalate ", " $ fmap lazyByteString fs) aggFunc
        showAggFunc fs = \case
            Just (AggFunc func) -> lazyByteString func <> "(" <> fs <> ")"
            Nothing             -> fs
        
        showGroupBy = case selectGroup of
            (g:gs) -> " GROUP BY " <> foldr showGroup (groupAcc g) gs
            _      -> ""
        showGroup (Group g) acc = acc <> ", (" <> lazyByteString g <> ")"
        groupAcc (Group g) = "(" <> lazyByteString g <> ")"

        showSubQueries = case selectSubQueries of
            (q:qs) -> 
                foldr (\x acc -> acc <> " " <> showSubQuery x) (" " <> showSubQuery q) qs
            _      -> ""
        showSubQuery (SubQuery (QueryString comb) query) =
            lazyByteString comb <> " (" <> buildSelect query <> ")"

        showOrderBy = case selectOrder of
            (o:os) -> " ORDER BY " <> foldr showOrder (orderAcc o) os
            _      -> ""
        showOrder (Order o) acc = acc <> ", (" <> lazyByteString o <> ")"
        orderAcc (Order o) = "(" <> lazyByteString o <> ")"

        showLimit = maybe "" ((" LIMIT " <>) . intDec . unLimit) selectLimit

        showOffset = maybe "" ((" OFFSET " <>) . intDec . unOffset) selectOffset

buildInsert :: Insert -> Builder
buildInsert Insert{..} =  "INSERT INTO "
                       <> lazyByteString (unTable insertTable)
                       <> " (" <> showFields <> ") "
                       <> showSource
                       <> maybe "" showOnConflict insertOnConflict
                       <> buildReturning insertReturning
  where showFields = intercalate ", " $ case insertFields of
            InsertFields fs -> fmap (\(f := _) -> lazyByteString $ unField f) fs
            FieldsFrom fs _ -> fmap (lazyByteString . unField) fs
        showSource = case insertFields of
            InsertFields fs -> "VALUES ("
                            <> intercalate ", " (fmap (\(_ := v) -> buildValue v) fs)
                            <> ")"
            FieldsFrom _ q  -> buildSelect q

        showOnConflict OnConflict{..} = " ON CONFLICT"
                                      <> showConflictTarget conflictTarget
                                      <> showConflictAction conflictAction
                                      <> showConflictCondition conflictCondition
        showConflictTarget = \case
            ConflictTarget tgt -> " " <> lazyByteString tgt
            AnyTarget          -> ""
        showConflictAction = \case
            DoNothing    -> " DO NOTHING"
            Perform upds -> " DO UPDATE SET " <> buildUpdateFields upds
        showConflictCondition = \case
            Always -> ""
            ConflictCondition cnd -> " WHERE " <> buildCondition cnd

buildUpdate :: Update -> Builder
buildUpdate Update{..} =  "UPDATE "
                       <> lazyByteString (unTable updateTable)
                       <> " SET "
                       <> buildUpdateFields (unUpdateFields updateFields)
                       <> buildWhere condition
                       <> buildReturning updateReturning
  where condition = joinToCondition updateTable updateJoin updateCondition

buildDelete :: Delete -> Builder
buildDelete Delete{..} =  "DELETE FROM "
                       <> lazyByteString (unTable deleteTable)
                       <> buildWhere condition
                       <> buildReturning deleteReturning
  where condition = joinToCondition deleteTable deleteJoin deleteCondition
  
joinToCondition :: Table -> [JoinTable] -> Maybe Condition -> Maybe Condition
joinToCondition table joins condition
  | null joins = condition
  | otherwise  = Just $ maybe joinCondition (joinCondition $&) condition
  where joinCondition = withQuery combination subSelect
        subSelect = (select $ SelectTable $ unTable table)
            { selectFields = idField
            , selectJoin   = joins
            }
        idField = raw (unTable table <> ".id") :| []
        combination = (QueryString $ unTable table <> ".id IN")

buildCondition :: Condition -> Builder
buildCondition = \case
    Body (QueryString cond) -> "(" <> lazyByteString cond <> ")"
    And c1 c2 -> "(" <> buildCondition c1 <> " AND " <> buildCondition c2 <> ")"
    Or  c1 c2 -> "(" <> buildCondition c1 <> " OR "  <> buildCondition c2 <> ")"
    WithQuery (QueryString comb) query ->
        "(" <> lazyByteString comb <> " (" <> buildSelect query <> "))"

buildJoins :: [JoinTable] -> Builder
buildJoins = \case
    (j:js) -> foldr (\x acc -> acc <> " " <> buildJoin x) (" " <> buildJoin j) js
    _      -> ""
  where buildJoin (JoinTable typ table cond) =  buildJoinType typ
                                             <> " JOIN "
                                             <> lazyByteString (unTable table)
                                             <> " ON "
                                             <> buildCondition cond
        buildJoinType = \case
            Inner      -> "INNER"
            Left       -> "LEFT"
            Right      -> "RIGHT"
            LeftOuter  -> "LEFT OUTER"
            RightOuter -> "RIGHT OUTER"

buildWhere :: Maybe Condition -> Builder
buildWhere = buildMaybe buildCondition " WHERE "

buildReturning :: Maybe Returning -> Builder
buildReturning = buildMaybe (lazyByteString . unReturning) " RETURNING "

buildMaybe :: (a -> Builder) -> Builder -> Maybe a -> Builder
buildMaybe build prefix = maybe "" ((prefix <>) . build)

buildUpdateFields :: NonEmpty Assign -> Builder
buildUpdateFields = intercalate ", " . fmap showAssign
  where showAssign (f := v) = lazyByteString (unField f) <> " = " <> buildValue v

buildValue :: Value -> Builder
buildValue = \case
    Query q -> "(" <> buildSelect q <> ")"
    Expr e  -> "(" <> lazyByteString e <> ")"

intercalate :: Builder -> NonEmpty Builder -> Builder
intercalate del (x :| xs) = foldr (\y acc -> acc <> del <> y) x xs

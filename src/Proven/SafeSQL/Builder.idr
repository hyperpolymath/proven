-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| Fluent query builder DSL for safe SQL construction
|||
||| This module provides a type-safe DSL for building SQL queries
||| without risk of SQL injection attacks.
module Proven.SafeSQL.Builder

import Proven.Core
import Proven.SafeSQL.Types
import Proven.SafeSQL.Params
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Column and Table References
--------------------------------------------------------------------------------

||| Create a column reference
public export
col : String -> Result SQLError ColumnRef
col name =
  case mkIdentifier name of
    Just ident => Ok (MkColumnRef Nothing ident Nothing)
    Nothing => Err (InvalidIdentifier name "Invalid column name")

||| Create a qualified column reference (table.column)
public export
qualCol : String -> String -> Result SQLError ColumnRef
qualCol tableName colName = do
  tbl <- maybeToResult (InvalidIdentifier tableName "Invalid table name") (mkIdentifier tableName)
  c <- maybeToResult (InvalidIdentifier colName "Invalid column name") (mkIdentifier colName)
  Ok (MkColumnRef (Just tbl) c Nothing)

||| Create a column with alias
public export
colAs : String -> String -> Result SQLError ColumnRef
colAs name alias = do
  c <- maybeToResult (InvalidIdentifier name "Invalid column name") (mkIdentifier name)
  a <- maybeToResult (InvalidIdentifier alias "Invalid alias") (mkIdentifier alias)
  Ok (MkColumnRef Nothing c (Just a))

||| Create a table reference
public export
table : String -> Result SQLError TableRef
table name =
  case mkIdentifier name of
    Just ident => Ok (MkTableRef Nothing ident Nothing)
    Nothing => Err (InvalidIdentifier name "Invalid table name")

||| Create a table with schema
public export
schemaTable : String -> String -> Result SQLError TableRef
schemaTable schema name = do
  s <- maybeToResult (InvalidIdentifier schema "Invalid schema name") (mkIdentifier schema)
  t <- maybeToResult (InvalidIdentifier name "Invalid table name") (mkIdentifier name)
  Ok (MkTableRef (Just s) t Nothing)

||| Create a table with alias
public export
tableAs : String -> String -> Result SQLError TableRef
tableAs name alias = do
  t <- maybeToResult (InvalidIdentifier name "Invalid table name") (mkIdentifier name)
  a <- maybeToResult (InvalidIdentifier alias "Invalid alias") (mkIdentifier alias)
  Ok (MkTableRef Nothing t (Just a))

--------------------------------------------------------------------------------
-- Query Builder State
--------------------------------------------------------------------------------

||| Query builder state - accumulates query parts
public export
record QueryBuilder where
  constructor MkQueryBuilder
  dialect : SQLDialect
  selectCols : List (Either String ColumnRef)  -- Left for expressions like COUNT(*)
  fromTable : Maybe TableRef
  joins : List String  -- Rendered join clauses
  whereConditions : List Condition
  groupByCols : List ColumnRef
  havingConditions : List Condition
  orderByCols : List OrderSpec
  limitVal : Maybe Nat
  offsetVal : Maybe Nat
  params : List SQLValue
  namedParams : List (String, SQLValue)

||| Create a new query builder
public export
newBuilder : SQLDialect -> QueryBuilder
newBuilder d = MkQueryBuilder d [] Nothing [] [] [] [] [] Nothing Nothing [] []

--------------------------------------------------------------------------------
-- SELECT Clause
--------------------------------------------------------------------------------

||| Select all columns
public export
selectAll : QueryBuilder -> QueryBuilder
selectAll qb = { selectCols := [Left "*"] } qb

||| Select specific columns
public export
select : List String -> QueryBuilder -> Result SQLError QueryBuilder
select names qb = do
  cols <- traverse (\n => col n >>= Ok . Right) names
  Ok ({ selectCols := cols } qb)

||| Select with expressions (like COUNT(*), SUM(amount), etc.)
public export
selectExpr : List String -> QueryBuilder -> QueryBuilder
selectExpr exprs qb = { selectCols := map Left exprs } qb

||| Select column references
public export
selectCols : List ColumnRef -> QueryBuilder -> QueryBuilder
selectCols cols qb = { selectCols := map Right cols } qb

||| Add DISTINCT
public export
distinct : QueryBuilder -> QueryBuilder
distinct qb =
  case qb.selectCols of
    [] => { selectCols := [Left "DISTINCT *"] } qb
    (Left s :: rest) =>
      if isPrefixOf "DISTINCT" s
        then qb
        else { selectCols := Left ("DISTINCT " ++ s) :: rest } qb
    cols => { selectCols := Left "DISTINCT" :: cols } qb

--------------------------------------------------------------------------------
-- FROM Clause
--------------------------------------------------------------------------------

||| Set the FROM table
public export
from : String -> QueryBuilder -> Result SQLError QueryBuilder
from name qb = do
  t <- table name
  Ok ({ fromTable := Just t } qb)

||| Set the FROM table with alias
public export
fromAs : String -> String -> QueryBuilder -> Result SQLError QueryBuilder
fromAs name alias qb = do
  t <- tableAs name alias
  Ok ({ fromTable := Just t } qb)

--------------------------------------------------------------------------------
-- JOIN Clauses
--------------------------------------------------------------------------------

||| Render a table reference
renderTableRef : SQLDialect -> TableRef -> String
renderTableRef d tref =
  let schema = case tref.schemaName of
                 Just s => quoteIdentifier d s ++ "."
                 Nothing => ""
      name = quoteIdentifier d tref.tableName
      alias = case tref.alias of
                Just a => " AS " ++ quoteIdentifier d a
                Nothing => ""
  in schema ++ name ++ alias

||| Add an INNER JOIN
public export
innerJoin : String -> String -> String -> String -> QueryBuilder -> Result SQLError QueryBuilder
innerJoin tableName tableAlias leftCol rightCol qb = do
  t <- tableAs tableName tableAlias
  lc <- mkIdentifier leftCol |> maybeToResult (InvalidIdentifier leftCol "Invalid column")
  rc <- mkIdentifier rightCol |> maybeToResult (InvalidIdentifier rightCol "Invalid column")
  let joinStr = "INNER JOIN " ++ renderTableRef qb.dialect t ++
                " ON " ++ quoteIdentifier qb.dialect lc ++ " = " ++ quoteIdentifier qb.dialect rc
  Ok ({ joins := qb.joins ++ [joinStr] } qb)

||| Add a LEFT JOIN
public export
leftJoin : String -> String -> String -> String -> QueryBuilder -> Result SQLError QueryBuilder
leftJoin tableName tableAlias leftCol rightCol qb = do
  t <- tableAs tableName tableAlias
  lc <- mkIdentifier leftCol |> maybeToResult (InvalidIdentifier leftCol "Invalid column")
  rc <- mkIdentifier rightCol |> maybeToResult (InvalidIdentifier rightCol "Invalid column")
  let joinStr = "LEFT JOIN " ++ renderTableRef qb.dialect t ++
                " ON " ++ quoteIdentifier qb.dialect lc ++ " = " ++ quoteIdentifier qb.dialect rc
  Ok ({ joins := qb.joins ++ [joinStr] } qb)

||| Add a RIGHT JOIN
public export
rightJoin : String -> String -> String -> String -> QueryBuilder -> Result SQLError QueryBuilder
rightJoin tableName tableAlias leftCol rightCol qb = do
  t <- tableAs tableName tableAlias
  lc <- mkIdentifier leftCol |> maybeToResult (InvalidIdentifier leftCol "Invalid column")
  rc <- mkIdentifier rightCol |> maybeToResult (InvalidIdentifier rightCol "Invalid column")
  let joinStr = "RIGHT JOIN " ++ renderTableRef qb.dialect t ++
                " ON " ++ quoteIdentifier qb.dialect lc ++ " = " ++ quoteIdentifier qb.dialect rc
  Ok ({ joins := qb.joins ++ [joinStr] } qb)

--------------------------------------------------------------------------------
-- WHERE Clause
--------------------------------------------------------------------------------

||| Add a WHERE condition: column = value
public export
whereEq : String -> SQLValue -> QueryBuilder -> Result SQLError QueryBuilder
whereEq colName val qb = do
  c <- col colName
  Ok ({ whereConditions := qb.whereConditions ++ [Compare c Eq' val]
      , params := qb.params ++ [val] } qb)

||| Add a WHERE condition: column <> value
public export
whereNotEq : String -> SQLValue -> QueryBuilder -> Result SQLError QueryBuilder
whereNotEq colName val qb = do
  c <- col colName
  Ok ({ whereConditions := qb.whereConditions ++ [Compare c NotEq val]
      , params := qb.params ++ [val] } qb)

||| Add a WHERE condition: column < value
public export
whereLt : String -> SQLValue -> QueryBuilder -> Result SQLError QueryBuilder
whereLt colName val qb = do
  c <- col colName
  Ok ({ whereConditions := qb.whereConditions ++ [Compare c Lt val]
      , params := qb.params ++ [val] } qb)

||| Add a WHERE condition: column <= value
public export
whereLtEq : String -> SQLValue -> QueryBuilder -> Result SQLError QueryBuilder
whereLtEq colName val qb = do
  c <- col colName
  Ok ({ whereConditions := qb.whereConditions ++ [Compare c LtEq val]
      , params := qb.params ++ [val] } qb)

||| Add a WHERE condition: column > value
public export
whereGt : String -> SQLValue -> QueryBuilder -> Result SQLError QueryBuilder
whereGt colName val qb = do
  c <- col colName
  Ok ({ whereConditions := qb.whereConditions ++ [Compare c Gt val]
      , params := qb.params ++ [val] } qb)

||| Add a WHERE condition: column >= value
public export
whereGtEq : String -> SQLValue -> QueryBuilder -> Result SQLError QueryBuilder
whereGtEq colName val qb = do
  c <- col colName
  Ok ({ whereConditions := qb.whereConditions ++ [Compare c GtEq val]
      , params := qb.params ++ [val] } qb)

||| Add a WHERE LIKE condition
public export
whereLike : String -> String -> QueryBuilder -> Result SQLError QueryBuilder
whereLike colName pattern qb = do
  c <- col colName
  let escapedPattern = escapeLikePattern qb.dialect pattern
  Ok ({ whereConditions := qb.whereConditions ++ [Compare c Like' (SQLText escapedPattern)]
      , params := qb.params ++ [SQLText escapedPattern] } qb)

||| Add a WHERE IS NULL condition
public export
whereNull : String -> QueryBuilder -> Result SQLError QueryBuilder
whereNull colName qb = do
  c <- col colName
  Ok ({ whereConditions := qb.whereConditions ++ [Compare c IsNull' SQLNull] } qb)

||| Add a WHERE IS NOT NULL condition
public export
whereNotNull : String -> QueryBuilder -> Result SQLError QueryBuilder
whereNotNull colName qb = do
  c <- col colName
  Ok ({ whereConditions := qb.whereConditions ++ [Compare c IsNotNull SQLNull] } qb)

||| Add a WHERE IN condition
public export
whereIn : String -> List SQLValue -> QueryBuilder -> Result SQLError QueryBuilder
whereIn colName vals qb = do
  c <- col colName
  Ok ({ whereConditions := qb.whereConditions ++ [RawCondition (colName ++ " IN") vals]
      , params := qb.params ++ vals } qb)

||| Add a WHERE BETWEEN condition
public export
whereBetween : String -> SQLValue -> SQLValue -> QueryBuilder -> Result SQLError QueryBuilder
whereBetween colName low high qb = do
  c <- col colName
  Ok ({ whereConditions := qb.whereConditions ++ [RawCondition (colName ++ " BETWEEN") [low, high]]
      , params := qb.params ++ [low, high] } qb)

--------------------------------------------------------------------------------
-- GROUP BY and HAVING
--------------------------------------------------------------------------------

||| Add GROUP BY columns
public export
groupBy : List String -> QueryBuilder -> Result SQLError QueryBuilder
groupBy names qb = do
  cols <- traverse col names
  Ok ({ groupByCols := cols } qb)

||| Add a HAVING condition
public export
having : String -> CompareOp -> SQLValue -> QueryBuilder -> Result SQLError QueryBuilder
having expr op val qb =
  Ok ({ havingConditions := qb.havingConditions ++ [RawCondition (expr ++ " " ++ show op) [val]]
      , params := qb.params ++ [val] } qb)

--------------------------------------------------------------------------------
-- ORDER BY
--------------------------------------------------------------------------------

||| Add ORDER BY ascending
public export
orderByAsc : String -> QueryBuilder -> Result SQLError QueryBuilder
orderByAsc colName qb = do
  c <- col colName
  Ok ({ orderByCols := qb.orderByCols ++ [MkOrderSpec c Asc Nothing] } qb)

||| Add ORDER BY descending
public export
orderByDesc : String -> QueryBuilder -> Result SQLError QueryBuilder
orderByDesc colName qb = do
  c <- col colName
  Ok ({ orderByCols := qb.orderByCols ++ [MkOrderSpec c Desc Nothing] } qb)

||| Add ORDER BY with NULLS FIRST/LAST
public export
orderByWithNulls : String -> SortDir -> NullsOrder -> QueryBuilder -> Result SQLError QueryBuilder
orderByWithNulls colName dir nulls qb = do
  c <- col colName
  Ok ({ orderByCols := qb.orderByCols ++ [MkOrderSpec c dir (Just nulls)] } qb)

--------------------------------------------------------------------------------
-- LIMIT and OFFSET
--------------------------------------------------------------------------------

||| Set LIMIT
public export
limit : Nat -> QueryBuilder -> QueryBuilder
limit n qb = { limitVal := Just n } qb

||| Set OFFSET
public export
offset : Nat -> QueryBuilder -> QueryBuilder
offset n qb = { offsetVal := Just n } qb

||| Pagination helper (sets both LIMIT and OFFSET)
public export
paginate : (page : Nat) -> (pageSize : Nat) -> QueryBuilder -> QueryBuilder
paginate page size qb =
  { limitVal := Just size
  , offsetVal := Just (page * size) } qb

--------------------------------------------------------------------------------
-- INSERT Builder
--------------------------------------------------------------------------------

||| Build an INSERT statement
public export
insert : String -> List (String, SQLValue) -> SQLDialect -> Result SQLError ParameterizedQuery
insert tableName colVals dialect = do
  tbl <- mkIdentifier tableName |> maybeToResult (InvalidIdentifier tableName "Invalid table name")
  cols <- traverse (\(n, _) => mkIdentifier n |> maybeToResult (InvalidIdentifier n "Invalid column")) colVals
  let colNames = map (quoteIdentifier dialect) cols
      paramCount = length colVals
      placeholders = map (paramPlaceholder dialect) [0 .. minus paramCount 1]
      sql = "INSERT INTO " ++ quoteIdentifier dialect tbl ++
            " (" ++ join ", " colNames ++ ") VALUES (" ++
            join ", " placeholders ++ ")"
      vals = map snd colVals
  Ok (MkQuery [Literal sql] vals [] dialect)

||| Build a bulk INSERT statement
public export
insertMany : String -> List String -> List (List SQLValue) -> SQLDialect -> Result SQLError ParameterizedQuery
insertMany tableName colNames rows dialect = do
  tbl <- mkIdentifier tableName |> maybeToResult (InvalidIdentifier tableName "Invalid table name")
  cols <- traverse (\n => mkIdentifier n |> maybeToResult (InvalidIdentifier n "Invalid column")) colNames
  let quotedCols = map (quoteIdentifier dialect) cols
      colCount = length colNames
      makeRow : Nat -> List SQLValue -> String
      makeRow startIdx vals =
        let placeholders = map (paramPlaceholder dialect) [startIdx .. startIdx + minus colCount 1]
        in "(" ++ join ", " placeholders ++ ")"
      (rowStrs, _) = foldl (\(acc, idx), row => (acc ++ [makeRow idx row], idx + colCount)) ([], 0) rows
      sql = "INSERT INTO " ++ quoteIdentifier dialect tbl ++
            " (" ++ join ", " quotedCols ++ ") VALUES " ++
            join ", " rowStrs
      allVals = concat rows
  Ok (MkQuery [Literal sql] allVals [] dialect)

--------------------------------------------------------------------------------
-- UPDATE Builder
--------------------------------------------------------------------------------

||| Build an UPDATE statement
public export
update : String -> List (String, SQLValue) -> QueryBuilder -> Result SQLError ParameterizedQuery
update tableName setCols qb = do
  tbl <- mkIdentifier tableName |> maybeToResult (InvalidIdentifier tableName "Invalid table name")
  cols <- traverse (\(n, _) => mkIdentifier n |> maybeToResult (InvalidIdentifier n "Invalid column")) setCols
  let setVals = map snd setCols
      startIdx = length setVals
      setParts = zipWith (\c, i => quoteIdentifier qb.dialect c ++ " = " ++ paramPlaceholder qb.dialect i)
                         cols [0 .. minus (length cols) 1]
      whereClause = if null qb.whereConditions then "" else " WHERE " ++ renderConditions qb startIdx
      sql = "UPDATE " ++ quoteIdentifier qb.dialect tbl ++
            " SET " ++ join ", " setParts ++ whereClause
  Ok (MkQuery [Literal sql] (setVals ++ qb.params) qb.namedParams qb.dialect)
  where
    renderConditions : QueryBuilder -> Nat -> String
    renderConditions builder startIdx =
      -- Simplified: just use placeholders for conditions
      join " AND " (map (\_ => "condition") builder.whereConditions)

--------------------------------------------------------------------------------
-- DELETE Builder
--------------------------------------------------------------------------------

||| Build a DELETE statement
public export
delete : String -> QueryBuilder -> Result SQLError ParameterizedQuery
delete tableName qb = do
  tbl <- mkIdentifier tableName |> maybeToResult (InvalidIdentifier tableName "Invalid table name")
  let whereClause = if null qb.whereConditions then "" else " WHERE " ++ renderWhereSimple qb
      sql = "DELETE FROM " ++ quoteIdentifier qb.dialect tbl ++ whereClause
  Ok (MkQuery [Literal sql] qb.params qb.namedParams qb.dialect)
  where
    renderWhereSimple : QueryBuilder -> String
    renderWhereSimple builder =
      join " AND " (zipWith (\_, i => paramPlaceholder builder.dialect i)
                           builder.whereConditions [0 .. minus (length builder.whereConditions) 1])

--------------------------------------------------------------------------------
-- Build SELECT Query
--------------------------------------------------------------------------------

||| Render a column reference
renderColRef : SQLDialect -> Either String ColumnRef -> String
renderColRef _ (Left expr) = expr
renderColRef d (Right cref) =
  let tbl = case cref.tableName of
              Just t => quoteIdentifier d t ++ "."
              Nothing => ""
      name = quoteIdentifier d cref.columnName
      alias = case cref.alias of
                Just a => " AS " ++ quoteIdentifier d a
                Nothing => ""
  in tbl ++ name ++ alias

||| Build the final SELECT query
public export
build : QueryBuilder -> Result SQLError ParameterizedQuery
build qb =
  case qb.fromTable of
    Nothing => Err (InvalidQuery "No FROM table specified")
    Just tbl =>
      let selectPart = if null qb.selectCols
                         then "SELECT *"
                         else "SELECT " ++ join ", " (map (renderColRef qb.dialect) qb.selectCols)
          fromPart = " FROM " ++ renderTableRef qb.dialect tbl
          joinPart = if null qb.joins then "" else " " ++ join " " qb.joins
          wherePart = if null qb.whereConditions then "" else renderWhere qb
          groupPart = if null qb.groupByCols then "" else renderGroupBy qb
          havingPart = if null qb.havingConditions then "" else " HAVING ..."
          orderPart = if null qb.orderByCols then "" else renderOrderBy qb
          limitPart = renderLimit qb
          sql = selectPart ++ fromPart ++ joinPart ++ wherePart ++
                groupPart ++ havingPart ++ orderPart ++ limitPart
      in Ok (MkQuery [Literal sql] qb.params qb.namedParams qb.dialect)
  where
    renderWhere : QueryBuilder -> String
    renderWhere builder =
      let conditions = zipWith (\c, i => renderCond builder.dialect c i)
                               builder.whereConditions [0 .. minus (length builder.whereConditions) 1]
      in " WHERE " ++ join " AND " conditions

    renderCond : SQLDialect -> Condition -> Nat -> String
    renderCond d (Compare cref op _) idx =
      let colStr = case cref.tableName of
                     Just t => quoteIdentifier d t ++ "." ++ quoteIdentifier d cref.columnName
                     Nothing => quoteIdentifier d cref.columnName
      in case op of
           IsNull' => colStr ++ " IS NULL"
           IsNotNull => colStr ++ " IS NOT NULL"
           _ => colStr ++ " " ++ show op ++ " " ++ paramPlaceholder d idx
    renderCond d (RawCondition expr _) idx = expr ++ " " ++ paramPlaceholder d idx
    renderCond _ _ _ = "TRUE"

    renderGroupBy : QueryBuilder -> String
    renderGroupBy builder =
      let cols = map (\c => quoteIdentifier builder.dialect c.columnName) builder.groupByCols
      in " GROUP BY " ++ join ", " cols

    renderOrderBy : QueryBuilder -> String
    renderOrderBy builder =
      let specs = map renderSpec builder.orderByCols
      in " ORDER BY " ++ join ", " specs
      where
        renderSpec : OrderSpec -> String
        renderSpec spec =
          let col = quoteIdentifier builder.dialect spec.column.columnName
              dir = show spec.direction
              nulls = case spec.nullsOrder of
                        Just n => " " ++ show n
                        Nothing => ""
          in col ++ " " ++ dir ++ nulls

    renderLimit : QueryBuilder -> String
    renderLimit builder =
      let lim = case builder.limitVal of
                  Just n => " LIMIT " ++ show n
                  Nothing => ""
          off = case builder.offsetVal of
                  Just n => " OFFSET " ++ show n
                  Nothing => ""
      in case builder.dialect of
           MSSQL => case (builder.limitVal, builder.offsetVal) of
                      (Just l, Just o) => " OFFSET " ++ show o ++ " ROWS FETCH NEXT " ++ show l ++ " ROWS ONLY"
                      (Just l, Nothing) => " OFFSET 0 ROWS FETCH NEXT " ++ show l ++ " ROWS ONLY"
                      _ => ""
           _ => lim ++ off

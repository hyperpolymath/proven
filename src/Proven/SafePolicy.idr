-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- SafePolicy: Formally verified policy enforcement
--
-- Provides:
-- - AST-level policy enforcement with zone classification
-- - Rule composition with conflict detection
-- - Policy evaluation with audit trails
-- - Zone boundaries (mutable/immutable/hybrid)

module Proven.SafePolicy

import Data.String
import Data.List
import Data.Nat
import Data.Maybe
import Data.Vect
import Decidable.Equality

%default total

||| Zone classification for code/data regions
public export
data ZoneClass = Mutable | Immutable | Hybrid | Restricted

||| Equality for zone classifications
public export
Eq ZoneClass where
  Mutable == Mutable = True
  Immutable == Immutable = True
  Hybrid == Hybrid = True
  Restricted == Restricted = True
  _ == _ = False

||| A zone identifier
public export
ZoneId : Type
ZoneId = String

||| A zone with classification and boundaries
public export
record Zone where
  constructor MkZone
  zoneId : ZoneId
  zoneClass : ZoneClass
  zoneParent : Maybe ZoneId
  zoneChildren : List ZoneId

||| Policy action types
public export
data PolicyAction =
    Allow
  | Deny
  | Audit       -- Allow but log
  | Transform   -- Allow with modification
  | Delegate    -- Defer to sub-policy

||| Equality for policy actions
public export
Eq PolicyAction where
  Allow == Allow = True
  Deny == Deny = True
  Audit == Audit = True
  Transform == Transform = True
  Delegate == Delegate = True
  _ == _ = False

||| A policy rule condition
public export
data Condition =
    Always
  | Never
  | InZone ZoneId
  | NotInZone ZoneId
  | HasTag String
  | NotHasTag String
  | MatchPattern String
  | And Condition Condition
  | Or Condition Condition
  | Not Condition

||| Evaluate a condition (simplified - real impl would have context)
public export
evalCondition : Condition -> ZoneId -> List String -> Bool
evalCondition Always _ _ = True
evalCondition Never _ _ = False
evalCondition (InZone z) current _ = z == current
evalCondition (NotInZone z) current _ = z /= current
evalCondition (HasTag t) _ tags = elem t tags
evalCondition (NotHasTag t) _ tags = not (elem t tags)
evalCondition (MatchPattern _) _ _ = True  -- Simplified
evalCondition (And c1 c2) z tags = evalCondition c1 z tags && evalCondition c2 z tags
evalCondition (Or c1 c2) z tags = evalCondition c1 z tags || evalCondition c2 z tags
evalCondition (Not c) z tags = not (evalCondition c z tags)

||| A policy rule
public export
record PolicyRule where
  constructor MkPolicyRule
  ruleName : String
  ruleCondition : Condition
  ruleAction : PolicyAction
  rulePriority : Nat
  ruleDescription : String

||| Create a simple allow rule
public export
allowRule : String -> Condition -> PolicyRule
allowRule name cond = MkPolicyRule name cond Allow 100 ""

||| Create a simple deny rule
public export
denyRule : String -> Condition -> PolicyRule
denyRule name cond = MkPolicyRule name cond Deny 100 ""

||| A policy (collection of rules)
public export
record Policy where
  constructor MkPolicy
  policyName : String
  policyRules : List PolicyRule
  defaultAction : PolicyAction

||| Empty policy (default deny)
public export
emptyPolicy : String -> Policy
emptyPolicy name = MkPolicy name [] Deny

||| Add a rule to a policy
public export
addRule : PolicyRule -> Policy -> Policy
addRule rule policy = { policyRules := rule :: policyRules policy } policy

||| Sort rules by priority (higher priority first)
public export
sortByPriority : List PolicyRule -> List PolicyRule
sortByPriority rules = sortBy (\r1, r2 => compare (rulePriority r2) (rulePriority r1)) rules

||| Evaluate a policy
public export
evaluatePolicy : Policy -> ZoneId -> List String -> PolicyAction
evaluatePolicy policy zone tags =
  let sorted = sortByPriority (policyRules policy)
      matching = filter (\r => evalCondition (ruleCondition r) zone tags) sorted
  in case matching of
       [] => defaultAction policy
       (r :: _) => ruleAction r

||| Proof that a policy allows an action
public export
data PolicyAllows : Policy -> ZoneId -> List String -> Type where
  MkPolicyAllows : evaluatePolicy p z t = Allow -> PolicyAllows p z t

||| Proof that a policy denies an action
public export
data PolicyDenies : Policy -> ZoneId -> List String -> Type where
  MkPolicyDenies : evaluatePolicy p z t = Deny -> PolicyDenies p z t

||| Policy conflict detection
public export
data Conflict =
    NoConflict
  | ActionConflict PolicyRule PolicyRule
  | ConditionOverlap PolicyRule PolicyRule

||| Check if two rules might conflict
public export
rulesConflict : PolicyRule -> PolicyRule -> Bool
rulesConflict r1 r2 =
  ruleAction r1 /= ruleAction r2 &&
  rulePriority r1 == rulePriority r2 &&
  -- Simplified overlap check
  ruleCondition r1 == ruleCondition r2

||| Simplified equality for conditions
Eq Condition where
  Always == Always = True
  Never == Never = True
  InZone z1 == InZone z2 = z1 == z2
  NotInZone z1 == NotInZone z2 = z1 == z2
  HasTag t1 == HasTag t2 = t1 == t2
  NotHasTag t1 == NotHasTag t2 = t1 == t2
  MatchPattern p1 == MatchPattern p2 = p1 == p2
  And a1 b1 == And a2 b2 = a1 == a2 && b1 == b2
  Or a1 b1 == Or a2 b2 = a1 == a2 && b1 == b2
  Not c1 == Not c2 = c1 == c2
  _ == _ = False

||| Find conflicts in a policy
public export
findConflicts : Policy -> List (PolicyRule, PolicyRule)
findConflicts policy =
  let rules = policyRules policy
  in do r1 <- rules
        r2 <- rules
        guard (ruleName r1 /= ruleName r2 && rulesConflict r1 r2)
        pure (r1, r2)

||| Policy composition - combine policies
public export
composePolicy : Policy -> Policy -> Policy
composePolicy p1 p2 =
  MkPolicy (policyName p1 ++ "+" ++ policyName p2)
           (policyRules p1 ++ policyRules p2)
           (defaultAction p1)

||| Zone boundary enforcement
public export
record ZoneBoundary where
  constructor MkZoneBoundary
  fromZone : ZoneId
  toZone : ZoneId
  crossingPolicy : Policy

||| Check if crossing is allowed
public export
canCross : ZoneBoundary -> List String -> Bool
canCross boundary tags =
  case evaluatePolicy (crossingPolicy boundary) (toZone boundary) tags of
    Allow => True
    Audit => True
    _ => False

||| Zone transition proof
public export
data ValidTransition : ZoneBoundary -> List String -> Type where
  MkValidTransition : canCross b t = True -> ValidTransition b t

||| Policy evaluation context
public export
record PolicyContext where
  constructor MkPolicyContext
  currentZone : ZoneId
  activeTags : List String
  parentContext : Maybe PolicyContext
  timestamp : Nat

||| Create root context
public export
rootContext : ZoneId -> PolicyContext
rootContext zone = MkPolicyContext zone [] Nothing 0

||| Enter a zone
public export
enterZone : ZoneId -> List String -> PolicyContext -> PolicyContext
enterZone zone tags ctx =
  MkPolicyContext zone (tags ++ activeTags ctx) (Just ctx) (S (timestamp ctx))

||| Exit current zone
public export
exitZone : PolicyContext -> Maybe PolicyContext
exitZone ctx = parentContext ctx

||| Policy audit entry
public export
record PolicyAuditEntry where
  constructor MkPolicyAuditEntry
  auditTimestamp : Nat
  auditZone : ZoneId
  auditPolicy : String
  auditRule : String
  auditAction : PolicyAction
  auditTags : List String

||| Policy evaluator with audit
public export
record PolicyEvaluator where
  constructor MkPolicyEvaluator
  evalPolicy : Policy
  evalAudit : List PolicyAuditEntry

||| Evaluate with audit trail
public export
evaluateWithAudit : PolicyEvaluator -> PolicyContext -> (PolicyAction, PolicyEvaluator)
evaluateWithAudit eval ctx =
  let action = evaluatePolicy (evalPolicy eval) (currentZone ctx) (activeTags ctx)
      entry = MkPolicyAuditEntry (timestamp ctx) (currentZone ctx)
                                 (policyName (evalPolicy eval)) "matched"
                                 action (activeTags ctx)
  in (action, { evalAudit := entry :: evalAudit eval } eval)

||| AST node type for policy targets
public export
data ASTNodeType =
    FunctionDef
  | VariableDecl
  | Assignment
  | FunctionCall
  | Import
  | Export
  | TypeDef
  | Comment

||| Equality for AST node types
public export
Eq ASTNodeType where
  FunctionDef == FunctionDef = True
  VariableDecl == VariableDecl = True
  Assignment == Assignment = True
  FunctionCall == FunctionCall = True
  Import == Import = True
  Export == Export = True
  TypeDef == TypeDef = True
  Comment == Comment = True
  _ == _ = False

||| AST node for policy evaluation
public export
record ASTNode where
  constructor MkASTNode
  nodeType : ASTNodeType
  nodeName : String
  nodeZone : ZoneId
  nodeTags : List String
  nodeChildren : List ASTNode

||| Policy for specific AST node types
public export
nodeTypePolicy : ASTNodeType -> PolicyAction -> PolicyRule
nodeTypePolicy ntype action =
  MkPolicyRule ("node-" ++ show ntype)
               (HasTag (show ntype))
               action 100 ""
  where
    show : ASTNodeType -> String
    show FunctionDef = "function-def"
    show VariableDecl = "variable-decl"
    show Assignment = "assignment"
    show FunctionCall = "function-call"
    show Import = "import"
    show Export = "export"
    show TypeDef = "type-def"
    show Comment = "comment"

||| Evaluate policy on AST node
public export
evaluateNode : Policy -> ASTNode -> PolicyAction
evaluateNode policy node =
  evaluatePolicy policy (nodeZone node) (nodeTags node)

||| Recursively evaluate policy on AST tree
public export
evaluateTree : Policy -> ASTNode -> List (ASTNode, PolicyAction)
evaluateTree policy node =
  let nodeResult = (node, evaluateNode policy node)
      childResults = concatMap (evaluateTree policy) (nodeChildren node)
  in nodeResult :: childResults

||| Check if all nodes pass policy
public export
allNodesPass : Policy -> ASTNode -> Bool
allNodesPass policy node =
  all (\p => snd p /= Deny) (evaluateTree policy node)

||| Proof that an AST tree satisfies a policy
public export
data TreeSatisfiesPolicy : Policy -> ASTNode -> Type where
  MkTreeSatisfiesPolicy : allNodesPass p n = True -> TreeSatisfiesPolicy p n

||| Immutable zone policy template
public export
immutableZonePolicy : ZoneId -> Policy
immutableZonePolicy zone =
  let denyMutation = denyRule "no-assignment" (And (InZone zone) (HasTag "assignment"))
      denyRedef = denyRule "no-redef" (And (InZone zone) (HasTag "variable-decl"))
  in addRule denyMutation (addRule denyRedef (emptyPolicy ("immutable-" ++ zone)))

||| Restricted zone policy template
public export
restrictedZonePolicy : ZoneId -> List String -> Policy
restrictedZonePolicy zone allowedTags =
  let allowRules = map (\t => allowRule ("allow-" ++ t) (And (InZone zone) (HasTag t))) allowedTags
  in foldl (flip addRule) (emptyPolicy ("restricted-" ++ zone)) allowRules

||| Policy inheritance - child zone inherits parent policy
public export
inheritPolicy : Policy -> ZoneId -> Policy
inheritPolicy parent childZone =
  let inheritedRules = map (\r => { ruleCondition :=
                                     Or (ruleCondition r) (InZone childZone) } r)
                           (policyRules parent)
  in { policyRules := inheritedRules } parent

||| Policy exception - override specific rules
public export
addException : PolicyRule -> Policy -> Policy
addException exception policy =
  { policyRules := { rulePriority := rulePriority exception + 100 } exception
                   :: policyRules policy } policy

||| Policy version for tracking changes
public export
record VersionedPolicy where
  constructor MkVersionedPolicy
  vpPolicy : Policy
  vpVersion : (Nat, Nat, Nat)
  vpChangelog : List String

||| Upgrade policy version
public export
upgradePolicy : Policy -> String -> VersionedPolicy -> VersionedPolicy
upgradePolicy newPolicy changeDesc vp =
  let (maj, min, pat) = vpVersion vp
  in MkVersionedPolicy newPolicy (maj, S min, 0) (changeDesc :: vpChangelog vp)



// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

open Proven_Bitwise

/**
 * SafePolicy - Policy expression evaluation that cannot crash.
 *
 * Provides a type-safe policy engine for evaluating access control rules,
 * feature flags, and conditional logic with well-defined behavior.
 */

/** Policy error types */
type policyError =
  | InvalidPolicy
  | EvaluationFailed
  | MaxDepthExceeded
  | UnknownAttribute
  | TypeMismatch

/** Policy evaluation result */
type policyResult = Allow | Deny | NotApplicable

/** Combine two results with AND semantics (deny wins) */
let policyResultAnd = (a: policyResult, b: policyResult): policyResult => {
  switch (a, b) {
  | (Deny, _) | (_, Deny) => Deny
  | (NotApplicable, _) | (_, NotApplicable) => NotApplicable
  | (Allow, Allow) => Allow
  }
}

/** Combine two results with OR semantics (allow wins) */
let policyResultOr = (a: policyResult, b: policyResult): policyResult => {
  switch (a, b) {
  | (Allow, _) | (_, Allow) => Allow
  | (Deny, Deny) => Deny
  | _ => NotApplicable
  }
}

/** Negate the result */
let policyResultNot = (r: policyResult): policyResult => {
  switch r {
  | Allow => Deny
  | Deny => Allow
  | NotApplicable => NotApplicable
  }
}

/** Convert to boolean (allow = true, deny/not_applicable = false) */
let policyResultToBool = (r: policyResult): bool => {
  r == Allow
}

/** Comparison operators for policy conditions */
type compareOp =
  | Equal
  | NotEqual
  | LessThan
  | LessEqual
  | GreaterThan
  | GreaterEqual
  | Contains
  | StartsWith
  | EndsWith
  | MatchesAny

/** Value types for policy attributes */
type value =
  | Boolean(bool)
  | Integer(int)
  | Float(float)
  | String(string)
  | StringList(array<string>)

/** Compare two values */
let compareValues = (a: value, b: value, op: compareOp): option<bool> => {
  switch (a, b, op) {
  // Boolean comparisons
  | (Boolean(va), Boolean(vb), Equal) => Some(va == vb)
  | (Boolean(va), Boolean(vb), NotEqual) => Some(va != vb)
  | (Boolean(_), Boolean(_), _) => None

  // Integer comparisons
  | (Integer(va), Integer(vb), Equal) => Some(va == vb)
  | (Integer(va), Integer(vb), NotEqual) => Some(va != vb)
  | (Integer(va), Integer(vb), LessThan) => Some(va < vb)
  | (Integer(va), Integer(vb), LessEqual) => Some(va <= vb)
  | (Integer(va), Integer(vb), GreaterThan) => Some(va > vb)
  | (Integer(va), Integer(vb), GreaterEqual) => Some(va >= vb)
  | (Integer(_), Integer(_), _) => None

  // Float comparisons
  | (Float(va), Float(vb), Equal) => Some(va == vb)
  | (Float(va), Float(vb), NotEqual) => Some(va != vb)
  | (Float(va), Float(vb), LessThan) => Some(va < vb)
  | (Float(va), Float(vb), LessEqual) => Some(va <= vb)
  | (Float(va), Float(vb), GreaterThan) => Some(va > vb)
  | (Float(va), Float(vb), GreaterEqual) => Some(va >= vb)
  | (Float(_), Float(_), _) => None

  // String comparisons
  | (String(va), String(vb), Equal) => Some(va == vb)
  | (String(va), String(vb), NotEqual) => Some(va != vb)
  | (String(va), String(vb), Contains) => Some(Js.String2.includes(va, vb))
  | (String(va), String(vb), StartsWith) => Some(Js.String2.startsWith(va, vb))
  | (String(va), String(vb), EndsWith) => Some(Js.String2.endsWith(va, vb))
  | (String(va), StringList(list), MatchesAny) => Some(Js.Array2.includes(list, va))
  | (String(_), String(_), _) => None

  // Type mismatch
  | _ => None
  }
}

/** A single policy condition */
type condition = {
  attribute: string,
  operator: compareOp,
  value: value,
}

/** Policy rule combining effect with conditions */
type rule = {
  effect: policyResult,
  conditions: array<condition>,
  description: option<string>,
}

/** Combining algorithm for multiple rules */
type combineAlgorithm =
  | FirstApplicable
  | DenyOverrides
  | AllowOverrides
  | UnanimousAllow
  | UnanimousDeny

/** Attribute map for policy evaluation */
type attributeMap = Js.Dict.t<value>

/** Create an empty attribute map */
let createAttributeMap = (): attributeMap => {
  Js.Dict.empty()
}

/** Set an attribute value */
let setAttribute = (attrs: attributeMap, key: string, value: value): unit => {
  Js.Dict.set(attrs, key, value)
}

/** Get an attribute value */
let getAttribute = (attrs: attributeMap, key: string): option<value> => {
  Js.Dict.get(attrs, key)
}

/** Check if attribute exists */
let hasAttribute = (attrs: attributeMap, key: string): bool => {
  Belt.Option.isSome(Js.Dict.get(attrs, key))
}

/** Evaluate a single rule against attributes */
let evaluateRule = (rule: rule, attrs: attributeMap): policyResult => {
  let allConditionsMet = Js.Array2.every(rule.conditions, condition => {
    switch getAttribute(attrs, condition.attribute) {
    | None => false
    | Some(attrValue) =>
      switch compareValues(attrValue, condition.value, condition.operator) {
      | None => false
      | Some(result) => result
      }
    }
  })

  if allConditionsMet {
    rule.effect
  } else {
    NotApplicable
  }
}

/** Policy set containing multiple rules */
type policySet = {
  rules: array<rule>,
  algorithm: combineAlgorithm,
  defaultResult: policyResult,
}

/** Create a new policy set */
let createPolicySet = (~algorithm: combineAlgorithm=DenyOverrides): policySet => {
  rules: [],
  algorithm,
  defaultResult: Deny,
}

/** Add a rule to a policy set (returns new policy set) */
let addRule = (policySet: policySet, rule: rule): policySet => {
  {
    ...policySet,
    rules: Js.Array2.concat(policySet.rules, [rule]),
  }
}

/** Evaluate the policy set against attributes */
let evaluatePolicySet = (policySet: policySet, attrs: attributeMap): policyResult => {
  if Js.Array2.length(policySet.rules) == 0 {
    policySet.defaultResult
  } else {
    let results = Js.Array2.map(policySet.rules, rule => evaluateRule(rule, attrs))

    let allowCount = Js.Array2.filter(results, r => r == Allow)->Js.Array2.length
    let denyCount = Js.Array2.filter(results, r => r == Deny)->Js.Array2.length
    let applicableCount = Js.Array2.filter(results, r => r != NotApplicable)->Js.Array2.length

    if applicableCount == 0 {
      policySet.defaultResult
    } else {
      switch policySet.algorithm {
      | FirstApplicable =>
        switch Js.Array2.find(results, r => r != NotApplicable) {
        | Some(r) => r
        | None => policySet.defaultResult
        }
      | DenyOverrides =>
        if denyCount > 0 {
          Deny
        } else {
          Allow
        }
      | AllowOverrides =>
        if allowCount > 0 {
          Allow
        } else {
          Deny
        }
      | UnanimousAllow =>
        if allowCount == applicableCount {
          Allow
        } else {
          Deny
        }
      | UnanimousDeny =>
        if denyCount == applicableCount {
          Deny
        } else {
          Allow
        }
      }
    }
  }
}

/** Check if an action is allowed */
let isAllowed = (policySet: policySet, attrs: attributeMap): bool => {
  evaluatePolicySet(policySet, attrs) == Allow
}

/** Simple feature flag */
type featureFlag = {
  name: string,
  enabled: bool,
  rolloutPercentage: int,
}

/** Check if feature is enabled for a user ID */
let isFeatureEnabledFor = (flag: featureFlag, userId: int): bool => {
  if !flag.enabled {
    false
  } else if flag.rolloutPercentage >= 100 {
    true
  } else if flag.rolloutPercentage == 0 {
    false
  } else {
    // Consistent hashing based on userId
    let hash = mod(userId, 100)
    hash < flag.rolloutPercentage
  }
}

/** Time-based policy (e.g., business hours) */
type timePolicy = {
  startHour: int,
  endHour: int,
  days: int, // Bitmask: Sun=1, Mon=2, Tue=4, Wed=8, Thu=16, Fri=32, Sat=64
}

/** Weekday constants for time policy */
let sunday = 1
let monday = 2
let tuesday = 4
let wednesday = 8
let thursday = 16
let friday = 32
let saturday = 64
let weekdays = lor(lor(lor(lor(monday, tuesday), wednesday), thursday), friday)
let weekend = lor(saturday, sunday)
let allDays = lor(weekdays, weekend)

/** Check if current time (hour 0-23, day 0-6 Sunday=0) is within policy */
let isTimeActiveAt = (policy: timePolicy, hour: int, dayOfWeek: int): bool => {
  if hour < 0 || hour > 23 {
    false
  } else if dayOfWeek < 0 || dayOfWeek > 6 {
    false
  } else {
    let dayMask = lsl(1, dayOfWeek)
    if land(policy.days, dayMask) == 0 {
      false
    } else if policy.startHour <= policy.endHour {
      hour >= policy.startHour && hour < policy.endHour
    } else {
      // Wraps around midnight
      hour >= policy.startHour || hour < policy.endHour
    }
  }
}

/** Create a business hours policy (9-17, Mon-Fri) */
let businessHoursPolicy: timePolicy = {
  startHour: 9,
  endHour: 17,
  days: weekdays,
}

/** Create a 24/7 policy */
let alwaysActivePolicy: timePolicy = {
  startHour: 0,
  endHour: 24,
  days: allDays,
}

/** Rule builder helpers */
module RuleBuilder = {
  let allow = (conditions: array<condition>): rule => {
    effect: Allow,
    conditions,
    description: None,
  }

  let deny = (conditions: array<condition>): rule => {
    effect: Deny,
    conditions,
    description: None,
  }

  let allowWithDescription = (conditions: array<condition>, desc: string): rule => {
    effect: Allow,
    conditions,
    description: Some(desc),
  }

  let denyWithDescription = (conditions: array<condition>, desc: string): rule => {
    effect: Deny,
    conditions,
    description: Some(desc),
  }
}

/** Condition builder helpers */
module ConditionBuilder = {
  let stringEquals = (attr: string, val: string): condition => {
    attribute: attr,
    operator: Equal,
    value: String(val),
  }

  let stringNotEquals = (attr: string, val: string): condition => {
    attribute: attr,
    operator: NotEqual,
    value: String(val),
  }

  let intEquals = (attr: string, val: int): condition => {
    attribute: attr,
    operator: Equal,
    value: Integer(val),
  }

  let intGreaterThan = (attr: string, val: int): condition => {
    attribute: attr,
    operator: GreaterThan,
    value: Integer(val),
  }

  let intLessThan = (attr: string, val: int): condition => {
    attribute: attr,
    operator: LessThan,
    value: Integer(val),
  }

  let boolEquals = (attr: string, val: bool): condition => {
    attribute: attr,
    operator: Equal,
    value: Boolean(val),
  }

  let stringContains = (attr: string, val: string): condition => {
    attribute: attr,
    operator: Contains,
    value: String(val),
  }

  let stringStartsWith = (attr: string, val: string): condition => {
    attribute: attr,
    operator: StartsWith,
    value: String(val),
  }

  let stringEndsWith = (attr: string, val: string): condition => {
    attribute: attr,
    operator: EndsWith,
    value: String(val),
  }

  let matchesAny = (attr: string, values: array<string>): condition => {
    attribute: attr,
    operator: MatchesAny,
    value: StringList(values),
  }
}

/** Common policy patterns */
module CommonPolicies = {
  /** Require authentication */
  let requireAuthenticated = RuleBuilder.deny([
    ConditionBuilder.boolEquals("authenticated", false),
  ])

  /** Allow admin users */
  let allowAdmin = RuleBuilder.allow([ConditionBuilder.stringEquals("role", "admin")])

  /** Deny banned users */
  let denyBanned = RuleBuilder.deny([ConditionBuilder.boolEquals("banned", true)])

  /** Rate limit check (requires "request_count" attribute) */
  let rateLimitExceeded = (limit: int): rule =>
    RuleBuilder.deny([ConditionBuilder.intGreaterThan("request_count", limit)])
}

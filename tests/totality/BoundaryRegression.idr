-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
module BoundaryRegression

import Proven.Core
import Proven.SafeTree
import Proven.SafeUnionFind
import Proven.SafeArgs.Types
import Proven.SafeArgs.Parser
import Proven.SafeRegex.Types
import Proven.SafeRegex.Matcher
import Proven.SafeXML.Parser
import Proven.SafeXML.Types
import Proven.SafeYAML.Parser
import Proven.SafeYAML.Types
import SafeMathProps
import Data.List
import Data.String
import System

%default total

check : String -> Bool -> IO ()
check label True = putStrLn ("PASS: " ++ label)
check label False = do
  putStrLn ("FAIL: " ++ label)
  exitFailure

tree : BinaryTree Nat
tree = Node 1 (Node 2 (Node 4 Leaf Leaf) Leaf)
              (Node 3 (Node 5 Leaf Leaf) (Node 6 Leaf Leaf))

nestedXML : Nat -> String
nestedXML Z = "text"
nestedXML (S n) = "<a>" ++ nestedXML n ++ "</a>"

nestedYAML : Nat -> String
nestedYAML Z = "true"
nestedYAML (S n) = "[" ++ nestedYAML n ++ "]"

xmlSiblings : Nat -> String
xmlSiblings Z = ""
xmlSiblings (S n) = "<a/>" ++ xmlSiblings n
quotedItems : Nat -> String
quotedItems Z = "\"x\""
quotedItems (S n) = "\"x\"," ++ quotedItems n

main : IO ()
main = do
  check "tree order including asymmetric levels" (levelorder tree == [1,2,3,4,5,6])
  check "empty tree" (levelorder (the (BinaryTree Nat) Leaf) == [])
  check "bundles resume outer argument loop"
    (case parseArgs defaultParserOptions
            [flag "alpha" 'a' "", flag "beta" 'b' "", option "value" 'v' ""]
            ["-ab", "--value", "payload", "--", "-ignored"] of
       Ok parsed => getFlag parsed "alpha" && getFlag parsed "beta" &&
                    getOption parsed "value" == Just "payload" &&
                    getRest parsed == ["-ignored"]
       Err _ => False)
  check "missing option value refused"
    (isErr (parseArgs defaultParserOptions [option "value" 'v' ""] ["-v"]))
  let state = initState "aλ" 100 defaultFlags
  check "regex character and end lookup"
    (currentChar state == Just 'a' && charAt state 1 == Just 'λ' &&
     charAt state 2 == Nothing)
  check "forged length cannot reach a partial string primitive"
    (charAt ({ inputLen := 100 } state) 50 == Nothing)
  check "XML nested and sibling/attribute paths"
    (isOk (parseXML (nestedXML 20)) &&
     isOk (parseXML "<r a='v'><a/><b>text</b><!--ok--><![CDATA[x]]></r>"))
  check "XML flat elements and exact attribute limits"
    (isOk (parseXML ("<r>" ++ xmlSiblings 128 ++ "</r>")) &&
     isOk (parseXMLWith ({ maxAttributesPerElement := 0 } Proven.SafeXML.Types.secureDefaults) "<r/>" ) &&
     isErr (parseXMLWith ({ maxAttributesPerElement := 0 } Proven.SafeXML.Types.secureDefaults) "<r a='x'/>" ) &&
     isOk (parseXMLWith ({ maxAttributesPerElement := 1 } Proven.SafeXML.Types.secureDefaults) "<r a='x'/>" ) &&
     isErr (parseXMLWith ({ maxAttributesPerElement := 1 } Proven.SafeXML.Types.secureDefaults) "<r a='x' b='y'/>" ))
  check "XML malformed and security refusals"
    (isErr (parseXML "<a></b>") && isErr (parseXML "<a>") &&
     isErr (parseXML "<!DOCTYPE r SYSTEM 'external'><r/>"))
  check "XML depth limit remains effective" (isErr (parseXML (nestedXML 105)))
  check "YAML flow collections and quoted escape"
    (isOk (parseYAML (nestedYAML 20)) &&
     isOk (parseYAML "{\"key\": [true, false, \"a\\n\"]}"))
  check "YAML flat quoted sequence"
    (isOk (parseYAML ("[" ++ quotedItems 128 ++ "]")))
  check "YAML malformed/forbidden-anchor refusals"
    (isErr (parseYAML "[true") && isErr (parseYAML "\"unclosed") &&
     isErr (parseYAML "&a true"))
  check "YAML depth limit remains effective" (isErr (parseYAML (nestedYAML 55)))
  check "union-find duplicate roots preserve first-occurrence set order"
    (allSets (MkUF [Parent 1, Root 1, Parent 1, Root 0] 2) == [[0,1,2],[3]] &&
     allSets (Proven.SafeUnionFind.create 3) == [[0],[1],[2]])
  runMathProperties
  putStrLn "Finite regression execution passed; parser completeness/equivalence is not claimed."

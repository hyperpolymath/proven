-- SPDX-License-Identifier: MPL-2.0
module PathProofRegressions
import Proven.SafePath.ByteKernel
%default total

public export
actualWire : (bytes : List Bits8) ->
  pathFinish (scanBytes 0 bytes) = boolCode (detect bytes)
actualWire = wireRefinesDetection

public export
surroundedParent : (before, after : List ByteClass) ->
  scanTokens Start (before ++ (Boundary :: Dot :: Dot :: Boundary :: after)) = Found
surroundedParent = parentInContext

public export
windowsParent : detect [97, 92, 46, 46, 92, 98] = True
windowsParent = Refl

public export
nulParent : detect [46, 46, 0, 97] = True
nulParent = Refl

public export
embeddedDots : detect [97, 46, 46, 98] = False
embeddedDots = Refl

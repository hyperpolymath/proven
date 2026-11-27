-- SPDX-License-Identifier: MPL-2.0
module MalformedPathState
import Proven.SafePath.ByteKernel
%default total
forged : pathFinish 255 = 0
forged = Refl

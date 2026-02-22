# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
SafeML - Numerically stable machine learning primitives.

Provides safe implementations of softmax, loss functions, and activations.
All computation is delegated to the Idris core via FFI.
"""

import ctypes
from typing import List, Optional

from .core import ProvenStatus, get_lib


def _to_c_array(data: List[float]) -> ctypes.Array:
    """Convert Python list to ctypes double array."""
    arr = (ctypes.c_double * len(data))()
    for i, v in enumerate(data):
        arr[i] = v
    return arr


def _from_c_array(arr: ctypes.Array, size: int) -> List[float]:
    """Convert ctypes double array to Python list."""
    return [arr[i] for i in range(size)]


class SafeML:
    """Numerically stable ML operations via FFI."""

    @staticmethod
    def softmax(logits: List[float]) -> List[float]:
        """
        Numerically stable softmax via FFI.

        Uses the log-sum-exp trick to prevent overflow.

        Args:
            logits: Input logits

        Returns:
            Softmax probabilities (sum to 1)

        Example:
            >>> SafeML.softmax([1.0, 2.0, 3.0])
            [0.0900..., 0.2447..., 0.6652...]
        """
        if not logits:
            return []

        lib = get_lib()
        inp = _to_c_array(logits)
        out = (ctypes.c_double * len(logits))()
        status = lib.proven_ml_softmax(inp, len(logits), out)
        if status != ProvenStatus.OK:
            # Fallback: uniform distribution
            return [1.0 / len(logits)] * len(logits)
        return _from_c_array(out, len(logits))

    @staticmethod
    def log_softmax(logits: List[float]) -> List[float]:
        """
        Numerically stable log-softmax via FFI.

        Args:
            logits: Input logits

        Returns:
            Log of softmax probabilities
        """
        probs = SafeML.softmax(logits)
        if not probs:
            return []
        # Use FFI log for each element
        lib = get_lib()
        result = []
        for p in probs:
            r = lib.proven_float_log(p)
            if r.status == ProvenStatus.OK:
                result.append(r.value)
            else:
                result.append(float("-inf"))
        return result

    @staticmethod
    def sigmoid(x: float) -> float:
        """
        Numerically stable sigmoid via FFI.

        Args:
            x: Input value

        Returns:
            Sigmoid value in (0, 1)
        """
        lib = get_lib()
        result = lib.proven_ml_sigmoid(x)
        if result.status != ProvenStatus.OK:
            return 0.5
        return result.value

    @staticmethod
    def relu(x: float) -> float:
        """
        ReLU activation via FFI.

        Args:
            x: Input value

        Returns:
            max(0, x)
        """
        lib = get_lib()
        result = lib.proven_ml_relu(x)
        if result.status != ProvenStatus.OK:
            return max(0.0, x)
        return result.value

    @staticmethod
    def leaky_relu(x: float, alpha: float = 0.01) -> float:
        """
        Leaky ReLU activation.

        Args:
            x: Input value
            alpha: Leak coefficient

        Returns:
            x if x > 0 else alpha * x
        """
        # Use relu FFI for the max(0, x) part, compute leaky variant
        if x > 0:
            return x
        lib = get_lib()
        result = lib.proven_float_mul(alpha, x)
        if result.status != ProvenStatus.OK:
            return alpha * x
        return result.value

    @staticmethod
    def tanh(x: float) -> float:
        """
        Hyperbolic tangent via FFI.

        Args:
            x: Input value

        Returns:
            tanh(x) in (-1, 1)
        """
        lib = get_lib()
        result = lib.proven_ml_tanh(x)
        if result.status != ProvenStatus.OK:
            return 0.0
        return result.value

    @staticmethod
    def gelu(x: float) -> float:
        """
        Gaussian Error Linear Unit via FFI.

        Approximate GELU using tanh.

        Args:
            x: Input value

        Returns:
            GELU(x)
        """
        lib = get_lib()
        # GELU(x) = 0.5 * x * (1 + tanh(sqrt(2/pi) * (x + 0.044715 * x^3)))
        tanh_result = lib.proven_ml_tanh(x)
        if tanh_result.status != ProvenStatus.OK:
            return 0.0

        # Compute via FFI float ops
        x3 = lib.proven_float_pow(x, 3.0)
        if x3.status != ProvenStatus.OK:
            return 0.0
        inner = lib.proven_float_add(x, lib.proven_float_mul(0.044715, x3.value).value
                                     if lib.proven_float_mul(0.044715, x3.value).status == ProvenStatus.OK
                                     else 0.0)
        if inner.status != ProvenStatus.OK:
            return 0.0
        # sqrt(2/pi) ~ 0.7978845608
        scaled = lib.proven_float_mul(0.7978845608028654, inner.value)
        if scaled.status != ProvenStatus.OK:
            return 0.0
        tanh_val = lib.proven_ml_tanh(scaled.value)
        if tanh_val.status != ProvenStatus.OK:
            return 0.0
        result = lib.proven_float_mul(0.5 * x, 1.0 + tanh_val.value)
        if result.status != ProvenStatus.OK:
            return 0.0
        return result.value

    @staticmethod
    def cross_entropy_loss(predictions: List[float], target: int) -> Optional[float]:
        """
        Cross-entropy loss for single sample via FFI.

        Args:
            predictions: Predicted probabilities (should sum to 1)
            target: True class index

        Returns:
            Loss value, or None if invalid
        """
        if not predictions or target < 0 or target >= len(predictions):
            return None

        lib = get_lib()
        inp = _to_c_array(predictions)
        result = lib.proven_ml_cross_entropy(inp, len(predictions), target)
        if result.status != ProvenStatus.OK:
            return None
        return result.value

    @staticmethod
    def binary_cross_entropy_loss(prediction: float, target: float) -> Optional[float]:
        """
        Binary cross-entropy loss via FFI.

        Args:
            prediction: Predicted probability (0-1)
            target: True label (0 or 1)

        Returns:
            Loss value, or None if invalid
        """
        # Delegate to cross_entropy with 2-class encoding
        if target == 1.0:
            preds = [1.0 - prediction, prediction]
            return SafeML.cross_entropy_loss(preds, 1)
        else:
            preds = [1.0 - prediction, prediction]
            return SafeML.cross_entropy_loss(preds, 0)

    @staticmethod
    def mse_loss(predictions: List[float], targets: List[float]) -> Optional[float]:
        """
        Mean squared error loss via FFI.

        Args:
            predictions: Predicted values
            targets: True values

        Returns:
            MSE loss, or None if lengths don't match
        """
        if len(predictions) != len(targets) or not predictions:
            return None

        lib = get_lib()
        p = _to_c_array(predictions)
        t = _to_c_array(targets)
        result = lib.proven_ml_mse(p, len(predictions), t, len(targets))
        if result.status != ProvenStatus.OK:
            return None
        return result.value

    @staticmethod
    def mae_loss(predictions: List[float], targets: List[float]) -> Optional[float]:
        """
        Mean absolute error loss via FFI.

        Args:
            predictions: Predicted values
            targets: True values

        Returns:
            MAE loss, or None if lengths don't match
        """
        if len(predictions) != len(targets) or not predictions:
            return None

        # Compute using FFI float ops
        lib = get_lib()
        total = 0.0
        for p_val, t_val in zip(predictions, targets):
            diff = lib.proven_float_sub(p_val, t_val)
            if diff.status != ProvenStatus.OK:
                return None
            abs_diff = abs(diff.value)
            add_result = lib.proven_float_add(total, abs_diff)
            if add_result.status != ProvenStatus.OK:
                return None
            total = add_result.value
        div_result = lib.proven_float_div(total, float(len(predictions)))
        if div_result.status != ProvenStatus.OK:
            return None
        return div_result.value

    @staticmethod
    def huber_loss(predictions: List[float], targets: List[float], delta: float = 1.0) -> Optional[float]:
        """
        Huber loss (smooth L1) via FFI.

        Args:
            predictions: Predicted values
            targets: True values
            delta: Threshold for quadratic vs linear

        Returns:
            Huber loss, or None if lengths don't match
        """
        if len(predictions) != len(targets) or not predictions:
            return None

        lib = get_lib()
        total = 0.0
        for p_val, t_val in zip(predictions, targets):
            diff = lib.proven_float_sub(p_val, t_val)
            if diff.status != ProvenStatus.OK:
                return None
            abs_diff = abs(diff.value)
            if abs_diff <= delta:
                contrib = lib.proven_float_mul(0.5, abs_diff * abs_diff)
            else:
                contrib = lib.proven_float_sub(
                    lib.proven_float_mul(delta, abs_diff).value
                    if lib.proven_float_mul(delta, abs_diff).status == ProvenStatus.OK else 0.0,
                    0.5 * delta
                )
            if hasattr(contrib, 'status'):
                if contrib.status != ProvenStatus.OK:
                    return None
                total += contrib.value
            else:
                total += contrib if isinstance(contrib, float) else 0.0

        div_result = lib.proven_float_div(total, float(len(predictions)))
        if div_result.status != ProvenStatus.OK:
            return None
        return div_result.value

    @staticmethod
    def l2_regularization(weights: List[float], lambda_: float = 0.01) -> float:
        """
        L2 (Ridge) regularization term via FFI.

        Args:
            weights: Model weights
            lambda_: Regularization strength

        Returns:
            Regularization term
        """
        lib = get_lib()
        total = 0.0
        for w in weights:
            sq = lib.proven_float_mul(w, w)
            if sq.status == ProvenStatus.OK:
                total += sq.value
        result = lib.proven_float_mul(0.5 * lambda_, total)
        if result.status != ProvenStatus.OK:
            return 0.0
        return result.value

    @staticmethod
    def l1_regularization(weights: List[float], lambda_: float = 0.01) -> float:
        """
        L1 (Lasso) regularization term via FFI.

        Args:
            weights: Model weights
            lambda_: Regularization strength

        Returns:
            Regularization term
        """
        lib = get_lib()
        total = 0.0
        for w in weights:
            total += abs(w)
        result = lib.proven_float_mul(lambda_, total)
        if result.status != ProvenStatus.OK:
            return 0.0
        return result.value

    @staticmethod
    def accuracy(predictions: List[int], targets: List[int]) -> Optional[float]:
        """
        Classification accuracy via FFI.

        Args:
            predictions: Predicted class indices
            targets: True class indices

        Returns:
            Accuracy (0-1), or None if invalid
        """
        if len(predictions) != len(targets) or not predictions:
            return None

        correct = sum(1 for p, t in zip(predictions, targets) if p == t)
        lib = get_lib()
        result = lib.proven_float_div(float(correct), float(len(predictions)))
        if result.status != ProvenStatus.OK:
            return None
        return result.value

    @staticmethod
    def argmax(values: List[float]) -> int:
        """
        Index of maximum value.

        Args:
            values: List of values

        Returns:
            Index of max, or -1 if empty
        """
        if not values:
            return -1
        max_idx = 0
        max_val = values[0]
        for i in range(1, len(values)):
            if values[i] > max_val:
                max_val = values[i]
                max_idx = i
        return max_idx

    @staticmethod
    def batch_norm(values: List[float], epsilon: float = 1e-5) -> List[float]:
        """
        Batch normalization (normalize to mean=0, std=1) via FFI.

        Args:
            values: Input values
            epsilon: Small constant for numerical stability

        Returns:
            Normalized values
        """
        if len(values) < 2:
            return values

        lib = get_lib()
        inp = _to_c_array(values)
        out = (ctypes.c_double * len(values))()
        status = lib.proven_ml_batch_norm(inp, len(values), epsilon, out)
        if status != ProvenStatus.OK:
            return list(values)
        return _from_c_array(out, len(values))

    @staticmethod
    def layer_norm(values: List[float], epsilon: float = 1e-5) -> List[float]:
        """
        Layer normalization via FFI.

        Args:
            values: Input values
            epsilon: Small constant for numerical stability

        Returns:
            Normalized values
        """
        return SafeML.batch_norm(values, epsilon)

    @staticmethod
    def clip_gradients(gradients: List[float], max_norm: float) -> List[float]:
        """
        Gradient clipping by norm via FFI.

        Args:
            gradients: Gradient values
            max_norm: Maximum L2 norm

        Returns:
            Clipped gradients
        """
        if not gradients:
            return []

        lib = get_lib()
        # Compute norm via FFI
        inp = _to_c_array(gradients)
        mag_result = lib.proven_tensor_magnitude(inp, len(gradients))
        if mag_result.status != ProvenStatus.OK:
            return list(gradients)

        norm = mag_result.value
        if norm <= max_norm:
            return list(gradients)

        scale_result = lib.proven_float_div(max_norm, norm)
        if scale_result.status != ProvenStatus.OK:
            return list(gradients)

        out = (ctypes.c_double * len(gradients))()
        status = lib.proven_tensor_scale(inp, len(gradients), scale_result.value, out)
        if status != ProvenStatus.OK:
            return list(gradients)
        return _from_c_array(out, len(gradients))

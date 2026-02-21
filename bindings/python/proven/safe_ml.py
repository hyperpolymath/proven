# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafeML - Numerically stable machine learning primitives.

Provides safe implementations of softmax, loss functions, and activations.
"""

from typing import List, Optional
import math


class SafeML:
    """Numerically stable ML operations."""

    @staticmethod
    def softmax(logits: List[float]) -> List[float]:
        """
        Numerically stable softmax.

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

        # Subtract max for numerical stability
        max_val = max(logits)
        exp_vals = [math.exp(x - max_val) for x in logits]
        sum_exp = sum(exp_vals)

        if sum_exp == 0:
            return [1.0 / len(logits)] * len(logits)

        return [e / sum_exp for e in exp_vals]

    @staticmethod
    def log_softmax(logits: List[float]) -> List[float]:
        """
        Numerically stable log-softmax.

        Args:
            logits: Input logits

        Returns:
            Log of softmax probabilities
        """
        if not logits:
            return []

        max_val = max(logits)
        shifted = [x - max_val for x in logits]
        log_sum_exp = math.log(sum(math.exp(x) for x in shifted))

        return [x - log_sum_exp for x in shifted]

    @staticmethod
    def sigmoid(x: float) -> float:
        """
        Numerically stable sigmoid.

        Args:
            x: Input value

        Returns:
            Sigmoid value in (0, 1)
        """
        if x >= 0:
            z = math.exp(-x)
            return 1.0 / (1.0 + z)
        else:
            z = math.exp(x)
            return z / (1.0 + z)

    @staticmethod
    def relu(x: float) -> float:
        """
        ReLU activation.

        Args:
            x: Input value

        Returns:
            max(0, x)
        """
        return max(0.0, x)

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
        return x if x > 0 else alpha * x

    @staticmethod
    def tanh(x: float) -> float:
        """
        Hyperbolic tangent.

        Args:
            x: Input value

        Returns:
            tanh(x) in (-1, 1)
        """
        return math.tanh(x)

    @staticmethod
    def gelu(x: float) -> float:
        """
        Gaussian Error Linear Unit.

        Approximate GELU using tanh.

        Args:
            x: Input value

        Returns:
            GELU(x)
        """
        return 0.5 * x * (1 + math.tanh(math.sqrt(2 / math.pi) * (x + 0.044715 * x**3)))

    @staticmethod
    def cross_entropy_loss(predictions: List[float], target: int) -> Optional[float]:
        """
        Cross-entropy loss for single sample.

        Args:
            predictions: Predicted probabilities (should sum to 1)
            target: True class index

        Returns:
            Loss value, or None if invalid
        """
        if not predictions or target < 0 or target >= len(predictions):
            return None

        prob = predictions[target]
        if prob <= 0:
            return float('inf')  # Or a large number

        return -math.log(prob)

    @staticmethod
    def binary_cross_entropy_loss(prediction: float, target: float) -> Optional[float]:
        """
        Binary cross-entropy loss.

        Args:
            prediction: Predicted probability (0-1)
            target: True label (0 or 1)

        Returns:
            Loss value, or None if invalid
        """
        eps = 1e-15
        prediction = max(eps, min(1 - eps, prediction))

        return -(target * math.log(prediction) + (1 - target) * math.log(1 - prediction))

    @staticmethod
    def mse_loss(predictions: List[float], targets: List[float]) -> Optional[float]:
        """
        Mean squared error loss.

        Args:
            predictions: Predicted values
            targets: True values

        Returns:
            MSE loss, or None if lengths don't match
        """
        if len(predictions) != len(targets) or not predictions:
            return None

        return sum((p - t) ** 2 for p, t in zip(predictions, targets)) / len(predictions)

    @staticmethod
    def mae_loss(predictions: List[float], targets: List[float]) -> Optional[float]:
        """
        Mean absolute error loss.

        Args:
            predictions: Predicted values
            targets: True values

        Returns:
            MAE loss, or None if lengths don't match
        """
        if len(predictions) != len(targets) or not predictions:
            return None

        return sum(abs(p - t) for p, t in zip(predictions, targets)) / len(predictions)

    @staticmethod
    def huber_loss(predictions: List[float], targets: List[float], delta: float = 1.0) -> Optional[float]:
        """
        Huber loss (smooth L1).

        Args:
            predictions: Predicted values
            targets: True values
            delta: Threshold for quadratic vs linear

        Returns:
            Huber loss, or None if lengths don't match
        """
        if len(predictions) != len(targets) or not predictions:
            return None

        total = 0.0
        for p, t in zip(predictions, targets):
            diff = abs(p - t)
            if diff <= delta:
                total += 0.5 * diff ** 2
            else:
                total += delta * (diff - 0.5 * delta)

        return total / len(predictions)

    @staticmethod
    def l2_regularization(weights: List[float], lambda_: float = 0.01) -> float:
        """
        L2 (Ridge) regularization term.

        Args:
            weights: Model weights
            lambda_: Regularization strength

        Returns:
            Regularization term
        """
        return 0.5 * lambda_ * sum(w ** 2 for w in weights)

    @staticmethod
    def l1_regularization(weights: List[float], lambda_: float = 0.01) -> float:
        """
        L1 (Lasso) regularization term.

        Args:
            weights: Model weights
            lambda_: Regularization strength

        Returns:
            Regularization term
        """
        return lambda_ * sum(abs(w) for w in weights)

    @staticmethod
    def accuracy(predictions: List[int], targets: List[int]) -> Optional[float]:
        """
        Classification accuracy.

        Args:
            predictions: Predicted class indices
            targets: True class indices

        Returns:
            Accuracy (0-1), or None if invalid
        """
        if len(predictions) != len(targets) or not predictions:
            return None

        correct = sum(1 for p, t in zip(predictions, targets) if p == t)
        return correct / len(predictions)

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
        return max(range(len(values)), key=lambda i: values[i])

    @staticmethod
    def batch_norm(values: List[float], epsilon: float = 1e-5) -> List[float]:
        """
        Batch normalization (normalize to mean=0, std=1).

        Args:
            values: Input values
            epsilon: Small constant for numerical stability

        Returns:
            Normalized values
        """
        if len(values) < 2:
            return values

        mean = sum(values) / len(values)
        var = sum((x - mean) ** 2 for x in values) / len(values)

        return [(x - mean) / math.sqrt(var + epsilon) for x in values]

    @staticmethod
    def layer_norm(values: List[float], epsilon: float = 1e-5) -> List[float]:
        """
        Layer normalization.

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
        Gradient clipping by norm.

        Args:
            gradients: Gradient values
            max_norm: Maximum L2 norm

        Returns:
            Clipped gradients
        """
        if not gradients:
            return []

        norm = math.sqrt(sum(g ** 2 for g in gradients))
        if norm <= max_norm:
            return gradients

        scale = max_norm / norm
        return [g * scale for g in gradients]

// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

import kotlin.math.sqrt

/**
 * Tensor shape representation.
 */
data class TensorShape(val dimensions: List<Int>) {
    init {
        require(dimensions.all { it > 0 }) { "All dimensions must be positive" }
    }

    val rank: Int get() = dimensions.size
    val elementCount: Int get() = dimensions.fold(1) { acc, dim -> acc * dim }

    fun isScalar(): Boolean = dimensions.isEmpty() || (dimensions.size == 1 && dimensions[0] == 1)
    fun isVector(): Boolean = dimensions.size == 1
    fun isMatrix(): Boolean = dimensions.size == 2

    fun canBroadcastWith(other: TensorShape): Boolean {
        val maxRank = maxOf(rank, other.rank)
        val paddedThis = List(maxRank - rank) { 1 } + dimensions
        val paddedOther = List(maxRank - other.rank) { 1 } + other.dimensions

        return paddedThis.zip(paddedOther).all { (a, b) ->
            a == b || a == 1 || b == 1
        }
    }

    fun broadcastWith(other: TensorShape): TensorShape? {
        if (!canBroadcastWith(other)) return null

        val maxRank = maxOf(rank, other.rank)
        val paddedThis = List(maxRank - rank) { 1 } + dimensions
        val paddedOther = List(maxRank - other.rank) { 1 } + other.dimensions

        val resultDims = paddedThis.zip(paddedOther).map { (a, b) -> maxOf(a, b) }
        return TensorShape(resultDims)
    }

    companion object {
        fun scalar(): TensorShape = TensorShape(listOf(1))
        fun vector(size: Int): TensorShape = TensorShape(listOf(size))
        fun matrix(rows: Int, cols: Int): TensorShape = TensorShape(listOf(rows, cols))
        fun cube(size: Int): TensorShape = TensorShape(listOf(size, size, size))
    }
}

/**
 * Multi-dimensional tensor with bounds checking.
 */
class Tensor<T> private constructor(
    val shape: TensorShape,
    private val data: MutableList<T>
) {
    val rank: Int get() = shape.rank
    val size: Int get() = data.size

    private fun flatIndex(indices: List<Int>): Int? {
        if (indices.size != shape.dimensions.size) return null

        var index = 0
        var stride = 1

        for (i in (shape.dimensions.size - 1) downTo 0) {
            if (indices[i] < 0 || indices[i] >= shape.dimensions[i]) return null
            index += indices[i] * stride
            stride *= shape.dimensions[i]
        }

        return index
    }

    operator fun get(vararg indices: Int): T? {
        val flatIdx = flatIndex(indices.toList()) ?: return null
        return data[flatIdx]
    }

    operator fun set(vararg indices: Int, value: T): Boolean {
        val flatIdx = flatIndex(indices.toList()) ?: return false
        data[flatIdx] = value
        return true
    }

    fun toList(): List<T> = data.toList()

    fun <R> map(transform: (T) -> R): Tensor<R> {
        return Tensor(shape, data.map(transform).toMutableList())
    }

    fun forEach(action: (T) -> Unit) {
        data.forEach(action)
    }

    fun forEachIndexed(action: (indices: List<Int>, value: T) -> Unit) {
        for (i in data.indices) {
            val indices = indicesToList(i)
            action(indices, data[i])
        }
    }

    private fun indicesToList(flatIndex: Int): List<Int> {
        val indices = mutableListOf<Int>()
        var remaining = flatIndex

        for (i in (shape.dimensions.size - 1) downTo 0) {
            val stride = shape.dimensions.drop(i + 1).fold(1) { acc, d -> acc * d }
            indices.add(0, remaining / stride)
            remaining %= stride
        }

        return indices
    }

    fun reshape(newShape: TensorShape): Tensor<T>? {
        if (newShape.elementCount != shape.elementCount) return null
        return Tensor(newShape, data.toMutableList())
    }

    fun transpose(): Tensor<T>? {
        if (shape.dimensions.size != 2) return null

        val rows = shape.dimensions[0]
        val cols = shape.dimensions[1]
        val newShape = TensorShape(listOf(cols, rows))
        val newData = MutableList(data.size) { data[0] }

        for (i in 0 until rows) {
            for (j in 0 until cols) {
                newData[j * rows + i] = data[i * cols + j]
            }
        }

        return Tensor(newShape, newData)
    }

    fun slice(dimension: Int, index: Int): Tensor<T>? {
        if (dimension < 0 || dimension >= shape.dimensions.size) return null
        if (index < 0 || index >= shape.dimensions[dimension]) return null

        val newDims = shape.dimensions.toMutableList()
        newDims.removeAt(dimension)

        if (newDims.isEmpty()) {
            // Return scalar
            val value = get(*List(shape.dimensions.size) { if (it == dimension) index else 0 }.toIntArray())
                ?: return null
            return scalar(value)
        }

        val newShape = TensorShape(newDims)
        val newData = mutableListOf<T>()

        forEachIndexed { indices, value ->
            if (indices[dimension] == index) {
                newData.add(value)
            }
        }

        return Tensor(newShape, newData)
    }

    companion object {
        fun <T> create(shape: TensorShape, defaultValue: T): Tensor<T> {
            val data = MutableList(shape.elementCount) { defaultValue }
            return Tensor(shape, data)
        }

        fun <T> fromList(shape: TensorShape, data: List<T>): Tensor<T>? {
            if (data.size != shape.elementCount) return null
            return Tensor(shape, data.toMutableList())
        }

        fun <T> scalar(value: T): Tensor<T> {
            return Tensor(TensorShape.scalar(), mutableListOf(value))
        }

        fun <T> vector(data: List<T>): Tensor<T> {
            return Tensor(TensorShape.vector(data.size), data.toMutableList())
        }

        fun <T> matrix(rows: Int, cols: Int, data: List<T>): Tensor<T>? {
            if (data.size != rows * cols) return null
            return Tensor(TensorShape.matrix(rows, cols), data.toMutableList())
        }

        fun zeros(shape: TensorShape): Tensor<Double> = create(shape, 0.0)
        fun ones(shape: TensorShape): Tensor<Double> = create(shape, 1.0)

        fun identity(size: Int): Tensor<Double> {
            val tensor = zeros(TensorShape.matrix(size, size))
            for (i in 0 until size) {
                tensor[i, i] = 1.0
            }
            return tensor
        }
    }
}

/**
 * Tensor operations for numeric types.
 */
object SafeTensor {
    /**
     * Element-wise addition.
     */
    fun add(a: Tensor<Double>, b: Tensor<Double>): Tensor<Double>? {
        if (a.shape != b.shape) return null

        val result = Tensor.zeros(a.shape)
        val aList = a.toList()
        val bList = b.toList()

        for (i in aList.indices) {
            result.toList()
        }

        return Tensor.fromList(a.shape, aList.zip(bList).map { (x, y) -> x + y })
    }

    /**
     * Element-wise subtraction.
     */
    fun subtract(a: Tensor<Double>, b: Tensor<Double>): Tensor<Double>? {
        if (a.shape != b.shape) return null
        val aList = a.toList()
        val bList = b.toList()
        return Tensor.fromList(a.shape, aList.zip(bList).map { (x, y) -> x - y })
    }

    /**
     * Element-wise multiplication.
     */
    fun multiply(a: Tensor<Double>, b: Tensor<Double>): Tensor<Double>? {
        if (a.shape != b.shape) return null
        val aList = a.toList()
        val bList = b.toList()
        return Tensor.fromList(a.shape, aList.zip(bList).map { (x, y) -> x * y })
    }

    /**
     * Scalar multiplication.
     */
    fun scale(tensor: Tensor<Double>, scalar: Double): Tensor<Double> {
        return tensor.map { it * scalar }
    }

    /**
     * Matrix multiplication.
     */
    fun matmul(a: Tensor<Double>, b: Tensor<Double>): Tensor<Double>? {
        if (!a.shape.isMatrix() || !b.shape.isMatrix()) return null

        val aRows = a.shape.dimensions[0]
        val aCols = a.shape.dimensions[1]
        val bRows = b.shape.dimensions[0]
        val bCols = b.shape.dimensions[1]

        if (aCols != bRows) return null

        val resultShape = TensorShape.matrix(aRows, bCols)
        val result = Tensor.zeros(resultShape)

        for (i in 0 until aRows) {
            for (j in 0 until bCols) {
                var sum = 0.0
                for (k in 0 until aCols) {
                    sum += (a[i, k] ?: 0.0) * (b[k, j] ?: 0.0)
                }
                result[i, j] = sum
            }
        }

        return result
    }

    /**
     * Dot product of two vectors.
     */
    fun dot(a: Tensor<Double>, b: Tensor<Double>): Double? {
        if (!a.shape.isVector() || !b.shape.isVector()) return null
        if (a.size != b.size) return null

        val aList = a.toList()
        val bList = b.toList()
        return aList.zip(bList).sumOf { (x, y) -> x * y }
    }

    /**
     * Sum of all elements.
     */
    fun sum(tensor: Tensor<Double>): Double {
        return tensor.toList().sum()
    }

    /**
     * Mean of all elements.
     */
    fun mean(tensor: Tensor<Double>): Double {
        val list = tensor.toList()
        return if (list.isEmpty()) 0.0 else list.sum() / list.size
    }

    /**
     * Maximum element.
     */
    fun max(tensor: Tensor<Double>): Double? {
        return tensor.toList().maxOrNull()
    }

    /**
     * Minimum element.
     */
    fun min(tensor: Tensor<Double>): Double? {
        return tensor.toList().minOrNull()
    }

    /**
     * L2 norm (Frobenius norm for matrices).
     */
    fun norm(tensor: Tensor<Double>): Double {
        return sqrt(tensor.toList().sumOf { it * it })
    }

    /**
     * Normalize tensor to unit norm.
     */
    fun normalize(tensor: Tensor<Double>): Tensor<Double>? {
        val n = norm(tensor)
        if (n == 0.0) return null
        return scale(tensor, 1.0 / n)
    }

    /**
     * Flatten tensor to 1D.
     */
    fun flatten(tensor: Tensor<Double>): Tensor<Double> {
        return Tensor.vector(tensor.toList())
    }

    /**
     * Concatenate tensors along first dimension.
     */
    fun concat(tensors: List<Tensor<Double>>): Tensor<Double>? {
        if (tensors.isEmpty()) return null
        if (tensors.size == 1) return tensors[0]

        // All tensors must have same dimensions except first
        val firstShape = tensors[0].shape.dimensions
        for (tensor in tensors.drop(1)) {
            if (tensor.shape.dimensions.drop(1) != firstShape.drop(1)) {
                return null
            }
        }

        val newFirstDim = tensors.sumOf { it.shape.dimensions[0] }
        val newDims = listOf(newFirstDim) + firstShape.drop(1)
        val newShape = TensorShape(newDims)
        val newData = tensors.flatMap { it.toList() }

        return Tensor.fromList(newShape, newData)
    }

    /**
     * Apply function element-wise.
     */
    fun apply(tensor: Tensor<Double>, fn: (Double) -> Double): Tensor<Double> {
        return tensor.map(fn)
    }

    /**
     * Clip values to range.
     */
    fun clip(tensor: Tensor<Double>, minVal: Double, maxVal: Double): Tensor<Double> {
        return tensor.map { it.coerceIn(minVal, maxVal) }
    }
}

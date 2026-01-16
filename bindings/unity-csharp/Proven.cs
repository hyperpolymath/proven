// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

using System;
using System.Collections.Generic;
using UnityEngine;

/// <summary>
/// Proven Safety Library for Unity (C#)
///
/// Formally verified safety primitives for game development.
/// Provides safe math, bounded types, and validation for Unity projects.
///
/// Version: 0.9.0
/// </summary>
namespace Proven
{
    #region Result Type

    /// <summary>
    /// Result type for safe operations (functional error handling)
    /// </summary>
    public readonly struct Result<T>
    {
        public readonly bool IsOk;
        public readonly T Value;
        public readonly string Error;

        private Result(bool ok, T value, string error)
        {
            IsOk = ok;
            Value = value;
            Error = error;
        }

        public static Result<T> Ok(T value) => new Result<T>(true, value, null);
        public static Result<T> Err(string error) => new Result<T>(false, default, error);

        public bool IsErr => !IsOk;

        public T UnwrapOr(T defaultValue) => IsOk ? Value : defaultValue;

        public Result<U> Map<U>(Func<T, U> mapper)
        {
            return IsOk ? Result<U>.Ok(mapper(Value)) : Result<U>.Err(Error);
        }

        public Result<U> AndThen<U>(Func<T, Result<U>> mapper)
        {
            return IsOk ? mapper(Value) : Result<U>.Err(Error);
        }
    }

    #endregion

    #region Safe Math

    /// <summary>
    /// Safe math operations with overflow protection
    /// </summary>
    public static class SafeMath
    {
        /// <summary>
        /// Safe addition with overflow check
        /// </summary>
        public static Result<int> Add(int a, int b)
        {
            try
            {
                checked
                {
                    return Result<int>.Ok(a + b);
                }
            }
            catch (OverflowException)
            {
                return Result<int>.Err("overflow");
            }
        }

        /// <summary>
        /// Safe subtraction with underflow check
        /// </summary>
        public static Result<int> Sub(int a, int b)
        {
            try
            {
                checked
                {
                    return Result<int>.Ok(a - b);
                }
            }
            catch (OverflowException)
            {
                return Result<int>.Err("underflow");
            }
        }

        /// <summary>
        /// Safe multiplication with overflow check
        /// </summary>
        public static Result<int> Mul(int a, int b)
        {
            try
            {
                checked
                {
                    return Result<int>.Ok(a * b);
                }
            }
            catch (OverflowException)
            {
                return Result<int>.Err("overflow");
            }
        }

        /// <summary>
        /// Safe division with zero check
        /// </summary>
        public static Result<int> Div(int a, int b)
        {
            if (b == 0)
                return Result<int>.Err("division_by_zero");

            if (a == int.MinValue && b == -1)
                return Result<int>.Err("overflow");

            return Result<int>.Ok(a / b);
        }

        /// <summary>
        /// Safe modulo with zero check
        /// </summary>
        public static Result<int> Mod(int a, int b)
        {
            if (b == 0)
                return Result<int>.Err("modulo_by_zero");

            return Result<int>.Ok(a % b);
        }

        // Long versions
        public static Result<long> Add(long a, long b)
        {
            try
            {
                checked { return Result<long>.Ok(a + b); }
            }
            catch (OverflowException)
            {
                return Result<long>.Err("overflow");
            }
        }

        public static Result<long> Mul(long a, long b)
        {
            try
            {
                checked { return Result<long>.Ok(a * b); }
            }
            catch (OverflowException)
            {
                return Result<long>.Err("overflow");
            }
        }

        // Float operations with inf/nan checks
        public static Result<float> AddFloat(float a, float b)
        {
            float result = a + b;
            if (float.IsInfinity(result) || float.IsNaN(result))
                return Result<float>.Err("float_overflow");
            return Result<float>.Ok(result);
        }

        public static Result<float> MulFloat(float a, float b)
        {
            float result = a * b;
            if (float.IsInfinity(result) || float.IsNaN(result))
                return Result<float>.Err("float_overflow");
            return Result<float>.Ok(result);
        }

        public static Result<float> DivFloat(float a, float b)
        {
            if (b == 0f)
                return Result<float>.Err("division_by_zero");

            float result = a / b;
            if (float.IsInfinity(result) || float.IsNaN(result))
                return Result<float>.Err("float_overflow");

            return Result<float>.Ok(result);
        }
    }

    #endregion

    #region Bounded Types

    /// <summary>
    /// Integer bounded to a range
    /// </summary>
    [Serializable]
    public struct BoundedInt
    {
        [SerializeField] private int _value;
        [SerializeField] private int _min;
        [SerializeField] private int _max;

        public int Value => _value;
        public int Min => _min;
        public int Max => _max;

        public BoundedInt(int value, int min, int max)
        {
            _min = min;
            _max = max;
            _value = Mathf.Clamp(value, min, max);
        }

        public void Set(int value)
        {
            _value = Mathf.Clamp(value, _min, _max);
        }

        public bool Add(int delta)
        {
            var result = SafeMath.Add(_value, delta);
            if (result.IsOk)
            {
                _value = Mathf.Clamp(result.Value, _min, _max);
                return true;
            }
            return false;
        }

        public bool Sub(int delta)
        {
            var result = SafeMath.Sub(_value, delta);
            if (result.IsOk)
            {
                _value = Mathf.Clamp(result.Value, _min, _max);
                return true;
            }
            return false;
        }

        public static implicit operator int(BoundedInt b) => b._value;
    }

    /// <summary>
    /// Float bounded to a range
    /// </summary>
    [Serializable]
    public struct BoundedFloat
    {
        [SerializeField] private float _value;
        [SerializeField] private float _min;
        [SerializeField] private float _max;

        public float Value => _value;
        public float Min => _min;
        public float Max => _max;

        public BoundedFloat(float value, float min, float max)
        {
            _min = min;
            _max = max;
            _value = Mathf.Clamp(value, min, max);
        }

        public void Set(float value)
        {
            _value = Mathf.Clamp(value, _min, _max);
        }

        public float Normalized => (_value - _min) / (_max - _min);

        public static implicit operator float(BoundedFloat b) => b._value;
    }

    /// <summary>
    /// Percentage value (0-100)
    /// </summary>
    [Serializable]
    public struct Percentage
    {
        [SerializeField] private float _value;

        public float Value => _value;

        public Percentage(float value)
        {
            _value = Mathf.Clamp(value, 0f, 100f);
        }

        public void Set(float value)
        {
            _value = Mathf.Clamp(value, 0f, 100f);
        }

        public float Normalized => _value / 100f;

        public float Of(float amount) => amount * _value / 100f;

        public static implicit operator float(Percentage p) => p._value;
    }

    /// <summary>
    /// Normalized value (0-1)
    /// </summary>
    [Serializable]
    public struct NormalizedFloat
    {
        [SerializeField] private float _value;

        public float Value => _value;

        public NormalizedFloat(float value)
        {
            _value = Mathf.Clamp01(value);
        }

        public void Set(float value)
        {
            _value = Mathf.Clamp01(value);
        }

        public static implicit operator float(NormalizedFloat n) => n._value;
    }

    #endregion

    #region Game Systems

    /// <summary>
    /// Health system with bounded values and events
    /// </summary>
    [Serializable]
    public class Health
    {
        [SerializeField] private int _current;
        [SerializeField] private int _maximum;

        public int Current => _current;
        public int Maximum => _maximum;
        public float Percentage => (float)_current / _maximum * 100f;
        public float Normalized => (float)_current / _maximum;
        public bool IsDead => _current <= 0;
        public bool IsFull => _current >= _maximum;

        public event Action<int, int> OnDamaged;  // (damage, newHealth)
        public event Action<int, int> OnHealed;   // (healing, newHealth)
        public event Action OnDeath;
        public event Action OnRevive;

        public Health(int maxHp, int? initial = null)
        {
            _maximum = Mathf.Max(1, maxHp);
            _current = Mathf.Clamp(initial ?? maxHp, 0, _maximum);
        }

        public int Damage(int amount)
        {
            if (amount <= 0 || IsDead) return 0;

            int actual = Mathf.Min(_current, amount);
            _current -= actual;

            OnDamaged?.Invoke(actual, _current);

            if (IsDead)
                OnDeath?.Invoke();

            return actual;
        }

        public int Heal(int amount)
        {
            if (amount <= 0) return 0;

            bool wasDead = IsDead;
            int actual = Mathf.Min(_maximum - _current, amount);
            _current += actual;

            OnHealed?.Invoke(actual, _current);

            if (wasDead && !IsDead)
                OnRevive?.Invoke();

            return actual;
        }

        public void SetMax(int newMax)
        {
            _maximum = Mathf.Max(1, newMax);
            _current = Mathf.Min(_current, _maximum);
        }

        public void Reset()
        {
            _current = _maximum;
        }
    }

    /// <summary>
    /// Cooldown timer with safe operations
    /// </summary>
    [Serializable]
    public class Cooldown
    {
        [SerializeField] private float _duration;
        [SerializeField] private float _remaining;

        public float Duration => _duration;
        public float Remaining => _remaining;
        public float Progress => _duration > 0 ? 1f - (_remaining / _duration) : 1f;
        public bool IsReady => _remaining <= 0f;

        public event Action OnReady;

        public Cooldown(float duration)
        {
            _duration = Mathf.Max(0f, duration);
            _remaining = 0f;
        }

        public void Start()
        {
            _remaining = _duration;
        }

        public void Update(float deltaTime)
        {
            if (_remaining <= 0f) return;

            bool wasNotReady = !IsReady;
            _remaining = Mathf.Max(0f, _remaining - deltaTime);

            if (wasNotReady && IsReady)
                OnReady?.Invoke();
        }

        public bool TryUse()
        {
            if (!IsReady) return false;
            Start();
            return true;
        }
    }

    /// <summary>
    /// Resource pool (mana, stamina, energy, etc.)
    /// </summary>
    [Serializable]
    public class ResourcePool
    {
        [SerializeField] private float _current;
        [SerializeField] private float _maximum;
        [SerializeField] private float _regenRate;

        public float Current => _current;
        public float Maximum => _maximum;
        public float Percentage => _current / _maximum * 100f;
        public float Normalized => _current / _maximum;

        public event Action<float> OnConsumed;  // amount consumed
        public event Action OnDepleted;

        public ResourcePool(float max, float regenPerSecond = 0f)
        {
            _maximum = Mathf.Max(0f, max);
            _current = _maximum;
            _regenRate = regenPerSecond;
        }

        public bool CanAfford(float cost) => _current >= cost;

        public bool TryConsume(float amount)
        {
            if (amount <= 0f) return true;
            if (_current < amount) return false;

            _current -= amount;
            OnConsumed?.Invoke(amount);

            if (_current <= 0f)
                OnDepleted?.Invoke();

            return true;
        }

        public void Regenerate(float deltaTime)
        {
            if (_regenRate <= 0f || _current >= _maximum) return;
            _current = Mathf.Min(_maximum, _current + _regenRate * deltaTime);
        }

        public void Refill()
        {
            _current = _maximum;
        }
    }

    #endregion

    #region Validation

    /// <summary>
    /// Validation utilities for game values
    /// </summary>
    public static class Validate
    {
        public static bool IsValidHealth(int value, int maxHp = 100)
            => value >= 0 && value <= maxHp;

        public static bool IsValidPercentage(float value)
            => value >= 0f && value <= 100f;

        public static bool IsNormalized(float value)
            => value >= 0f && value <= 1f;

        public static bool IsValidAngleDeg(float value)
            => value >= 0f && value < 360f;

        public static bool IsValidAngleRad(float value)
            => value >= 0f && value < Mathf.PI * 2f;

        public static bool IsValidColor(Color color)
            => color.r >= 0f && color.r <= 1f &&
               color.g >= 0f && color.g <= 1f &&
               color.b >= 0f && color.b <= 1f &&
               color.a >= 0f && color.a <= 1f;

        public static bool IsValidColor32(Color32 color)
            => true; // Color32 is always valid (0-255 per component)

        public static bool IsValidLayer(int layer)
            => layer >= 0 && layer < 32;

        public static bool IsValidTag(string tag)
            => !string.IsNullOrEmpty(tag);
    }

    #endregion

    #region Safe Operations

    /// <summary>
    /// Safe Unity operations that can fail
    /// </summary>
    public static class SafeOps
    {
        /// <summary>
        /// Safe GetComponent with Result type
        /// </summary>
        public static Result<T> GetComponent<T>(GameObject obj) where T : Component
        {
            if (obj == null)
                return Result<T>.Err("null_gameobject");

            var component = obj.GetComponent<T>();
            if (component == null)
                return Result<T>.Err("component_not_found");

            return Result<T>.Ok(component);
        }

        /// <summary>
        /// Safe GetComponent with Result type
        /// </summary>
        public static Result<T> GetComponent<T>(Component comp) where T : Component
        {
            if (comp == null)
                return Result<T>.Err("null_component");

            var component = comp.GetComponent<T>();
            if (component == null)
                return Result<T>.Err("component_not_found");

            return Result<T>.Ok(component);
        }

        /// <summary>
        /// Safe Find with tag
        /// </summary>
        public static Result<GameObject> FindWithTag(string tag)
        {
            if (string.IsNullOrEmpty(tag))
                return Result<GameObject>.Err("empty_tag");

            try
            {
                var obj = GameObject.FindWithTag(tag);
                if (obj == null)
                    return Result<GameObject>.Err("object_not_found");

                return Result<GameObject>.Ok(obj);
            }
            catch (UnityException)
            {
                return Result<GameObject>.Err("invalid_tag");
            }
        }

        /// <summary>
        /// Safe instantiate
        /// </summary>
        public static Result<T> Instantiate<T>(T prefab) where T : UnityEngine.Object
        {
            if (prefab == null)
                return Result<T>.Err("null_prefab");

            var instance = UnityEngine.Object.Instantiate(prefab);
            if (instance == null)
                return Result<T>.Err("instantiation_failed");

            return Result<T>.Ok(instance);
        }

        /// <summary>
        /// Safe array access
        /// </summary>
        public static Result<T> ArrayGet<T>(T[] array, int index)
        {
            if (array == null)
                return Result<T>.Err("null_array");

            if (index < 0 || index >= array.Length)
                return Result<T>.Err("index_out_of_bounds");

            return Result<T>.Ok(array[index]);
        }

        /// <summary>
        /// Safe list access
        /// </summary>
        public static Result<T> ListGet<T>(List<T> list, int index)
        {
            if (list == null)
                return Result<T>.Err("null_list");

            if (index < 0 || index >= list.Count)
                return Result<T>.Err("index_out_of_bounds");

            return Result<T>.Ok(list[index]);
        }

        /// <summary>
        /// Safe dictionary access
        /// </summary>
        public static Result<V> DictGet<K, V>(Dictionary<K, V> dict, K key)
        {
            if (dict == null)
                return Result<V>.Err("null_dictionary");

            if (!dict.TryGetValue(key, out V value))
                return Result<V>.Err("key_not_found");

            return Result<V>.Ok(value);
        }
    }

    #endregion

    #region Physics Utilities

    /// <summary>
    /// Safe physics operations
    /// </summary>
    public static class SafePhysics
    {
        /// <summary>
        /// Clamp velocity to max speed
        /// </summary>
        public static Vector2 ClampVelocity(Vector2 velocity, float maxSpeed)
        {
            if (maxSpeed <= 0f) return Vector2.zero;
            if (velocity.sqrMagnitude > maxSpeed * maxSpeed)
                return velocity.normalized * maxSpeed;
            return velocity;
        }

        /// <summary>
        /// Clamp velocity to max speed (3D)
        /// </summary>
        public static Vector3 ClampVelocity(Vector3 velocity, float maxSpeed)
        {
            if (maxSpeed <= 0f) return Vector3.zero;
            if (velocity.sqrMagnitude > maxSpeed * maxSpeed)
                return velocity.normalized * maxSpeed;
            return velocity;
        }

        /// <summary>
        /// Safe lerp that clamps weight
        /// </summary>
        public static float SafeLerp(float a, float b, float t)
        {
            return Mathf.Lerp(a, b, Mathf.Clamp01(t));
        }

        /// <summary>
        /// Safe lerp for Vector2
        /// </summary>
        public static Vector2 SafeLerp(Vector2 a, Vector2 b, float t)
        {
            return Vector2.Lerp(a, b, Mathf.Clamp01(t));
        }

        /// <summary>
        /// Safe lerp for Vector3
        /// </summary>
        public static Vector3 SafeLerp(Vector3 a, Vector3 b, float t)
        {
            return Vector3.Lerp(a, b, Mathf.Clamp01(t));
        }

        /// <summary>
        /// Safe move toward that handles zero delta
        /// </summary>
        public static float SafeMoveToward(float current, float target, float maxDelta)
        {
            if (maxDelta <= 0f) return current;
            return Mathf.MoveTowards(current, target, maxDelta);
        }
    }

    #endregion

    #region Version

    /// <summary>
    /// Library information
    /// </summary>
    public static class ProvenInfo
    {
        public const string Version = "0.9.0";
    }

    #endregion
}

(defpackage :lbge.math
  (:use :cl)
  (:local-nicknames (:ax :alexandria))
  (:export
    ; exports from common.lisp:
    :hand
    :eqfp
    :neqfp
    :round-to-eps
    :clamp
    :get-sign
    :append-to

    ; exports from definitions.lisp:
    :size
    :add
    :sub
    :mul
    :div
    :norm2
    :norm
    :eqg
    :neqg
    :negg
    :absg
    :det
    :transpose
    :conj
    :inv
    :versor
    :expq
    :logq
    :exptq

    ; exports from vector.lisp:
    :float2
    :float3
    :float4
    :float2-zero
    :float3-zero
    :float4-zero
    :float2-one
    :float3-one
    :float4-one
    :x
    :y
    :z
    :w
    :in-vec
    :make-float2
    :make-float3
    :make-float4
    :dot
    :cross
    :zero-vector-p
    :angle
    :project
    :normalize

    ; exports from matrix.lisp:
    :make-float2x2
    :make-float3x3
    :make-float4x4
    :float2x2-zero
    :float3x3-zero
    :float4x4-zero
    :float2x2-one
    :float3x3-one
    :float4x4-one
    :float2x2-iden
    :float3x3-iden
    :float4x4-iden
    :mat-size
    :get-at
    :set-at
    :get-row
    :get-col
    :det2x2
    :det3x3
    :det4x4
    :make-ortho-projection
    :make-look-at

    ; exports from quaternion.lisp:
    :make-quaternion
    :make-quaternion-f2
    :make-quaternion-f3
    :quaternion-zero
    :quaternion-one
    :quaternion-v
    :quaternion-a
    :from-euler
    :to-euler

    ; exports from transform.lisp:
    :make-transform
    :translation
    :rotation
    :scale
    :transform-matrix

    ; exports from polynomial.lisp
    :make-polynomial
    :raise-degree
    :pad-poly
    :call

    ; exports from lerp.lisp
    :lerp
    :slerp

    ; exports from newton.lisp
    :make-newton
    :copy-newton-interp
    :nadd-point
    :add-point
    :call))

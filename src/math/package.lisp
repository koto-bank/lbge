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

   ; exports from definitions.lisp:
    :get-size
    :add
    :sub
    :mul
    :div
    :norm2
    :norm
    :negv
    :absv
    :det
    :transpose
    :absm
    :negm
    :conj
    :inv
    :versor
    :expq
    :logq
    :exptq
    :absq
    :negq

   ; exports from vector.lisp:
    :float2-x
    :float2-y
    :float3-x
    :float3-y
    :float3-z
    :float4-x
    :float4-y
    :float4-z
    :float4-w
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
    :eqv
    :neqv
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
    :eqm
    :neqm
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
    :eqq
    :neqq
    :from-euler
    :to-euler

   ; exports from transform.lisp:
    :make-transform
    :translation
    :rotation
    :scale
    :transform-matrix

  ; exports from interpolations.lisp
   :lerp))

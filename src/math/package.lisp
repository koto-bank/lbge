(defpackage :lbge.math
  (:use :cl)
  (:local-nicknames (:ax :alexandria))
  (:export
   ; exports from common.lisp
   :eqfp
   :neqfp
   ; exports from vector.lisp
   :float2
   :float3
   :float4
   :float2-zero
   :float3-zero
   :float4-zero
   :float2-one
   :float3-one
   :float4-one
   :float2-x
   :float2-y
   :float3-x
   :float3-y
   :float3-z
   :float4-x
   :float4-y
   :float4-z
   :float4-w
   :make-float2
   :make-float3
   :make-float4
   :in-list
   :get-size
   :add
   :sub
   :mul
   :div
   :dot
   :norm
   :cross
   :normalize
   :angle
   :negv
   :absv
   :project
   :eqv
   :neqv
   ; exports from matrix.lisp
   :det
   :transpose
   :absm
   :negm
   :eqm
   :neqm
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
   :make-ortho-projection

   ;; quaternions
   :make-quaternion
   :quaternion-x
   :quaternion-y
   :quaternion-z
   :quaternion-w

   ;; transform
   :make-transform
   :translation
   :rotation
   :scale
   :transform-matrix))

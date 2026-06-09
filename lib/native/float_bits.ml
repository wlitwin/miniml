(** Reinterpret a float's IEEE-754 bits as uppercase hexadecimal, for emitting
    exact float constants into LLVM IR.

    This is kept out of the translated codegen because [Int64] has no MiniML
    equivalent (a MiniML int is 63-bit and cannot hold the sign bit of a
    double). The translator maps [Float_bits.bits_hex] to the MiniML builtin
    [Math.float_bits_hex] (lib/std + each backend), so the self-host codegen
    obtains the same string without an [Int64] dependency. *)

let bits_hex f = Printf.sprintf "%LX" (Int64.bits_of_float f)

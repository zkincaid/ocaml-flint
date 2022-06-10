open Ctypes
open C.Type
open C.Function

module ARF = struct
  type t = arf_t

  module C = struct
    let arf_t = C.Type.arf_t

    let mk_arf () : arf_t =
      allocate_n ~count:1 ~finalise:C.Function.arf_clear arf_struct
  end

  module External = struct
    module CI = Cstubs_internals

    type 'a fatptr = (Obj.t option, 'a) CI.fatptr

    external flint_stubs_utils_to_string : arf fatptr -> string
      = "arf_stubs_utils_to_string"

    let to_string (CI.CPointer p : t) = flint_stubs_utils_to_string p
  end

  let pp fmt x = Format.pp_print_string fmt (External.to_string x)

  let get_fmpz_fixed_si x n =
    let z = Flint.FMPZ.C.mk_fmpz () in
    ignore (arf_get_fmpz_fixed_si z x (Signed.Long.of_int n));
    Flint.FMPZ.to_z z
end

module MAG = struct
  type t = mag_t

  module C = struct
    let mag_t = C.Type.mag_t

    let mk_mag () : mag_t =
      allocate_n ~count:1 ~finalise:C.Function.mag_clear mag_struct
  end

  module External = struct
    module CI = Cstubs_internals

    type 'a fatptr = (Obj.t option, 'a) CI.fatptr

    external flint_stubs_utils_to_string : mag fatptr -> string
      = "mag_stubs_utils_to_string"

    let to_string (CI.CPointer p : t) = flint_stubs_utils_to_string p
  end

  let pp fmt x = Format.pp_print_string fmt (External.to_string x)
end

module ARB = struct
  type t = ARB.t

  module C = struct
    let arb_t = C.Type.ARB.t

    let mk_arb () : ARB.t =
      allocate_n ~count:1 ~finalise:C.Function.arb_clear ARB.s
  end

  module External = struct
    module CI = Cstubs_internals

    type 'a fatptr = (Obj.t option, 'a) CI.fatptr

    external flint_stubs_utils_to_string : ARB.s fatptr -> string
      = "arb_stubs_utils_to_string"

    let to_string (CI.CPointer p : t) = flint_stubs_utils_to_string p
  end

  let pp fmt x = Format.pp_print_string fmt (External.to_string x)
  let mid x = x |-> ARB.mid
  let rad x = x |-> ARB.rad
end

module ACB = struct
  type t = ACB.t

  module C = struct
    let acb_t = C.Type.ACB.t

    let mk_acb () : ACB.t =
      allocate_n ~count:1 ~finalise:C.Function.acb_clear ACB.s
  end

  module External = struct
    module CI = Cstubs_internals

    type 'a fatptr = (Obj.t option, 'a) CI.fatptr

    external flint_stubs_utils_to_string : ACB.s fatptr -> string
      = "acb_stubs_utils_to_string"

    let to_string (CI.CPointer p : t) = flint_stubs_utils_to_string p
  end

  let pp fmt x = Format.pp_print_string fmt (External.to_string x)
  let rel_accuracy_bits t = Signed.Long.to_int @@ acb_rel_accuracy_bits t
  let real x = x |-> ACB.real
  let imag x = x |-> ACB.imag
end

open Ctypes

(* This Types_generated module is an instantiation of the Types
   functor defined in the type_description.ml file. It's generated by
   a C program that dune creates and runs behind the scenes. *)
module Types = Types_generated

module Functions (F : Ctypes.FOREIGN) = struct
  open F
  open Types

  (* let ml_z_mpz_init_set_z =
   *   foreign "ml_z_mpz_init_set_z" (MPZ.t @-> z @-> returning void)
   *
   * let ml_z_from_mpz = foreign "ml_z_from_mpz" (MPZ.t @-> returning z) *)

  let fmpz_clear = foreign "fmpz_clear" (fmpz_t @-> returning void)
  let fmpz_get_si = foreign "fmpz_get_si" (fmpz_t @-> returning long)
  let fmpz_set_si = foreign "fmpz_set_si" (fmpz_t @-> long @-> returning void)
  let fmpz_set = foreign "fmpz_set" (fmpz_t @-> fmpz_t @-> returning void)
  let fmpz_init = foreign "fmpz_init" (fmpz_t @-> returning void)

  let fmpz_init_set_ui =
    foreign "fmpz_init_set_ui" (fmpz_t @-> ulong @-> returning void)

  let fmpq_clear = foreign "fmpq_clear" (fmpq_t @-> returning void)

  let fmpq_set_fmpz_frac =
    foreign "fmpq_set_fmpz_frac"
      (fmpq_t @-> fmpz_t @-> fmpz_t @-> returning void)

  let free = foreign "free" (ptr char @-> returning void)
  let strlen = foreign "strlen" (ptr char @-> returning size_t)

  (** fmpz_poly ************************************************************************)

  let fmpz_poly_init = foreign "fmpz_poly_init" (fmpz_poly_t @-> returning void)

  let fmpz_poly_init2 =
    foreign "fmpz_poly_init2" (fmpz_poly_t @-> long @-> returning void)

  let fmpz_poly_realloc =
    foreign "fmpz_poly_realloc" (fmpz_poly_t @-> long @-> returning void)

  let fmpz_poly_clear =
    foreign "fmpz_poly_clear" (fmpz_poly_t @-> returning void)

  let fmpz_poly_get_coeff_fmpz =
    foreign "fmpz_poly_get_coeff_fmpz"
      (fmpz_t @-> fmpz_poly_t @-> long @-> returning void)

  let fmpz_poly_set_coeff_fmpz =
    foreign "fmpz_poly_set_coeff_fmpz"
      (fmpz_poly_t @-> long @-> fmpz_t @-> returning void)

  let fmpz_poly_set =
    foreign "fmpz_poly_set" (fmpz_poly_t @-> fmpz_poly_t @-> returning void)

  let fmpz_poly_add =
    foreign "fmpz_poly_add"
      (fmpz_poly_t @-> fmpz_poly_t @-> fmpz_poly_t @-> returning void)

  let fmpz_poly_sub =
    foreign "fmpz_poly_sub"
      (fmpz_poly_t @-> fmpz_poly_t @-> fmpz_poly_t @-> returning void)

  let fmpz_poly_mul =
    foreign "fmpz_poly_mul"
      (fmpz_poly_t @-> fmpz_poly_t @-> fmpz_poly_t @-> returning void)

  let fmpz_poly_scalar_mul_fmpz =
    foreign "fmpz_poly_scalar_mul_fmpz"
      (fmpz_poly_t @-> fmpz_poly_t @-> fmpz_t @-> returning void)

  let fmpz_poly_set_si =
    foreign "fmpz_poly_set_si" (fmpz_poly_t @-> long @-> returning void)

  (** fmpz_mat *************************************************************************)

  let fmpz_mat_init =
    foreign "fmpz_mat_init" (fmpz_mat_t @-> long @-> long @-> returning void)

  let fmpz_mat_clear =
    foreign "fmpz_mat_clear" (fmpz_mat_t @-> returning void)

  let fmpz_mat_set =
    foreign "fmpz_mat_set" (fmpz_mat_t @-> fmpz_mat_t @-> returning void)

  let fmpz_mat_entry =
    foreign "fmpz_mat_entry" (fmpz_mat_t @-> long @-> long @-> returning fmpz_t)

  let fmpz_mat_zero =
    foreign "fmpz_mat_zero" (fmpz_mat_t @-> returning void)

  let fmpz_mat_one =
    foreign "fmpz_mat_one" (fmpz_mat_t @-> returning void)

  let fmpz_mat_equal =
    foreign "fmpz_mat_equal" (fmpz_mat_t @-> fmpz_mat_t @-> returning bool)

  let fmpz_mat_is_zero =
    foreign "fmpz_mat_is_zero" (fmpz_mat_t @-> returning bool)

  let fmpz_mat_is_one =
    foreign "fmpz_mat_is_one" (fmpz_mat_t @-> returning bool)

  let fmpz_mat_window_init =
    foreign
      "fmpz_mat_window_init"
      (fmpz_mat_t @-> fmpz_mat_t @-> long @-> long @-> long @-> long @-> returning void)

  let fmpz_mat_transpose =
    foreign "fmpz_mat_transpose" (fmpz_mat_t @-> fmpz_mat_t @-> returning void)

  let fmpz_mat_concat_vertical =
    foreign
      "fmpz_mat_concat_vertical"
      (fmpz_mat_t @-> fmpz_mat_t @-> fmpz_mat_t @-> returning void)

  let fmpz_mat_concat_horizontal =
    foreign
      "fmpz_mat_concat_horizontal"
      (fmpz_mat_t @-> fmpz_mat_t @-> fmpz_mat_t @-> returning void)

  let fmpz_mat_add =
    foreign "fmpz_mat_add" (fmpz_mat_t @-> fmpz_mat_t @-> fmpz_mat_t @-> returning void)

  let fmpz_mat_sub =
    foreign "fmpz_mat_sub" (fmpz_mat_t @-> fmpz_mat_t @-> fmpz_mat_t @-> returning void)

  let fmpz_mat_neg =
    foreign "fmpz_mat_neg" (fmpz_mat_t @-> fmpz_mat_t @-> returning void)

  let fmpz_mat_scalar_mul_fmpz =
    foreign
      "fmpz_mat_scalar_mul_fmpz"
      (fmpz_mat_t @-> fmpz_mat_t @-> fmpz_t @-> returning void)

  let fmpz_mat_scalar_divexact_fmpz =
    foreign
      "fmpz_mat_scalar_divexact_fmpz"
      (fmpz_mat_t @-> fmpz_mat_t @-> fmpz_t @-> returning void)

  let fmpz_mat_mul =
    foreign "fmpz_mat_mul" (fmpz_mat_t @-> fmpz_mat_t @-> fmpz_mat_t @-> returning void)

  let fmpz_mat_inv =
    foreign "fmpz_mat_inv" (fmpz_mat_t @-> fmpz_t @-> fmpz_mat_t @-> returning bool)

  let fmpz_mat_kronecker_product =
    foreign
      "fmpz_mat_kronecker_product"
      (fmpz_mat_t @-> fmpz_mat_t @-> fmpz_mat_t @-> returning void)

  let fmpz_mat_content =
    foreign "fmpz_mat_content" (fmpz_t @-> fmpz_mat_t @-> returning void)

  let fmpz_mat_trace =
    foreign "fmpz_mat_trace" (fmpz_t @-> fmpz_mat_t @-> returning void)

  let fmpz_mat_det =
    foreign "fmpz_mat_det" (fmpz_t @-> fmpz_mat_t @-> returning void)

  let fmpz_mat_rank =
    foreign "fmpz_mat_rank" (fmpz_mat_t @-> returning long)

  let fmpz_mat_charpoly =
    foreign "fmpz_mat_charpoly" (fmpz_poly_t @-> fmpz_mat_t @-> returning void)

  let fmpz_mat_hnf =
    foreign "fmpz_mat_hnf" (fmpz_mat_t @-> fmpz_mat_t @-> returning void)

  let fmpz_mat_hnf_transform =
    foreign
      "fmpz_mat_hnf_transform"
      (fmpz_mat_t @-> fmpz_mat_t @-> fmpz_mat_t @-> returning void)

  let fmpz_mat_lll =
    foreign
      "fmpz_mat_lll_original"
      (fmpz_mat_t @-> fmpq_t @-> fmpq_t @-> returning void)


  (** fmpz_poly_factor *****************************************************************)

  let fmpz_poly_factor_init =
    foreign "fmpz_poly_factor_init" (fmpz_poly_factor_t @-> returning void)

  let fmpz_poly_factor_clear =
    foreign "fmpz_poly_factor_clear" (fmpz_poly_factor_t @-> returning void)

  let fmpz_poly_factor =
    foreign "fmpz_poly_factor" (fmpz_poly_factor_t @-> fmpz_poly_t @-> returning void)

  let fmpz_poly_factor_squarefree =
    foreign
      "fmpz_poly_factor_squarefree"
      (fmpz_poly_factor_t @-> fmpz_poly_t @-> returning void)

  (** acb ******************************************************************************)
  let acb_init = foreign "acb_init" (ACB.t @-> returning void)
  let acb_clear = foreign "acb_clear" (ACB.t @-> returning void)
  let acb_set = foreign "acb_set" (ACB.t @-> ACB.t @-> returning void)

  let acb_set_arb_arb =
    foreign "acb_set_arb_arb" (ACB.t @-> ARB.t @-> ARB.t @-> returning void)

  let acb_rel_accuracy_bits =
    foreign "acb_rel_accuracy_bits" (ACB.t @-> returning long)

  let acb_set_fmpz =
    foreign "acb_set_fmpz" (ACB.t @-> fmpz_t @-> returning void)

  let acb_set_fmpq =
    foreign "acb_set_fmpq" (ACB.t @-> fmpq_t @-> long @-> returning void)

  let acb_equal =
    foreign "acb_equal" (ACB.t @-> ACB.t @-> returning bool)

  let acb_overlaps =
    foreign "acb_overlaps" (ACB.t @-> ACB.t @-> returning bool)

  let acb_contains =
    foreign "acb_contains" (ACB.t @-> ACB.t @-> returning bool)

  let acb_trim =
    foreign "acb_trim" (ACB.t @-> ACB.t @-> returning void)

  let acb_neg =
    foreign "acb_neg" (ACB.t @-> ACB.t @-> returning void)

  let acb_conj =
    foreign "acb_conj" (ACB.t @-> ACB.t @-> returning void)

  let acb_add =
    foreign "acb_add" (ACB.t @-> ACB.t @-> ACB.t @-> long @-> returning void)

  let acb_mul =
    foreign "acb_mul" (ACB.t @-> ACB.t @-> ACB.t @-> long @-> returning void)

  let acb_inv =
    foreign "acb_inv" (ACB.t @-> ACB.t @-> long @-> returning void)

  let acb_div =
    foreign "acb_div" (ACB.t @-> ACB.t @-> ACB.t @-> long @-> returning void)

  let acb_const_pi =
    foreign "acb_const_pi" (ACB.t @-> long @-> returning void)

  let acb_pow_si =
    foreign "acb_pow_si" (ACB.t @-> ACB.t @-> long @-> long @-> returning void)

  let acb_log =
    foreign "acb_log" (ACB.t @-> ACB.t @-> long @-> returning void)


  (** arb_fmpz_poly ********************************************************************)
  let arb_fmpz_poly_complex_roots =
    foreign
      "arb_fmpz_poly_complex_roots"
      (ACB.t @-> fmpz_poly_t @-> long @-> long @-> returning void)


  (** mag ******************************************************************************)
  let mag_init = foreign "mag_init" (mag_t @-> returning void)
  let mag_clear = foreign "mag_clear" (mag_t @-> returning void)
  let mag_get_fmpz = foreign "mag_get_fmpz" (fmpz_t @-> mag_t @-> returning void)

  (** arf ******************************************************************************)
  let arf_init = foreign "arf_init" (arf_t @-> returning void)
  let arf_clear = foreign "arf_clear" (arf_t @-> returning void)
  let arf_get_fmpz_fixed_si =
    foreign "arf_get_fmpz_fixed_si"
      (fmpz_t @-> arf_t @-> long @-> returning bool)
  let arf_set_fmpz_2exp =
    foreign "arf_set_fmpz_2exp" (ARF.t @-> fmpz_t @-> fmpz_t @-> returning void)
  let arf_get_fmpz_2exp =
    foreign "arf_get_fmpz_2exp" (fmpz_t @-> fmpz_t @-> ARF.t @-> returning void)


  (** arb ******************************************************************************)
  let arb_init = foreign "arb_init" (ARB.t @-> returning void)
  let arb_clear = foreign "arb_clear" (ARB.t @-> returning void)

  let arb_set_interval_arf =
    foreign "arb_set_interval_arf"
      (ARB.t @-> ARF.t @-> ARF.t @-> long @-> returning void)

  let arb_set_round_fmpz_2exp =
    foreign "arb_set_round_fmpz_2exp"
      (ARB.t @-> fmpz_t @-> fmpz_t @-> long @-> returning void)

  let arb_zero = foreign "arb_zero" (ARB.t @-> returning void)

  let arb_get_mag = foreign "arb_get_mag" (MAG.t @-> ARB.t @-> returning void)

  (** ca *******************************************************************************)
  let ca_init = foreign "ca_init" (ca_t @-> ca_ctx_t @-> returning void)
  let ca_clear = foreign "ca_clear" (ca_t @-> ca_ctx_t @-> returning void)

  let ca_set_si =
    foreign "ca_set_si" (ca_t @-> long @-> ca_ctx_t @-> returning void)

  let ca_set_fmpz =
    foreign "ca_set_fmpz" (ca_t @-> fmpz_t @-> ca_ctx_t @-> returning void)

  let ca_set_fmpq =
    foreign "ca_set_fmpq" (ca_t @-> fmpq_t @-> ca_ctx_t @-> returning void)

  let ca_ctx_init = foreign "ca_ctx_init" (ca_ctx_t @-> returning void)
  let ca_ctx_clear = foreign "ca_ctx_clear" (ca_ctx_t @-> returning void)

  let ca_equal_repr =
    foreign "ca_equal_repr" (ca_t @-> ca_t @-> ca_ctx_t @-> returning bool)

  let ca_cmp_repr =
    foreign "ca_cmp_repr" (ca_t @-> ca_t @-> ca_ctx_t @-> returning int)

  let ca_hash_repr =
    foreign "ca_hash_repr" (ca_t @-> ca_ctx_t @-> returning ulong)

  let ca_is_unknown =
    foreign "ca_is_unknown" (ca_t @-> ca_ctx_t @-> returning bool)

  let ca_is_special =
    foreign "ca_is_special" (ca_t @-> ca_ctx_t @-> returning bool)

  let ca_check_equal =
    foreign "ca_check_equal" (ca_t @-> ca_t @-> ca_ctx_t @-> returning truth_t)

  let ca_check_lt =
    foreign "ca_check_lt" (ca_t @-> ca_t @-> ca_ctx_t @-> returning truth_t)

  let ca_check_le =
    foreign "ca_check_le" (ca_t @-> ca_t @-> ca_ctx_t @-> returning truth_t)

  let ca_check_gt =
    foreign "ca_check_gt" (ca_t @-> ca_t @-> ca_ctx_t @-> returning truth_t)

  let ca_check_ge =
    foreign "ca_check_ge" (ca_t @-> ca_t @-> ca_ctx_t @-> returning truth_t)

  let ca_floor =
    foreign "ca_floor" (ca_t @-> ca_t @-> ca_ctx_t @-> returning void)

  let ca_ceil = foreign "ca_ceil" (ca_t @-> ca_t @-> ca_ctx_t @-> returning void)

  let ca_get_acb_accurate_parts =
    foreign "ca_get_acb_accurate_parts"
      (ACB.t @-> ca_t @-> long @-> ca_ctx_t @-> returning void)

  let ca_sqrt = foreign "ca_sqrt" (ca_t @-> ca_t @-> ca_ctx_t @-> returning void)

  let ca_add =
    foreign "ca_add" (ca_t @-> ca_t @-> ca_t @-> ca_ctx_t @-> returning void)

  let ca_sub =
    foreign "ca_sub" (ca_t @-> ca_t @-> ca_t @-> ca_ctx_t @-> returning void)

  let ca_mul =
    foreign "ca_mul" (ca_t @-> ca_t @-> ca_t @-> ca_ctx_t @-> returning void)

  let ca_div =
    foreign "ca_div" (ca_t @-> ca_t @-> ca_t @-> ca_ctx_t @-> returning void)

  let ca_pow_si =
    foreign "ca_pow_si" (ca_t @-> ca_t @-> long @-> ca_ctx_t @-> returning void)

  let ca_pow_fmpq =
    foreign "ca_pow_fmpq"
      (ca_t @-> ca_t @-> fmpq_t @-> ca_ctx_t @-> returning void)

  let ca_neg = foreign "ca_neg" (ca_t @-> ca_t @-> ca_ctx_t @-> returning void)
  let ca_inv = foreign "ca_inv" (ca_t @-> ca_t @-> ca_ctx_t @-> returning void)
  let ca_abs = foreign "ca_abs" (ca_t @-> ca_t @-> ca_ctx_t @-> returning void)

  let ca_get_fmpq =
    foreign "ca_get_fmpq" (fmpq_t @-> ca_t @-> ca_ctx_t @-> returning bool)

  let ca_get_fmpz =
    foreign "ca_get_fmpz" (fmpz_t @-> ca_t @-> ca_ctx_t @-> returning bool)

  let ca_check_is_negative_real =
    foreign "ca_check_is_negative_real" (ca_t @-> ca_ctx_t @-> returning truth_t)

  let qqbar_init = foreign "qqbar_init" (qqbar_t @-> returning void)
  let qqbar_clear = foreign "qqbar_clear" (qqbar_t @-> returning void)

  let ca_get_qqbar =
    foreign "ca_get_qqbar" (qqbar_t @-> ca_t @-> ca_ctx_t @-> returning bool)

  let ca_set_qqbar =
    foreign "ca_set_qqbar" (ca_t @-> qqbar_t @-> ca_ctx_t @-> returning void)

  let flag_qqbar_roots =
    let cons_if i cst cst' l = if Int.logand i cst = 0 then l else cst' :: l in
    view int
      ~read:(fun i ->
        cons_if i qqbar_roots_irreducible QQBAR_ROOTS_IRREDUCIBLE
        @@ cons_if i qqbar_roots_unsorted QQBAR_ROOTS_UNSORTED
        @@ [])
      ~write:(fun l ->
        List.fold_left
          (fun acc -> function
            | QQBAR_ROOTS_IRREDUCIBLE -> Int.logor acc qqbar_roots_irreducible
            | QQBAR_ROOTS_UNSORTED -> Int.logor acc qqbar_roots_unsorted)
          0 l)

  let qqbar_roots_fmpz_poly =
    foreign "qqbar_roots_fmpz_poly"
      (qqbar_t @-> fmpz_poly_t @-> flag_qqbar_roots @-> returning void)

  let qqbar_validate_existence_uniqueness =
    foreign "_qqbar_validate_existence_uniqueness"
      (ACB.t @-> fmpz_poly_t @-> ACB.t @-> long @-> returning bool)

  let qqbar_set = foreign "qqbar_set" (qqbar_t @-> qqbar_t @-> returning void)
  let qqbar_swap = foreign "qqbar_swap" (qqbar_t @-> qqbar_t @-> returning void)

  let qqbar_equal =
    foreign "qqbar_equal" (qqbar_t @-> qqbar_t @-> returning bool)

  let qqbar_hash = foreign "qqbar_hash" (qqbar_t @-> returning ulong)

  let qqbar_cmp_root_order =
    foreign "qqbar_cmp_root_order" (qqbar_t @-> qqbar_t @-> returning int)

  let qqbar_is_real = foreign "qqbar_is_real" (qqbar_t @-> returning bool)
  let qqbar_is_zero = foreign "qqbar_is_zero" (qqbar_t @-> returning bool)
  let qqbar_is_one = foreign "qqbar_is_one" (qqbar_t @-> returning bool)
  let qqbar_print = foreign "qqbar_print" (qqbar_t @-> returning void)
end

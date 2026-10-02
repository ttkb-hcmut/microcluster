open Ppxlib
open Ppxlib_ast.Parsetree
module A = Ast_builder.Default

module Package_type = struct
  let to_module_type ~loc ((ident, constraints) : package_type) =
    let (module A) = Ast_builder.make loc in
    let v desc = { pmty_loc = loc; pmty_attributes = []; pmty_desc = desc } in
    let ptype__v ~name manifest = { ptype_name = { txt = name ; loc }; ptype_loc = loc; ptype_attributes = []; ptype_params = []; ptype_cstrs = []; ptype_kind = Ptype_abstract; ptype_private = Public; ptype_manifest = Some manifest } in
    v @@ Pmty_with ((v @@ Pmty_ident ident), constraints |> List.map @@ fun (ident, b) ->
      let ident' = let { txt = ident; loc = _} = ident in match ident with Ldot _ | Lapply _ -> failwith "???" | Lident x -> x in
      Pwith_type (ident, ptype__v ~name:ident' b))
end

let expand__ftor_imm ~ctxt =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let (module A) = Ast_builder.make loc in
  let a ({ ppat_desc = p ; _ }) body =
  match p with
  | Ppat_constraint ({ ppat_desc = Ppat_unpack ftor_param_name ; _ }, { ptyp_desc = Ptyp_package ftor_param_sig ; _ }) ->
    A.pmod_functor (Named (ftor_param_name, Package_type.to_module_type ~loc ftor_param_sig)) (
      match body.pexp_desc with
      (* | Pexp_fun (_, { pc_rhs = body; pc_lhs = p; pc_guard = _ } :: paramsrest) -> a p body ~paramsrest ~xx *)
      | _ ->
        A.pmod_structure [
          A.pstr_value Nonrecursive [A.value_binding ~pat:(A.ppat_var { txt = "v" ; loc }) ~expr:(body)]
        ]
    )
  | _ -> failwith "wtf" in a

let () =
  let ftor_imm ?(name = "ftor") () =
    Extension.V3.declare
      name
      Extension.Context.expression
      Ast_pattern.(single_expr_payload (pexp_fun __ __ __ __))
      (fun ~ctxt lbl exp0 p body ->
        match body with
        | body ->
        ( match lbl, exp0 with
        | Nolabel, None ->
          let loc = Expansion_context.Extension.extension_point_loc ctxt in
          let (module A) = Ast_builder.make loc in
          A.pexp_pack (expand__ftor_imm ~ctxt p body)
        | _ -> failwith "shit"
        )
      )
    |> Context_free.Rule.extension
  and ftor_imm_let ?(name = "ftor_") () =
    Extension.V3.declare
      name
      Extension.Context.structure_item
      Ast_pattern.(pstr __)
      (fun ~ctxt ->
        ( function
        | [{ pstr_desc = Pstr_value (_recflag, bindings); pstr_loc }] ->
          let funname, nameloc, module_expr, attribs, loc =
          ( match bindings with
          | [{ pvb_pat = { ppat_desc; _ }; pvb_expr = { pexp_desc; _ }; pvb_attributes = attribs; pvb_loc = loc }] ->
            ( match ppat_desc, pexp_desc with
            | Ppat_var { txt = funname; loc = nameloc }, Pexp_function ({ pc_guard = _exp0; pc_lhs = pp; pc_rhs = body } :: _) ->
              (funname, nameloc, expand__ftor_imm ~ctxt pp body, attribs, loc)
            | _, _ -> failwith "sslol"
            )
          | _ -> failwith "lol"
          ) in
          let funname =
            String.cat
              (Char.escaped @@ Char.uppercase_ascii @@ String.get funname 0)
              (String.sub funname 1 (String.length funname - 1)) in
          { pstr_desc = Pstr_module { pmb_name = { txt = Some funname; loc = nameloc }; pmb_expr = module_expr; pmb_attributes = attribs; pmb_loc = loc }; pstr_loc }
        | _ -> failwith "shit"
        )
      )
    |> Context_free.Rule.extension
  in
  Driver.register_transformation
    ~rules:[ ftor_imm (); ftor_imm ~name:"functor" (); ftor_imm_let (); ftor_imm_let ~name:"functor_" () ] "ppx-ftor"

(* Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Lwt
open Caqti_lwt

let test_fx (module C : CONNECTION) =
  let module Q = Test_sql_fx.Make (C) in
  Q.create () >>
  Q.insert true 1.0 "" >>
  Q.insert false 3.141592654 "" >>
  (Q.count_all () >|= fun c -> assert (c = 2)) >>
  (Q.fold_select_ge (fun _ _ _ _ -> succ) 2.0 4 >|= fun c -> assert (c = 5))

let test_Cfx conn =
  let module Q = Test_sql_Cfx in
  Q.create conn >>
  Q.insert conn true 1.0 "" >>
  Q.insert conn true 2.0 "" >>
  Q.insert conn true 3.0 "" >>
  Q.insert conn true 4.0 "" >>
  (Q.count_ge conn 1.5 >|= fun c -> assert (c = 3)) >>
  Q.iter_s_select_ge conn (fun _ _ _ _ -> return_unit) 1.5

let test_fxC conn =
  let module Q = Test_sql_fxC in
  Q.create conn >>
  Q.insert true 1.0 "" conn >>
  Q.insert true 2.0 "" conn >>
  (Q.count_ge 1.5 conn >|= fun c -> assert (c = 1)) >>
  Q.iter_s_select_ge (fun _ _ _ _ -> return_unit) 1.5 conn

let () = Lwt_main.run
  begin
    Dynlink.allow_unsafe_modules true;
    let uri = Uri.of_string "sqlite3:" in
    connect uri >>= test_fx >>
    connect uri >>= test_Cfx >>
    connect uri >>= test_fxC >>
    return ()
  end

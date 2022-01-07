(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2022 Steven de Oliveira <de.oliveira.steven@gmail.com>  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

let is_int str =
  try ignore (int_of_string str); true with _ -> false

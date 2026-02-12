(* SPDX-License-Identifier: Apache-2.0 *)
(* SPDX-FileCopyrightText: 2025 Hyperpolymath *)

(** Safe color manipulation with clamped values. *)

(** RGB color with values 0-255. *)
type rgb = {
  r: int;
  g: int;
  b: int;
}

(** RGBA color with alpha 0.0-1.0. *)
type rgba = {
  r: int;
  g: int;
  b: int;
  a: float;
}

(** HSL color (hue 0-360, saturation/lightness 0.0-1.0). *)
type hsl = {
  h: float;
  s: float;
  l: float;
}

(** HSLA color with alpha. *)
type hsla = {
  h: float;
  s: float;
  l: float;
  a: float;
}

type error =
  | Invalid_format
  | Invalid_hex
  | Value_out_of_range

(** Clamp an integer to 0-255. *)
let clamp_byte v = max 0 (min 255 v)

(** Clamp a float to 0.0-1.0. *)
let clamp_unit v = Float.max 0.0 (Float.min 1.0 v)

(** Create RGB color with clamping. *)
let rgb r g b =
  { r = clamp_byte r; g = clamp_byte g; b = clamp_byte b }

(** Create RGBA color with clamping. *)
let rgba r g b a =
  { r = clamp_byte r; g = clamp_byte g; b = clamp_byte b; a = clamp_unit a }

(** Create HSL color with clamping. *)
let hsl h s l =
  let h = Float.rem h 360.0 in
  let h = if h < 0.0 then h +. 360.0 else h in
  { h; s = clamp_unit s; l = clamp_unit l }

(** Create HSLA color with clamping. *)
let hsla h s l a =
  let h = Float.rem h 360.0 in
  let h = if h < 0.0 then h +. 360.0 else h in
  { h; s = clamp_unit s; l = clamp_unit l; a = clamp_unit a }

(** Parse hex color string (#RGB, #RRGGBB, #RGBA, #RRGGBBAA). *)
let parse_hex str =
  let str = String.trim str in
  let str = if String.length str > 0 && str.[0] = '#' then
    String.sub str 1 (String.length str - 1)
  else str in
  let parse_byte hex =
    try int_of_string ("0x" ^ hex)
    with _ -> -1
  in
  let len = String.length str in
  if len = 3 || len = 4 then
    (* Short form: #RGB or #RGBA *)
    let r = parse_byte (String.make 2 str.[0]) in
    let g = parse_byte (String.make 2 str.[1]) in
    let b = parse_byte (String.make 2 str.[2]) in
    if r < 0 || g < 0 || b < 0 then Error Invalid_hex
    else if len = 3 then
      Ok (rgba r g b 1.0)
    else
      let a = parse_byte (String.make 2 str.[3]) in
      if a < 0 then Error Invalid_hex
      else Ok (rgba r g b (float_of_int a /. 255.0))
  else if len = 6 || len = 8 then
    (* Long form: #RRGGBB or #RRGGBBAA *)
    let r = parse_byte (String.sub str 0 2) in
    let g = parse_byte (String.sub str 2 2) in
    let b = parse_byte (String.sub str 4 2) in
    if r < 0 || g < 0 || b < 0 then Error Invalid_hex
    else if len = 6 then
      Ok (rgba r g b 1.0)
    else
      let a = parse_byte (String.sub str 6 2) in
      if a < 0 then Error Invalid_hex
      else Ok (rgba r g b (float_of_int a /. 255.0))
  else
    Error Invalid_format

(** Format RGB as hex string. *)
let to_hex color =
  Printf.sprintf "#%02x%02x%02x" color.r color.g color.b

(** Format RGBA as hex string with alpha. *)
let to_hex_rgba color =
  let alpha_byte = int_of_float (color.a *. 255.0) in
  Printf.sprintf "#%02x%02x%02x%02x" color.r color.g color.b alpha_byte

(** Format RGB as CSS rgb() function. *)
let to_css_rgb color =
  Printf.sprintf "rgb(%d, %d, %d)" color.r color.g color.b

(** Format RGBA as CSS rgba() function. *)
let to_css_rgba color =
  Printf.sprintf "rgba(%d, %d, %d, %.3f)" color.r color.g color.b color.a

(** Format HSL as CSS hsl() function. *)
let to_css_hsl color =
  Printf.sprintf "hsl(%.1f, %.1f%%, %.1f%%)"
    color.h (color.s *. 100.0) (color.l *. 100.0)

(** Convert RGB to HSL. *)
let rgb_to_hsl color =
  let r = float_of_int color.r /. 255.0 in
  let g = float_of_int color.g /. 255.0 in
  let b = float_of_int color.b /. 255.0 in
  let max_c = Float.max r (Float.max g b) in
  let min_c = Float.min r (Float.min g b) in
  let l = (max_c +. min_c) /. 2.0 in
  if max_c = min_c then
    { h = 0.0; s = 0.0; l }
  else
    let d = max_c -. min_c in
    let s = if l > 0.5 then d /. (2.0 -. max_c -. min_c) else d /. (max_c +. min_c) in
    let h =
      if max_c = r then
        (g -. b) /. d +. (if g < b then 6.0 else 0.0)
      else if max_c = g then
        (b -. r) /. d +. 2.0
      else
        (r -. g) /. d +. 4.0
    in
    { h = h *. 60.0; s; l }

(** Convert HSL to RGB. *)
let hsl_to_rgb color =
  let h = color.h /. 360.0 in
  let s = color.s in
  let l = color.l in
  if s = 0.0 then
    let v = int_of_float (l *. 255.0) in
    { r = v; g = v; b = v }
  else
    let hue_to_rgb p q t =
      let t = if t < 0.0 then t +. 1.0 else if t > 1.0 then t -. 1.0 else t in
      if t < 1.0 /. 6.0 then p +. (q -. p) *. 6.0 *. t
      else if t < 1.0 /. 2.0 then q
      else if t < 2.0 /. 3.0 then p +. (q -. p) *. (2.0 /. 3.0 -. t) *. 6.0
      else p
    in
    let q = if l < 0.5 then l *. (1.0 +. s) else l +. s -. l *. s in
    let p = 2.0 *. l -. q in
    let r = hue_to_rgb p q (h +. 1.0 /. 3.0) in
    let g = hue_to_rgb p q h in
    let b = hue_to_rgb p q (h -. 1.0 /. 3.0) in
    { r = int_of_float (r *. 255.0);
      g = int_of_float (g *. 255.0);
      b = int_of_float (b *. 255.0) }

(** Lighten a color by a percentage (0.0-1.0). *)
let lighten amount color =
  let h = rgb_to_hsl color in
  let new_l = clamp_unit (h.l +. amount) in
  hsl_to_rgb { h with l = new_l }

(** Darken a color by a percentage (0.0-1.0). *)
let darken amount color =
  let h = rgb_to_hsl color in
  let new_l = clamp_unit (h.l -. amount) in
  hsl_to_rgb { h with l = new_l }

(** Saturate a color by a percentage. *)
let saturate amount color =
  let h = rgb_to_hsl color in
  let new_s = clamp_unit (h.s +. amount) in
  hsl_to_rgb { h with s = new_s }

(** Desaturate a color by a percentage. *)
let desaturate amount color =
  let h = rgb_to_hsl color in
  let new_s = clamp_unit (h.s -. amount) in
  hsl_to_rgb { h with s = new_s }

(** Rotate hue by degrees. *)
let rotate_hue degrees color =
  let h = rgb_to_hsl color in
  let new_h = Float.rem (h.h +. degrees) 360.0 in
  let new_h = if new_h < 0.0 then new_h +. 360.0 else new_h in
  hsl_to_rgb { h with h = new_h }

(** Get complementary color (180 degree rotation). *)
let complement color =
  rotate_hue 180.0 color

(** Invert a color. *)
let invert color =
  { r = 255 - color.r; g = 255 - color.g; b = 255 - color.b }

(** Grayscale conversion using luminosity method. *)
let grayscale color =
  let gray = int_of_float (
    float_of_int color.r *. 0.299 +.
    float_of_int color.g *. 0.587 +.
    float_of_int color.b *. 0.114
  ) in
  { r = gray; g = gray; b = gray }

(** Mix two colors with a weight (0.0 = all first, 1.0 = all second). *)
let mix weight c1 c2 =
  let w = clamp_unit weight in
  let mix_channel a b = int_of_float (
    float_of_int a *. (1.0 -. w) +. float_of_int b *. w
  ) in
  { r = mix_channel c1.r c2.r;
    g = mix_channel c1.g c2.g;
    b = mix_channel c1.b c2.b }

(** Calculate contrast ratio between two colors (WCAG). *)
let contrast_ratio c1 c2 =
  let relative_luminance c =
    let channel v =
      let v = float_of_int v /. 255.0 in
      if v <= 0.03928 then v /. 12.92
      else Float.pow ((v +. 0.055) /. 1.055) 2.4
    in
    0.2126 *. channel c.r +. 0.7152 *. channel c.g +. 0.0722 *. channel c.b
  in
  let l1 = relative_luminance c1 in
  let l2 = relative_luminance c2 in
  let lighter = Float.max l1 l2 in
  let darker = Float.min l1 l2 in
  (lighter +. 0.05) /. (darker +. 0.05)

(** Check if colors have sufficient contrast for text (WCAG AA: 4.5:1). *)
let is_readable ?(level=4.5) c1 c2 =
  contrast_ratio c1 c2 >= level

(** Get a readable text color (black or white) for a background. *)
let text_color_for_background bg =
  let luminance =
    float_of_int bg.r *. 0.299 +.
    float_of_int bg.g *. 0.587 +.
    float_of_int bg.b *. 0.114
  in
  if luminance > 128.0 then { r = 0; g = 0; b = 0 }
  else { r = 255; g = 255; b = 255 }

(** Common colors. *)
let black = { r = 0; g = 0; b = 0 }
let white = { r = 255; g = 255; b = 255 }
let red = { r = 255; g = 0; b = 0 }
let green = { r = 0; g = 128; b = 0 }
let blue = { r = 0; g = 0; b = 255 }
let yellow = { r = 255; g = 255; b = 0 }
let cyan = { r = 0; g = 255; b = 255 }
let magenta = { r = 255; g = 0; b = 255 }
let orange = { r = 255; g = 165; b = 0 }
let purple = { r = 128; g = 0; b = 128 }
let pink = { r = 255; g = 192; b = 203 }
let gray = { r = 128; g = 128; b = 128 }
let transparent = { r = 0; g = 0; b = 0; a = 0.0 }

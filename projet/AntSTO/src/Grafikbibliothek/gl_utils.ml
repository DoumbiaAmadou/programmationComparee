open Tsdl
open Tgl4

let check = function
  | `Ok x -> x
  | `Error msg -> failwith msg

let option_get = function
  | None -> failwith "option_get"
  | Some x -> x

let sprintf = Printf.sprintf

let gl_set_attribute x y = check (Sdl.gl_set_attribute x y)

let join l = String.concat "\n" l

let init_scene () =
  check @@ Sdl.init Sdl.Init.(timer + video);
  at_exit Sdl.quit;

  let width = 640 and height = 480 in
  gl_set_attribute Sdl.Gl.context_profile_mask Sdl.Gl.context_profile_core;
  gl_set_attribute Sdl.Gl.context_major_version 3;
  gl_set_attribute Sdl.Gl.context_minor_version 0;

  (* Do double buffering in GL *)
  gl_set_attribute Sdl.Gl.doublebuffer 1;
  gl_set_attribute Sdl.Gl.depth_size 24;

  let window = check @@
    Sdl.create_window "Shader example" ~w:width ~h:height
      Sdl.Window.(opengl + shown + resizable)
  in

  let context = check @@ Sdl.gl_create_context window in

  window, context

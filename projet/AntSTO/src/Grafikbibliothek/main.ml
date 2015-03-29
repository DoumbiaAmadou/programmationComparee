open Tgl4
open Tsdl

open Gl_utils
open Math_funcs
open Hot_n_cold

let flip_image {Stb_image. channels; width; height; data} =
  let stride = width * channels in
  let half_height = height / 2 in
  for row = 0 to half_height - 1 do
    let top = stride * row in
    let bot = stride * (height - row - 1) in
    for col = 0 to stride - 1 do
      let a = data.{top + col} in
      let b = data.{bot + col} in
      data.{top + col} <- b;
      data.{bot + col} <- a;
    done
  done

let load_texture filename = match Stb_image.load ~channels:4 filename with
  | None -> failwith ("ERROR: could not load " ^ filename)
  | Some image ->
    let open Stb_image in
    if (image.width land (image.width - 1) <> 0) ||
       (image.height land (image.height - 1) <> 0) then
      Printf.eprintf "WARNING: texture %S is not power-of-2 dimensions\n"
        filename;
    flip_image image;
    let tex = Lo.Gen.tex ~name:filename () in
    Gl.active_texture Gl.texture0;
    Gl.bind_texture Gl.texture_2d !!tex;
    Gl.tex_image2d
      Gl.texture_2d
      0
      Gl.rgba
      image.width
      image.height
      0
      Gl.rgba
      Gl.unsigned_byte
      (`Data image.data);
    Gl.generate_mipmap Gl.texture_2d;
    Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_s Gl.clamp_to_edge;
    Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_t Gl.clamp_to_edge;
    Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter Gl.linear;
    Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter Gl.linear_mipmap_linear;
    (* TODO: setup anisotropic filter *)
    tex

let main () =
  let window, context = init_scene () in
  let width = ref 640 and height = ref 480 in

	(* get version info *)
  Printf.printf "Renderer: %s\n%!"
    (option_get @@ Gl.get_string Gl.renderer);
  Printf.printf "OpenGL version supported %s\n%!"
    (option_get @@ Gl.get_string Gl.version);
  Hot_n_cold.log_gl_params ();

  (* tell GL to only draw onto a pixel if the shape is closer to the viewer *)
  Render.depth (Some `less);

  (*----------------------------create geometry-----------------------------*)
  let points = Buffer.gpu Buffer.f32 in
  Buffer.upload Buffer.(of_array f32 [|
      -0.5; -0.5; +0.0;
      +0.5; -0.5; +0.0;
      +0.5; +0.5; +0.0;
      +0.5; +0.5; +0.0;
      -0.5; +0.5; +0.0;
      -0.5; -0.5; +0.0;
    |]) points;

  let texcoords = Buffer.gpu Buffer.f32 in
  Buffer.upload Buffer.(of_array f32 [|
      0.0; 0.0;
      1.0; 0.0;
      1.0; 1.0;
      1.0; 1.0;
      0.0; 1.0;
      0.0; 0.0;
    |]) texcoords;

  let uniforms = Uniform.(mat4 "view" ** mat4 "proj" ** sampler2d "basic_texture" ** nil) in
  let attribs = Attrib.(vec3 f32 "vertex_position" ** vec2 f32 "vt" ** nil) in
  let vao = Attrib.instance attribs in
  Attrib.update vao Values.(buf points ** buf texcoords ** ());

  let pass =
    let vs = Lo.Shader.vertex   @@ file_contents "test_vs.glsl" in
    let fs = Lo.Shader.fragment @@ file_contents "test_fs.glsl" in
    let pass = Render.pass uniforms attribs vs fs in
    Handle.release vs;
    Handle.release fs;
    pass
  in

  (* input variables *)
  let near = 0.1 in (* clipping plane *)
  let far  = 100.0 in (* clipping plane *)
  let fov  = 67.0 *. one_deg_in_rad in
  let aspect = float_of_int !width /. float_of_int !height in (* aspect ratio *)
  let range = tan (fov *. 0.5) *. near in
  let sx = near /. (range *. aspect) in
  let sy = near /. range in
  let sz = -. (far +. near) /. (far -. near) in
  let pz = -. (2.0 *. far *. near) /. (far -. near) in

  let proj_mat = M4.make
       sx 0.0 0.0   0.0 (* first column  *)
      0.0  sy 0.0   0.0 (* second column *)
      0.0 0.0  sz (-1.0) (* third column  *)
      0.0 0.0  pz   0.0 (* fourth column *)
  in

  (* create view matrix *)
  let cam_speed = 1.0 in
  let cam_yaw_speed = 10.0 in
  let cam_pos = ref {V3. x = 0.0; y = 0.0; z = 2.0} in
  let cam_yaw = ref 0.0 in

  let view_mat () =
    M4.(rotate_y_deg (-. !cam_yaw) *% translate (V3.neg !cam_pos)) in

  (* load texture *)
  let tex = load_texture "skulluvmap.png" in

  Gl.enable Gl.cull_face_enum;
  Gl.cull_face Gl.back;
  Gl.front_face Gl.ccw;

  let event = Sdl.Event.create () in
  let update_fps_counter = fps_counter () in
  let frame_timer = frame_timer 60 in

  let previous_seconds = ref (Unix.gettimeofday ()) in

  (* The main event loop *)
  let rec loop () =
    let is_done =
      if Sdl.wait_event_timeout (Some event) (frame_timer ()) then
        match Sdl.Event.(enum (get event typ)) with
        | `Quit -> true
        | `Window_event
          when Sdl.Event.(get event window_event_id = window_event_resized) ->
          let w = Sdl.Event.(get event window_data1) in
          let h = Sdl.Event.(get event window_data2) in
          width := Int32.to_int w;
          height := Int32.to_int h;
          false
        | _ -> false
      else
        false
    in
    if is_done then ()
    else
      begin
        let current_seconds = Unix.gettimeofday () in
        let elapsed_seconds = current_seconds -. !previous_seconds in
        previous_seconds := current_seconds;

        Sdl.set_window_title
          window (sprintf "%.02f fps" @@ update_fps_counter ());
        Render.clear [`depth; `color];
        Render.viewport 0 0 !width !height;

        Render.render pass `triangles ~count:6
          Values.(view_mat () ** proj_mat ** tex ** ()) vao;

        let cam_moved = ref false in
        let check_key =
          let keys = Sdl.get_keyboard_state () in
          fun k -> keys.{k} <> 0
        in
        let check_cam_key k =
          let result = check_key k in
          if result then cam_moved := true;
          result
        in
        if check_cam_key Sdl.Scancode.a then
          cam_pos := {!cam_pos with V3.x = !cam_pos.V3.x -. cam_speed *. elapsed_seconds};
        if check_cam_key Sdl.Scancode.d then
          cam_pos := {!cam_pos with V3.x = !cam_pos.V3.x +. cam_speed *. elapsed_seconds};
        if check_cam_key Sdl.Scancode.pageup then
          cam_pos := {!cam_pos with V3.y = !cam_pos.V3.y +. cam_speed *. elapsed_seconds};
        if check_cam_key Sdl.Scancode.pagedown then
          cam_pos := {!cam_pos with V3.y = !cam_pos.V3.y -. cam_speed *. elapsed_seconds};
        if check_cam_key Sdl.Scancode.w then
          cam_pos := {!cam_pos with V3.z = !cam_pos.V3.z -. cam_speed *. elapsed_seconds};
        if check_cam_key Sdl.Scancode.s then
          cam_pos := {!cam_pos with V3.z = !cam_pos.V3.z +. cam_speed *. elapsed_seconds};
        if check_cam_key Sdl.Scancode.left then
          cam_yaw := !cam_yaw +. cam_yaw_speed *. elapsed_seconds;
        if check_cam_key Sdl.Scancode.right then
          cam_yaw := !cam_yaw -. cam_yaw_speed *. elapsed_seconds;

        (* put the stuff we've been drawing onto the display *)
        Sdl.gl_swap_window window;
        loop ()
      end
  in
  loop ()

let () = main ()

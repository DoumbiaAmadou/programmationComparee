open Tgl4
open Math_funcs

(* Misc *)

type name      = string

let small_buf = Bigarray.(Array1.create int32 c_layout 8)

let char_buf_sz = 2048
let char_buf = Bigarray.(Array1.create Char c_layout char_buf_sz)
let char_buf_str ?sz () =
  let sz = match sz with
    | Some sz -> sz
    | None ->
      let sz = ref 0 in
      begin try for i = 0 to char_buf_sz - 1 do
            if char_buf.{i} = '\000' then
              (sz := i; raise Not_found)
          done
        with Not_found -> ()
      end;
      !sz
  in
  String.init sz (Bigarray.Array1.get char_buf)

let slice a b arr = Bigarray.Array1.sub arr a b

let shader_kind = function
  | `vertex -> Gl.vertex_shader
  | `fragment -> Gl.fragment_shader

let shader_kind_string = function
  | `vertex -> "vertex"
  | `fragment -> "fragment"

let gl_type_to_string ty =
  try List.assoc ty
        [ Gl.bool              , "bool"
        ; Gl.int               , "int"
        ; Gl.float             , "float"
        ; Gl.float_vec2        , "float_vec2"
        ; Gl.float_vec3        , "float_vec3"
        ; Gl.float_vec4        , "float_vec4"
        ; Gl.float_mat2        , "float_mat2"
        ; Gl.float_mat3        , "float_mat3"
        ; Gl.float_mat4        , "float_mat4"
        ; Gl.sampler_2d        , "sampler_2d"
        ; Gl.sampler_3d        , "sampler_3d"
        ; Gl.sampler_cube      , "sampler_cube"
        ; Gl.sampler_2d_shadow , "sampler_2d_shadow"
        ]
  with Not_found -> "other"

let log_gl_params () =
  let log_gl_int_param (param,name) =
    Gl.get_integerv param small_buf;
    Printf.eprintf "%s %ld\n" name small_buf.{0}
  in
  Printf.eprintf "GL Context Params:\n";
  (* integers - only works if the order is 0-10 integer return types *)
  List.iter log_gl_int_param [
    Gl.max_combined_texture_image_units, "GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS";
    Gl.max_cube_map_texture_size       , "GL_MAX_CUBE_MAP_TEXTURE_SIZE";
    Gl.max_draw_buffers                , "GL_MAX_DRAW_BUFFERS";
    Gl.max_fragment_uniform_components , "GL_MAX_FRAGMENT_UNIFORM_COMPONENTS";
    Gl.max_texture_image_units         , "GL_MAX_TEXTURE_IMAGE_UNITS";
    Gl.max_texture_size                , "GL_MAX_TEXTURE_SIZE";
    Gl.max_varying_floats              , "GL_MAX_VARYING_FLOATS";
    Gl.max_vertex_attribs              , "GL_MAX_VERTEX_ATTRIBS";
    Gl.max_vertex_texture_image_units  , "GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS";
    Gl.max_vertex_uniform_components   , "GL_MAX_VERTEX_UNIFORM_COMPONENTS";
  ];
  Gl.get_integerv Gl.max_viewport_dims small_buf;
  Printf.eprintf "GL_MAX_VIEWPORT_DIMS %ld %ld\n" small_buf.{0} small_buf.{1};
  let tmp = Bigarray.(Array1.create int8_unsigned c_layout 1) in
  Gl.get_booleanv Gl.stereo tmp;
  Printf.eprintf "GL_STEREO %b\n" (tmp.{0} <> 0)

let file_contents fname =
  let ic = open_in fname in
  let rec aux acc =
    match input_line ic with
    | line -> aux (line :: acc)
    | exception End_of_file -> List.rev acc
  in
  let lines = aux [] in
  close_in_noerr ic;
  String.concat "\n" lines

let log_error = Printf.eprintf
let sprintf = Printf.sprintf

let fps_counter () =
  let frame_count = ref 0 in
  let previous_seconds = ref (Unix.gettimeofday ()) in
  let fps = ref 0. in
  fun () ->
    let current_seconds = Unix.gettimeofday () in
    let elapsed_seconds = current_seconds -. !previous_seconds in
    if (elapsed_seconds > 0.25) then
      begin
        previous_seconds := current_seconds;
        fps := float_of_int !frame_count /. elapsed_seconds;
        frame_count := 0
      end
    else
      incr frame_count;
    !fps

let frame_timer fps =
  let tick = ref (Unix.gettimeofday ()) in
  let delta = 1. /. float_of_int fps in
  fun () ->
    let now = Unix.gettimeofday () in
    let tick' = !tick +. delta in
    let delay =
      if now > tick' then
        (tick := now; 0.)
      else if now > !tick then
        (tick := tick'; tick' -. now)
      else
        (!tick -. now)
    in
    int_of_float (delay *. 1000.)


(** All handles to OpenGL resources are represented as integers.
    Add resource management on top, to ensure safe-use and prevent or detect
    leaking. *)
module Handle : sig
  type desc = { kind: name; release: int -> unit }
  val desc : name -> (int -> unit) -> desc

  type +'a t
  val handle: ?name:name -> desc -> int -> _ t
  val get: _ t -> int
  val release: _ t -> unit
  exception Released of desc * name

  val describe: _ t -> desc * name
end = struct
  type desc = { kind: string; release: int -> unit }
  type 'a t = { desc: desc; mutable resource: int; name: string }

  let desc kind release = {kind; release}

  let release t =
    if t.resource <> -1 then
      (t.desc.release t.resource;
       t.resource <- -1)

  let handle ?(name="") desc resource =
    let t = {desc; resource; name} in
    Gc.finalise release t;
    t

  exception Released of desc * name

  let get = function
    | { desc; resource = -1; name} -> raise (Released (desc, name))
    | { resource } -> resource

  let describe { desc; name } = desc, name

  let () =
    Printexc.register_printer (function
        | Released (desc, name) ->
          Some (sprintf "Released ({ kind = %S }, %S)" desc.kind name)
        | _ -> None)
end
type 'a gl = 'a Handle.t
let (!!) = Handle.get

type tex
type vbo
type vao
type 'a shader
type program

(* Low-level interface *)
module Lo = struct

  module Gen : sig
    val tex : ?name:name -> unit -> tex gl
    val vbo : ?name:name -> unit -> vbo gl
    val vao : ?name:name -> unit -> vao gl
    val shader : ?name:name -> [`vertex|`fragment] -> _ shader gl
    val program : ?name:name -> unit -> program gl
  end = struct
    let desc_tex =
      Handle.desc "tex" (fun n ->
          small_buf.{0} <- Int32.of_int n;
          Gl.delete_textures 1 small_buf)

    let desc_vbo =
      Handle.desc "vbo" (fun n ->
          small_buf.{0} <- Int32.of_int n;
          Gl.delete_buffers 1 small_buf)

    let desc_vao =
      Handle.desc "vao" (fun n ->
          small_buf.{0} <- Int32.of_int n;
          Gl.delete_vertex_arrays 1 small_buf)

    let desc_shader =
      Handle.desc "shader" Gl.delete_shader

    let desc_program =
      Handle.desc "shader" Gl.delete_program

    let tex ?name () =
      Gl.gen_textures 1 small_buf;
      Handle.handle ?name desc_tex (Int32.to_int small_buf.{0})

    let vbo ?name () =
      Gl.gen_buffers 1 small_buf;
      Handle.handle ?name desc_vbo (Int32.to_int small_buf.{0})

    let vao ?name () =
      Gl.gen_vertex_arrays 1 small_buf;
      Handle.handle ?name desc_vao (Int32.to_int small_buf.{0})

    let shader ?name kind =
      Handle.handle ?name desc_shader (Gl.create_shader (shader_kind kind))

    let program ?name () =
      Handle.handle ?name desc_program (Gl.create_program ())
  end

  module Shader : sig
    val vertex : ?name:name -> string -> [`vertex] shader gl
    val fragment : ?name:name -> string -> [`fragment] shader gl

    val log : 'a shader gl -> string
  end = struct
    let log shader =
      Gl.get_shader_info_log !!shader char_buf_sz (Some small_buf) char_buf;
      ignore shader;
      char_buf_str ~sz:(Int32.to_int small_buf.{0}) ()

    let shader ?name kind source =
      let s = Gen.shader ?name kind in
      Gl.shader_source !!s source;
      Gl.compile_shader !!s;
      Gl.get_shaderiv !!s Gl.compile_status small_buf;
      if Int32.to_int small_buf.{0} <> Gl.true_ then
        log_error "%s shader %S did not compile: %s\n"
          (shader_kind_string kind)
          (match name with Some name -> name | None -> "<no-name>")
          (log s);
      s

    let vertex ?name source = shader ?name `vertex source
    let fragment ?name source = shader ?name `fragment source
  end

  module Program : sig
    (* Shaders can be deleted after linking *)
    val link : ?name:name -> [`vertex] shader gl -> [`fragment] shader gl -> (string * int) list -> program gl

    val log : program gl -> string
    val validate : program gl -> bool
    val debug_dump : ?out:out_channel -> program gl -> unit
  end = struct

    let log program =
      Gl.get_program_info_log !!program char_buf_sz (Some small_buf) char_buf;
      ignore program;
      char_buf_str ~sz:(Int32.to_int small_buf.{0}) ()

    let validate program =
      Gl.validate_program !!program;
      Gl.get_programiv !!program Gl.validate_status small_buf;
      let result = Int32.to_int small_buf.{0} = Gl.true_ in
      if not result then
        log_error "Invalid program: %s\n" (log program);
      ignore program;
      result

    let debug_dump ?(out=stderr) program =
      Printf.fprintf out "--------------------\nshader programme %i info:\n" program;
      Gl.get_programiv program Gl.link_status small_buf;
      Printf.fprintf out "GL_LINK_STATUS = %ld\n" small_buf.{0};

      Gl.get_programiv program Gl.attached_shaders small_buf;
      Printf.fprintf out "GL_ATTACHED_SHADERS = %ld\n" small_buf.{0};

      Gl.get_programiv program Gl.active_attributes small_buf;
      Printf.fprintf out "GL_ACTIVE_ATTRIBUTES = %ld\n" small_buf.{0};

      let attribs = Int32.to_int small_buf.{0} in
      let actual_length = slice 0 1 small_buf in
      let size          = slice 1 1 small_buf in
      let typ           = slice 2 1 small_buf in
      let params        = slice 3 1 small_buf in
      for i = 0 to attribs - 1 do
        Gl.get_active_attrib program i char_buf_sz (Some actual_length) size typ char_buf;
        if size.{0} > 1l then
          for j = 0 to Int32.to_int size.{0} - 1 do
            let long_name = sprintf "%s[%d]" (char_buf_str ()) j in
            let location = Gl.get_attrib_location program long_name in
            Printf.fprintf out "  %d) type:%s name:%s location:%d\n"
              i (gl_type_to_string (Int32.to_int typ.{0})) long_name location

          done
        else
          let name = char_buf_str () in
          let location = Gl.get_attrib_location program name in
          Printf.fprintf out "  %d) type:%s name:%s location:%d\n"
            i (gl_type_to_string (Int32.to_int typ.{0})) name location
      done;

      Gl.get_programiv program Gl.active_uniforms params;
      let params = Int32.to_int params.{0} in
      Printf.fprintf out "GL_ACTIVE_UNIFORMS = %d\n" params;

      for i = 0 to params - 1 do
        Gl.get_active_uniform program i char_buf_sz (Some actual_length) size typ char_buf;
        if size.{0} > 1l then
          for j = 0 to Int32.to_int size.{0} - 1 do
            let long_name = sprintf "%s[%d]" (char_buf_str ()) j in
            let location = Gl.get_uniform_location program long_name in
            Printf.fprintf out "  %d) type:%s name:%s location:%d\n"
              i (gl_type_to_string (Int32.to_int typ.{0})) long_name location
          done
        else
          let name = char_buf_str () in
          let location = Gl.get_uniform_location program name in
          Printf.fprintf out "  %d) type:%s name:%s location:%d\n"
            i (gl_type_to_string (Int32.to_int typ.{0})) name location
      done

    let debug_dump ?out program =
      debug_dump ?out !!program;
      ignore program

    let link ?(name="name") vs fs attributes =
      let prog = Gen.program () in
      Gl.attach_shader !!prog !!vs;
      Gl.attach_shader !!prog !!fs;
      let bind_attrib index (name,arity) =
        Gl.bind_attrib_location !!prog index name;
        index + arity
      in
      ignore (List.fold_left bind_attrib 0 attributes : int);
      Gl.link_program !!prog;
      Gl.get_programiv !!prog Gl.link_status small_buf;
      if Int32.to_int small_buf.{0} <> Gl.true_ then
        begin
          log_error "ERROR: could not link shader program %S: %s\n" name (log prog);
          debug_dump prog
        end;
      (* FIXME: eventually raise exception, but don't fail here *)
      assert (validate prog);
      Gl.detach_shader !!prog !!vs;
      Gl.detach_shader !!prog !!fs;
      prog
  end
end

module Uniform : sig
  type 'a ty
  type 'a obj

  (* Value types *)
  val int:  string -> int ty * string
  val uint: string -> int ty * string
  val bool: string -> bool ty * string
  val float:  string -> float ty * string
  val double: string -> float ty * string
  val vec2: string -> V2.t ty * string
  val vec3: string -> V3.t ty * string
  val vec4: string -> V4.t ty * string
  val mat3: string -> M3.t ty * string
  val mat4: string -> M4.t ty * string

  (* Sampler types *)
  val sampler1d: string -> tex gl ty * string
  val sampler2d: string -> tex gl ty * string
  val sampler3d: string -> tex gl ty * string
  val sampler_cube: string -> tex gl ty * string
  val sampler2d_rect: string -> tex gl ty * string
  val sampler1d_array: string -> tex gl ty * string
  val sampler2d_array: string -> tex gl ty * string
  (*val sampler_cube_array*)
  (*val sampler_buffer*)
  (*val sampler_2dms*)
  (*val sampler_2dms_array*)

  (* Type tuples *)
  type 'a set
  val ( ** ): ('a ty * string) -> 'b set -> ('a * 'b) set
  val nil: unit set

  val link: program gl -> 'a set -> 'a obj
  val run : 'a obj -> 'a -> unit
end = struct

  type 'a ty =
    | Bool   : bool ty
    | Int    : int ty
    | UInt   : int ty
    | Float  : float ty
    | Double : float ty
    | Vec2   : V2.t ty
    | Vec3   : V3.t ty
    | Vec4   : V4.t ty
    | Mat3   : M3.t ty
    | Mat4   : M4.t ty
    | Sampler1D       : tex gl ty
    | Sampler2D       : tex gl ty
    | Sampler3D       : tex gl ty
    | Sampler_cube     : tex gl ty
    | Sampler2D_rect  : tex gl ty
    | Sampler1D_array : tex gl ty
    | Sampler2D_array : tex gl ty

  type ('tag, 'a) t =
    | Nil : ('tag, unit) t
    | More : 'tag * 'a ty * ('tag, 'b) t -> ('tag, 'a * 'b) t

  type 'a set = (string, 'a) t
  type 'a obj = (int, 'a) t

  let nil = Nil
  let ( ** ) (ty, tag) set = More (tag, ty, set)

  let rec link
    : type a. int -> a set -> a obj
    = fun program -> function
    | Nil -> Nil
    | More (name, ty, set) ->
      (* FIXME: here, we could check that provided type match expected type *)
      More (Gl.get_uniform_location program name, ty, link program set)

  let link program set =
    let result = link !!program set in
    ignore program;
    result

  let run_texture kind loc a tunit =
    Gl.active_texture (Gl.texture0 + tunit);
    Gl.bind_texture kind !!a;
    Gl.uniform1i loc tunit;
    succ tunit

  let run_one (type a) loc (ty : a ty) (tunit : int) (a : a) = match ty with
    | Bool -> Gl.uniform1i loc (if a then 1 else 0); tunit
    | Int -> Gl.uniform1i loc a; tunit
    | UInt -> Gl.uniform1ui loc a; tunit
    | Float -> Gl.uniform1f loc a; tunit
    | Double -> Gl.uniform1d loc a; tunit
    | Vec2 -> Gl.uniform2f loc a.V2.x a.V2.y; tunit
    | Vec3 -> Gl.uniform3f loc a.V3.x a.V3.y a.V3.z; tunit
    | Vec4 -> Gl.uniform4f loc a.V4.x a.V4.y a.V4.z a.V4.w; tunit
    | Mat3 -> Gl.uniform_matrix3fv loc 1 false (M3.m a); tunit
    | Mat4 -> Gl.uniform_matrix4fv loc 1 false (M4.m a); tunit
    | Sampler1D -> run_texture Gl.texture_1d loc a tunit
    | Sampler2D -> run_texture Gl.texture_2d loc a tunit
    | Sampler3D -> run_texture Gl.texture_3d loc a tunit
    | Sampler_cube -> run_texture Gl.texture_cube_map loc a tunit
    | Sampler2D_rect -> run_texture Gl.texture_rectangle loc a tunit
    | Sampler1D_array -> run_texture Gl.texture_1d_array loc a tunit
    | Sampler2D_array -> run_texture Gl.texture_2d_array loc a tunit

  let rec run
    : type a. a obj -> a -> int -> unit
    = fun obj xs tunit -> match obj with
    | Nil -> ()
    | More (loc, ty, set) ->
      let x, xs' = xs in
      let tunit = run_one loc ty tunit x in
      run set xs' tunit
  let run obj xs = run obj xs 0

  let int s = Int, s
  let bool s = Bool, s
  let uint s = UInt, s
  let float s = Float, s
  let double s = Double, s
  let vec2 s = Vec2, s
  let vec3 s = Vec3, s
  let vec4 s = Vec4, s
  let mat3 s = Mat3, s
  let mat4 s = Mat4, s

  let sampler1d s = Sampler1D, s
  let sampler2d s = Sampler2D, s
  let sampler3d s = Sampler3D, s
  let sampler_cube s = Sampler_cube, s
  let sampler2d_rect s = Sampler2D_rect, s
  let sampler1d_array s = Sampler1D_array, s
  let sampler2d_array s = Sampler2D_array, s
end

module Buffer : sig
  type ('a, 'b) kind = ('a, 'b) Bigarray.kind
  type s8 = (int, Bigarray.int8_signed_elt) kind
  val s8 : s8
  type u8 = (int, Bigarray.int8_unsigned_elt) kind
  val u8 : u8
  type s16 = (int, Bigarray.int16_signed_elt) kind
  val s16 : s16
  type u16 = (int, Bigarray.int16_unsigned_elt) kind
  val u16 : u16
  type s32 = (int32, Bigarray.int32_elt) kind
  val s32 : s32
  type f32 = (float, Bigarray.float32_elt) kind
  val f32 : f32
  type f64 = (float, Bigarray.float64_elt) kind
  val f64 : f64

  val size_of: _ kind -> int

  module Cpu = Bigarray.Array1
  type 'kind cpu = ('a, 'b, Bigarray.c_layout) Cpu.t
    constraint 'kind = ('a, 'b) kind

  module Gpu : sig
    type policy = [`static|`dynamic]
    type 'kind t constraint 'kind = ('a, 'b) kind

    val create: ?name:name -> ?policy:policy -> 'kind -> 'kind t
    val import: vbo gl -> ?policy:policy -> 'kind -> 'kind t

    val dim: _ t -> int
    val kind: 'kind t -> 'kind

    val bind: _ t -> unit
  end

  type 'kind gpu = 'kind Gpu.t
  val gpu: ?name:name -> ?policy:Gpu.policy -> 'kind -> 'kind gpu

  val upload: 'a cpu -> 'a gpu -> unit
  val resize: 'a gpu -> int -> unit

  val of_array : ('a, 'b) kind -> 'a array -> ('a, 'b) kind cpu

  val blit: 'a cpu -> ?offset:int -> 'a gpu -> unit
end = struct
  type ('a, 'b) kind = ('a, 'b) Bigarray.kind

  type s8 = (int, Bigarray.int8_signed_elt) Bigarray.kind
  let s8 : s8 = Bigarray.int8_signed
  type u8 = (int, Bigarray.int8_unsigned_elt) Bigarray.kind
  let u8 : u8 = Bigarray.int8_unsigned
  type s16 = (int, Bigarray.int16_signed_elt) Bigarray.kind
  let s16 : s16 = Bigarray.int16_signed
  type u16 = (int, Bigarray.int16_unsigned_elt) Bigarray.kind
  let u16 : u16 = Bigarray.int16_unsigned
  type s32 = (int32, Bigarray.int32_elt) Bigarray.kind
  let s32 : s32 = Bigarray.int32
  type f32 = (float, Bigarray.float32_elt) Bigarray.kind
  let f32 : f32 = Bigarray.float32
  type f64 = (float, Bigarray.float64_elt) Bigarray.kind
  let f64 : f64 = Bigarray.float64

  let size_of (type a) (type b) : (a, b) kind -> int = function
    | Bigarray.Int8_signed    -> 1
    | Bigarray.Int8_unsigned  -> 1
    | Bigarray.Int16_signed   -> 2
    | Bigarray.Int16_unsigned -> 2
    | Bigarray.Int32          -> 4
    | Bigarray.Float32        -> 4
    | Bigarray.Float64        -> 8
    | _ -> invalid_arg "Buffer.size_of"

  module Cpu = Bigarray.Array1
  type 'kind cpu = ('a, 'b, Bigarray.c_layout) Cpu.t
    constraint 'kind = ('a, 'b) kind

  module Gpu = struct
    type policy = [`static|`dynamic]

    type 'kind t = {
      kind: 'kind;
      vbo: vbo gl;
      policy: Gl.enum;
      mutable dim: int;
    } constraint 'kind = ('a, 'b) kind

    let import vbo ?(policy=`static) kind =
      let _ : int = size_of kind in
      let policy = match policy with
        | `static -> Gl.static_draw
        | `dynamic -> Gl.dynamic_draw
      in
      { vbo; policy; kind; dim = 0 }

    let create ?name ?policy k = import (Lo.Gen.vbo ?name ()) ?policy k

    let dim t = t.dim

    let kind t = t.kind

    let bind {vbo} =
      Gl.bind_buffer Gl.array_buffer !!vbo;
      ignore vbo
  end
  type 'kind gpu = 'kind Gpu.t
  let gpu = Gpu.create

  let upload cpu gpu =
    let size = size_of gpu.Gpu.kind in
    Gl.bind_buffer Gl.array_buffer !!(gpu.Gpu.vbo);
    Gl.buffer_data Gl.array_buffer (size * Cpu.dim cpu) (Some cpu) gpu.Gpu.policy;
    gpu.Gpu.dim <- Cpu.dim cpu

  let blit cpu ?(offset=0) gpu =
    let size = size_of gpu.Gpu.kind in
    Gl.bind_buffer Gl.array_buffer !!(gpu.Gpu.vbo);
    Gl.buffer_sub_data Gl.array_buffer (offset * size) (Cpu.dim cpu * size)
      (Some cpu)

  let resize gpu dim =
    let size = size_of gpu.Gpu.kind in
    Gl.bind_buffer Gl.array_buffer !!(gpu.Gpu.vbo);
    Gl.buffer_data Gl.array_buffer (size * dim) None gpu.Gpu.policy;
    gpu.Gpu.dim <- dim

  let of_array kind arr =
    let len = Array.length arr in
    let ba = Cpu.create kind Bigarray.c_layout len in
    for i = 0 to len - 1 do
      Cpu.unsafe_set ba i (Array.unsafe_get arr i)
    done;
    ba

end

module Attrib : sig
  open Buffer

  type 'a scalar
  type 'a buf constraint 'a = _ Bigarray.kind
  type 'a buf3 constraint 'a = _ Bigarray.kind
  type 'a buf4 constraint 'a = _ Bigarray.kind
  type 'a ty

  (* Value types *)
  val s8: s8 scalar
  val u8: u8 scalar
  val s16: s16 scalar
  val u16: u16 scalar
  val s32: s32 scalar
  val u32: s32 scalar
  val f32: f32 scalar
  val f64: f64 scalar

  val scalar: 'a scalar -> string -> 'a buf ty * string
  val vec2: 'a scalar -> string -> 'a buf ty * string
  val vec3: 'a scalar -> string -> 'a buf ty * string
  val vec4: 'a scalar -> string -> 'a buf ty * string

  val mat3: 'a scalar -> string -> 'a buf3 ty * string
  val mat4: 'a scalar -> string -> 'a buf4 ty * string

  (* Describe buffer structure *)
  val gpu_buf : ?offset:int -> ?skip:int -> 'a gpu -> 'a buf
  val contiguous3 : 'a buf -> 'a buf3
  val contiguous4 : 'a buf -> 'a buf4
  val separate3 : 'a buf -> 'a buf -> 'a buf -> 'a buf3
  val separate4 : 'a buf -> 'a buf -> 'a buf -> 'a buf -> 'a buf4

  (* Type tuples *)
  type 'a set
  val nil: unit set
  val ( ** ): ('a ty * string) -> 'b set -> ('a * 'b) set

  type 'a obj
  val link: 'a set -> (string * int) list
  val instance: ?name:name -> 'a set -> 'a obj
  val update : 'a obj -> 'a -> unit
  val bind : 'a obj -> unit
end = struct
  open Buffer

  type _ scalar =
    | S8  : s8 scalar
    | U8  : u8 scalar
    | S16 : s16 scalar
    | U16 : u16 scalar
    | S32 : s32 scalar
    | U32 : s32 scalar
    | F32 : f32 scalar
    | F64 : f64 scalar

  type 'a buf = {gpu: 'a gpu; offset: int; skip: int}
  type 'a buf3 =
    | Contiguous3 of 'a buf
    | Separate3 of 'a buf * 'a buf * 'a buf
  type 'a buf4 =
    | Contiguous4 of 'a buf
    | Separate4 of 'a buf * 'a buf * 'a buf * 'a buf

  let gpu_buf ?(offset=0) ?(skip=0) gpu = { gpu; skip; offset }
  let contiguous3 buf = Contiguous3 buf
  let separate3 b1 b2 b3 = Separate3 (b1, b2, b3)
  let contiguous4 buf = Contiguous4 buf
  let separate4 b1 b2 b3 b4 = Separate4 (b1, b2, b3, b4)

  type 'a ty =
    | Scalar : 'a scalar -> 'a buf ty
    | Vec2   : 'a scalar -> 'a buf ty
    | Vec3   : 'a scalar -> 'a buf ty
    | Vec4   : 'a scalar -> 'a buf ty
    | Mat3   : 'a scalar -> 'a buf3 ty
    | Mat4   : 'a scalar -> 'a buf4 ty

  let s8  = S8
  let u8  = U8
  let s16 = S16
  let u16 = U16
  let s32 = S32
  let u32 = S32
  let f32 = F32
  let f64 = F64

  let scalar t s = Scalar t, s
  let vec2 t s = Vec2 t, s
  let vec3 t s = Vec3 t, s
  let vec4 t s = Vec4 t, s
  let mat3 t s = Mat3 t, s
  let mat4 t s = Mat4 t, s

  type 'a set =
    | Nil : unit set
    | More : string * 'a ty * 'b set -> ('a * 'b) set

  let nil = Nil
  let ( ** ) (ty, tag) set = More (tag, ty, set)

  let arity (type a) : a ty -> int = function
    | Scalar _ -> 1
    | Vec2 _ -> 1 | Vec3 _ -> 1 | Vec4 _ -> 1
    | Mat3 _ -> 3 | Mat4 _ -> 4

  let rec link : type a. a set -> (string * int) list = function
    | Nil -> []
    | More (name, ty, rest) ->
      (name, arity ty) :: link rest

  type 'a obj = { vao: vao gl; set: 'a set }

  let instance ?name set =
    let vao = Lo.Gen.vao ?name () in
    { vao; set }

  let bind_buffer { gpu; offset; skip } =
    Gpu.bind gpu;
    size_of (Gpu.kind gpu), skip, offset

  let enum_scalar (type a) : a scalar -> Gl.enum = function
    | S8  -> Gl.byte
    | U8  -> Gl.unsigned_byte
    | S16 -> Gl.short
    | U16 -> Gl.unsigned_short
    | S32 -> Gl.int
    | U32 -> Gl.unsigned_int
    | F32 -> Gl.float
    | F64 -> Gl.double

  let run_atomic loc n (x : _ buf) (s : _ scalar) =
    let gl_typ = enum_scalar s in
    let size, offset, skip = bind_buffer x in
    let stride = if skip = 0 then 0 else (skip + 1) * n in
    Gl.vertex_attrib_pointer loc n gl_typ false
      (stride * size) (`Offset (offset * size));
    Gl.enable_vertex_attrib_array loc

  let run_every step every loc n (x : _ buf) (s : _ scalar) =
    let gl_typ = enum_scalar s in
    let size, offset, skip = bind_buffer x in
    let stride = (skip + 1) * n in
    let offset = offset + step in
    let stride = stride * every in
    Gl.vertex_attrib_pointer loc n gl_typ false
      (stride * size) (`Offset (offset * size));
    Gl.enable_vertex_attrib_array loc

  let run_ty : type a. int -> a -> a ty -> int = fun loc x -> function
    | Scalar s ->
      run_atomic loc 1 x s;
      loc + 1
    | Vec2 s ->
      run_atomic loc 2 x s;
      loc + 1
    | Vec3 s ->
      run_atomic loc 3 x s;
      loc + 1
    | Vec4 s ->
      run_atomic loc 4 x s;
      loc + 1
    | Mat3 s ->
      begin match x with
        | Contiguous3 x ->
          run_every 0 3 (loc + 0) 3 x s;
          run_every 1 3 (loc + 1) 3 x s;
          run_every 2 3 (loc + 2) 3 x s;
          loc + 3
        | Separate3 (x1, x2, x3) ->
          run_atomic (loc + 0) 3 x1 s;
          run_atomic (loc + 1) 3 x2 s;
          run_atomic (loc + 2) 3 x3 s;
          loc + 3
      end
    | Mat4 s ->
      begin match x with
        | Contiguous4 x ->
          run_every 0 4 (loc + 0) 4 x s;
          run_every 1 4 (loc + 1) 4 x s;
          run_every 2 4 (loc + 2) 4 x s;
          run_every 3 4 (loc + 3) 4 x s;
          loc + 4
        | Separate4 (x1, x2, x3, x4) ->
          run_atomic (loc + 0) 4 x1 s;
          run_atomic (loc + 1) 4 x2 s;
          run_atomic (loc + 2) 4 x3 s;
          run_atomic (loc + 3) 4 x4 s;
          loc + 4
      end

  let rec run_set : type a. int -> a -> a set -> unit = fun loc xs -> function
    | Nil -> ()
    | More (_, ty, rest) ->
      let x, xs' = xs in
      let loc = run_ty loc x ty in
      run_set loc xs' rest

  let update { vao; set } a =
    Gl.bind_vertex_array !!vao;
    run_set 0 a set;
    ignore vao

  let bind obj =
    Gl.bind_vertex_array !!(obj.vao);
    ignore obj
end

module Render : sig
  type ('uniform, 'attrib) pass

  val clear : [`color | `depth | `color_to of V4.t | `depth_to of float | `stencil | `stencil_to of int ] list -> unit
  val depth : [`never|`less|`equal|`lequal|`greater|`notequal|`gequal|`always] option -> unit
  val viewport : int -> int -> int -> int -> unit

  val pass :
    ?name:name ->
    'u Uniform.set -> 'a Attrib.set ->
    [`vertex] shader gl -> [`fragment] shader gl ->
    ('u, 'a) pass
  val release_pass: _ pass -> unit

  val render :
    ('u, 'a) pass -> [`triangles] -> ?offset:int -> count:int -> 'u -> 'a Attrib.obj -> unit
end = struct
  let clear flags =
    let rec enum acc = function
      | [] -> acc
      | `color :: xs -> enum (acc lor Gl.color_buffer_bit) xs
      | `depth :: xs -> enum (acc lor Gl.depth_buffer_bit) xs
      | `stencil :: xs -> enum (acc lor Gl.stencil_buffer_bit) xs
      | `color_to {V4. x; y; z; w} :: xs ->
        Gl.clear_color x y z w;
        enum (acc lor Gl.color_buffer_bit) xs
      | `depth_to d :: xs ->
        Gl.clear_depth d;
        enum (acc lor Gl.depth_buffer_bit) xs
      | `stencil_to s :: xs ->
        Gl.clear_stencil s;
        enum (acc lor Gl.stencil_buffer_bit) xs
    in
    Gl.clear (enum 0 flags)

  let depth = function
    | None -> Gl.disable Gl.depth_test
    | Some test ->
      Gl.enable Gl.depth_test;
      Gl.depth_func
        (match test with
         | `never    -> Gl.never
         | `less     -> Gl.less
         | `equal    -> Gl.equal
         | `lequal   -> Gl.lequal
         | `greater  -> Gl.greater
         | `notequal -> Gl.notequal
         | `gequal   -> Gl.gequal
         | `always   -> Gl.always
        )

  let viewport = Gl.viewport

  type ('uniform, 'attrib) pass = {
    uniform: 'uniform Uniform.obj;
    program: program gl;
  }

  let release_pass t =
    Handle.release t.program

  let render pass `triangles ?(offset=0) ~count uniform_values vao =
    Gl.use_program !!(pass.program);
    Uniform.run pass.uniform uniform_values;
    Attrib.bind vao;
    Gl.draw_arrays Gl.triangles offset count;
    ignore pass;
    ignore vao

  let pass ?name uset aset vs fs =
    let attributes = Attrib.link aset in
    let program = Lo.Program.link ?name vs fs attributes in
    let uniform = Uniform.link program uset in
    { uniform; program }
end

module Values = struct
  type 'kind buf  = 'kind Attrib.buf
  type 'kind buf3 = 'kind Attrib.buf3
  type 'kind buf4 = 'kind Attrib.buf4

  let ( ** ) a b = (a, b)
  let nil = ()

  let buf         = Attrib.gpu_buf
  let contiguous3 = Attrib.contiguous3
  let contiguous4 = Attrib.contiguous4
  let separate3   = Attrib.separate3
  let separate4   = Attrib.separate4
end

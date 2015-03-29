open Math_funcs

(** names help debugging *)
type name = string

(** All handles to OpenGL resources are represented as integers.
    Add resource management on top, to ensure safe-use and prevent or detect
    leaking.
    Handles can be manually released, and are otherwise released by GC *)
module Handle : sig
  type desc = { kind : name; release : int -> unit; }
  val  desc : name -> (int -> unit) -> desc

  type +'a t
  val handle   : ?name:name -> desc -> int -> 'a t
  val get      : 'a t -> int
  val release  : 'a t -> unit
  val describe : 'a t -> desc * name

  exception Released of desc * name
end
type 'a gl = 'a Handle.t
val ( !! ) : 'a Handle.t -> int

(** Basic OpenGL resources *)
type tex and vbo and vao and 'a shader and program

(** Low-level management of OpenGL resources:
    allocation and release, debugging, driver info dumping ... *)
module Lo : sig
  (** Generation of fresh resources *)
  module Gen : sig
    val tex : ?name:name -> unit -> tex gl
    val vbo : ?name:name -> unit -> vbo gl
    val vao : ?name:name -> unit -> vao gl
    val shader : ?name:name -> [ `fragment | `vertex ] -> 'a shader gl
    val program : ?name:name -> unit -> program gl
  end

  (** Compilation of shaders *)
  module Shader : sig
    val vertex : ?name:name -> string -> [ `vertex ] shader gl
    val fragment : ?name:name -> string -> [ `fragment ] shader gl
    val log : 'a shader gl -> string
  end

  (** Linking of shaders *)
  module Program : sig
    val link :
      ?name:name ->
      [ `vertex ] shader gl ->
      [ `fragment ] shader gl -> (string * int) list -> program gl

    val log : program gl -> string
    val validate : program gl -> bool
    val debug_dump : ?out:out_channel -> program gl -> unit
  end
end

(** OpenGL evaluation model is roughly as follow:
    - shaders are pure functions run in a massively parallel fashion on the GPU
    - those functions are parameterized by two kind of variables:
      - uniforms, which are bound to a unique value in a given pass
      - attributes, which are bound to a stream of values

    Uniforms alone allow to paramaterize the functions, but attributes enable
    the parallelism: an independant thread is run for each value of the stream.

    A program consists of multiple stage of shaders, each stage acting as a
    "map-filter-unfold" transformer for the next stage:
    - transform input value from the stream
    - various rules, depending on the position of the stage in the pipeline,
      determine rejection of current thread or creation of multiple
      sub-threads.

    Only vertex shader (moving points) and fragment shaders (producing "color"
    information by being executed on each point of the output surface covered
    by the surface described by vertexes).
*)

(** A strongly-typed interface to uniforms management.
    Uniforms are values passed as constants to shader programs. *)
module Uniform : sig
  type 'a ty

  (** Primitive types *)
  val int    : string -> int ty * string
  val uint   : string -> int ty * string
  val bool   : string -> bool ty * string
  val float  : string -> float ty * string
  val double : string -> float ty * string

  (** Fixed-size vectors and matrices of floats *)
  val vec2 : string -> vec2 ty * string
  val vec3 : string -> vec3 ty * string
  val vec4 : string -> vec4 ty * string
  val mat3 : string -> mat3 ty * string
  val mat4 : string -> mat4 ty * string

  (** Sampler types, for binding textures.
      FIXME: textures should be strongly typed too. *)
  val sampler1d       : string -> tex gl ty * string
  val sampler2d       : string -> tex gl ty * string
  val sampler3d       : string -> tex gl ty * string
  val sampler_cube    : string -> tex gl ty * string
  val sampler2d_rect  : string -> tex gl ty * string
  val sampler1d_array : string -> tex gl ty * string
  val sampler2d_array : string -> tex gl ty * string

  (** The interface of program is represented as a set of primitive uniform
      parameters.
      E.g:
      Uniform.(float "size" ** mat4 "model_matrix"
            ** sampler2d "main_texture" ** nil)
  *)
  type 'a set
  val ( ** ) : 'a ty * string -> 'b set -> ('a * 'b) set
  val nil : unit set
end

(** Typed buffers, describing a stream of value sent to the GPU.  As a
    simplification, we assume that buffers are only composed of primitive
    values, whereas OpenGL allow buffer of structured values (C-like struct
    type).  *)
module Buffer : sig
  type ('a, 'b) kind = ('a, 'b) Bigarray.kind

  type s8  = (int, Bigarray.int8_signed_elt) kind
  type u8  = (int, Bigarray.int8_unsigned_elt) kind
  type s16 = (int, Bigarray.int16_signed_elt) kind
  type u16 = (int, Bigarray.int16_unsigned_elt) kind
  type s32 = (int32, Bigarray.int32_elt) kind
  type f32 = (float, Bigarray.float32_elt) kind
  type f64 = (float, Bigarray.float64_elt) kind

  val s8 : s8
  val u8 : u8
  val s16 : s16
  val u16 : u16
  val s32 : s32
  val f32 : f32
  val f64 : f64

  (** Size in bytes of one value of the given kind *)
  val size_of : ('a, 'b) kind -> int

  (** We distinguish between buffers on the cpu and buffers on the gpu.
      Data have to be transferred explicitly.
      A cpu buffer is just an OCaml bigarray. *)
  module Cpu = Bigarray.Array1
  type 'c cpu = ('a, 'b, Bigarray.c_layout) Cpu.t
    constraint 'c = ('a, 'b) kind

  (** Gpu buffers are abstract, but can be filled by uploading or blitting a
      cpu buffer to gpu memory. *)
  module Gpu :
  sig
    type 'c t constraint 'c = ('a, 'b) kind

    (** Hint for the driver:
        - a `static buffer is filled rarely (or even once) and used often
        - a `dynamic buffer is modified often (say, for each frame)
    *)
    type policy = [ `dynamic | `static ]

    val create : ?name:name -> ?policy:policy -> ('a, 'b) kind -> ('a, 'b) kind t
    val dim : ('a, 'b) kind t -> int
    val kind : ('a, 'b) kind t -> ('a, 'b) kind
  end
  type 'a gpu = 'a Gpu.t constraint 'a = ('b, 'c) kind

  (** Shortcut for buffer creation *)
  val gpu : ?name:name -> ?policy:Gpu.policy -> ('a, 'b) kind -> ('a, 'b) kind gpu

  (** Upload new data to gpu. Buffer is reallocated to fit the appropriate size. *)
  val upload : ('a, 'b) kind cpu -> ('a, 'b) kind gpu -> unit

  (** Change size of the buffer. New content is undefined. *)
  val resize : ('a, 'b) kind gpu -> int -> unit

  (** Turn an OCaml array to a cpu buffer. *)
  val of_array : ('a, 'b) kind -> 'a array -> ('a, 'b) kind cpu

  (** Update (part of) a gpu buffer from a cpu buffer.
      No reallocation is done. *)
  val blit : ('a, 'b) kind cpu -> ?offset:int -> ('a, 'b) kind gpu -> unit
end

(** Attributes
*)
module Attrib :
sig
  (** A type for scalar values backed by a specific representation *)
  type 'a scalar

  (** A gpu buffer for a stream of values *)
  type 'a buf  constraint 'a = ('b, 'c) Bigarray.kind

  (** buf3 brings structure over buffers to interpret them as a stream 3x3 matrices *)
  type 'a buf3 constraint 'a = ('b, 'c) Bigarray.kind
  (** buf4 brings structure over buffers to interpret them as a stream 4x4 matrices *)
  type 'a buf4 constraint 'a = ('b, 'c) Bigarray.kind

  (** The final type of attributes *)
  type 'a ty

  (** Construct types *)
  val scalar : 'kind scalar -> string -> 'kind buf ty * string
  val vec2   : 'kind scalar -> string -> 'kind buf ty * string
  val vec3   : 'kind scalar -> string -> 'kind buf ty * string
  val vec4   : 'kind scalar -> string -> 'kind buf ty * string
  val mat3   : 'kind scalar -> string -> 'kind buf3 ty * string
  val mat4   : 'kind scalar -> string -> 'kind buf4 ty * string

  (** Construct set of types *)
  type 'a set
  val nil : unit set
  val ( ** ) : 'a ty * string -> 'b set -> ('a * 'b) set

  (** Construct values of a given set *)
  type 'a obj
  val instance : ?name:name -> 'a set -> 'a obj
  val update : 'a obj -> 'a -> unit

  (** Shortcuts to buffer kinds from [Buffer] module *)
  val s8  : Buffer.s8 scalar
  val u8  : Buffer.u8 scalar
  val s16 : Buffer.s16 scalar
  val u16 : Buffer.u16 scalar
  val s32 : Buffer.s32 scalar
  val u32 : Buffer.s32 scalar
  val f32 : Buffer.f32 scalar
  val f64 : Buffer.f64 scalar
end

module Render :
sig
  (** A render pass is a program with a specific api *)
  type ('uniform, 'attrib) pass

  (** Misc functions preparing a render *)
  val clear :
    [ `color | `depth | `stencil
    | `color_to of Math_funcs.V4.t
    | `depth_to of float
    | `stencil_to of int ] list
    -> unit

  val depth :
    [ `always | `equal | `gequal | `greater | `lequal | `less |
      `never | `notequal ] option -> unit

  val viewport : int -> int -> int -> int -> unit

  (** Construct a new pass from:
      - description of uniforms and attributes interface,
      - shaders for vertex and fragments *)
  val pass :
    ?name:name ->
    'u Uniform.set ->
    'a Attrib.set ->
    [ `vertex ] shader gl -> [ `fragment ] shader gl -> ('u, 'a) pass

  (** Release all memory associated to a pass.
      Reusing pass after will result in a [Handle.Released] exception. *)
  val release_pass : ('a, 'b) pass -> unit

  (** Given values for uniforms and streams for attributes, render some
      geometry. *)
  val render :
    (* Program to use *)
    ('u, 'a) pass
    (* Geometry description *)
    -> [ `triangles ] -> ?offset:int -> count:int
    (* Values for uniforms and attributes *)
    -> 'u -> 'a Attrib.obj
    -> unit
end

(** Shorthands for constructing values *)
module Values :
sig
  type 'kind buf  = 'kind Attrib.buf
  type 'kind buf3 = 'kind Attrib.buf3
  type 'kind buf4 = 'kind Attrib.buf4

  val ( ** ) : 'a -> 'b -> 'a * 'b
  val nil : unit
  val buf : ?offset:int -> ?skip:int -> 'kind Buffer.gpu -> 'kind buf
  val contiguous3 : 'kind buf -> 'kind buf3
  val contiguous4 : 'kind buf -> 'kind buf4
  val separate3 : 'kind buf -> 'kind buf -> 'kind buf -> 'kind buf3
  val separate4 : 'kind buf -> 'kind buf -> 'kind buf -> 'kind buf -> 'kind buf4
end

(** Misc functions *)

val gl_type_to_string : Tgl4.Gl.enum -> string
val file_contents : string -> string
val fps_counter : unit -> unit -> float
val frame_timer : int -> unit -> int
val log_gl_params : unit -> unit

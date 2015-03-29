val pi : float
val one_deg_in_rad : float
val one_rad_in_deg : float

module V2 : sig
  type t = { x : float; y : float; }
  val zero : t
  val ( +% ) : t -> t -> t
  val ( -% ) : t -> t -> t
  val ( *% ) : t -> float -> t
  val ( /% ) : t -> float -> t
  val norm : t -> float
  val norm2 : t -> float
  val normalize : t -> t
  val to_string : t -> string
end

module V3 : sig
  type t = { x : float; y : float; z : float; }
  val zero : t
  val v2 : V2.t -> float -> t
  val ( +% ) : t -> t -> t
  val ( -% ) : t -> t -> t
  val ( *% ) : t -> float -> t
  val ( /% ) : t -> float -> t
  val norm : t -> float
  val norm2 : t -> float
  val normalize : t -> t
  val neg : t -> t
  val dot : t -> t -> float
  val cross : t -> t -> t
  val to_string : t -> string
  val dist2 : t -> t -> float
  val direction_to_heading : t -> float
  val heading_to_direction : float -> t
end

module V4 : sig
  type t = { x : float; y : float; z : float; w : float; }
  val zero : t
  val v2 : V2.t -> float -> float -> t
  val v3 : V3.t -> float -> t
  val ( +% ) : t -> t -> t
  val ( -% ) : t -> t -> t
  val ( *% ) : t -> float -> t
  val ( /% ) : t -> float -> t
  val to_string : t -> string
end

val v3_of_v4 : V4.t -> V3.t

module Q : sig
  type t = { r : float; i : float; j : float; k : float; }
  val ( /% ) : t -> float -> t
  val ( *% ) : t -> float -> t
  val normalize : t -> t
  val ( +% ) : t -> t -> t
  val ( %*% ) : t -> t -> t
  val dot : t -> t -> float
  val from_axis_rad : float -> V3.t -> t
  val from_axis_deg : float -> V3.t -> t
  val slerp : t -> t -> float -> t
  val to_string : t -> string
end


module M3 : sig
  type t = {
    a : float; b : float; c : float;
    d : float; e : float; f : float;
    g : float; h : float; i : float;
  }
  val zero : t
  val identity : t
  val to_string : t -> string
  val buf :
    (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
  val m :
    t -> (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
end

module M4 : sig
  type t = {
    a : float; b : float; c : float; d : float;
    e : float; f : float; g : float; h : float;
    i : float; j : float; k : float; l : float;
    m : float; n : float; o : float; p : float;
  }
  val zero : t
  val identity : t
  val to_string : t -> string
  val buf :
    (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
  val make :
    float -> float -> float -> float ->
    float -> float -> float -> float ->
    float -> float -> float -> float ->
    float -> float -> float -> float -> t
  val m :
    t -> (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
  val act : t -> V4.t -> V4.t
  val ( *% ) : t -> t -> t
  val determinant : t -> float
  val inverse : t -> t
  val transpose : t -> t
  val translate : V3.t -> t
  val rotate_x_deg : float -> t
  val rotate_y_deg : float -> t
  val rotate_z_deg : float -> t
  val rotate : ?x:float -> ?y:float -> ?z:float -> unit -> t
  val scale : V3.t -> t
  val look_at : V3.t -> V3.t -> V3.t -> t
  val perspective : float -> float -> float -> float -> t
  val of_quat : Q.t -> t
end

type vec2 = V2.t
type vec3 = V3.t
type vec4 = V4.t
type mat3 = M3.t
type mat4 = M4.t
type quat = Q.t

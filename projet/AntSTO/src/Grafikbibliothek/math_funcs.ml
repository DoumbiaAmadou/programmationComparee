let pi = 4.0 *. atan 1.0
let one_deg_in_rad = (2.0 *. pi) /. 360.0
let one_rad_in_deg = 360.0 /. (2.0 *. pi)

module V2 = struct
  type t = {x: float; y: float}
  let zero = {x = 0.; y = 0.}

  let ( +% ) {x = x1; y = y1} {x = x2; y = y2} =
    {x = x1 +. x2; y = y1 +. y2}

  let ( -% ) {x = x1; y = y1} {x = x2; y = y2} =
    {x = x1 -. x2; y = y1 -. y2}

  let ( *% ) {x; y} s =
    {x = x *. s; y = y *. s}

  let ( /% ) {x; y} s =
    {x = x /. s; y = y /. s}

  let norm {x; y} = sqrt (x *. x +. y *. y)
  let norm2 {x; y} = (x *. x +. y *. y)
  let normalize v = v /% norm v

  let to_string {x; y} =
    Printf.sprintf "{x = %f; y = %f}" x y
end

module V3 = struct
  type t = {x: float; y: float; z: float}
  let zero = {x = 0.; y = 0.; z = 0.}

  let v2 {V2. x; y} z = {x; y; z}

  let ( +% ) {x = x1; y = y1; z = z1} {x = x2; y = y2; z = z2} =
    {x = x1 +. x2; y = y1 +. y2; z = z1 +. z2}

  let ( -% ) {x = x1; y = y1; z = z1} {x = x2; y = y2; z = z2} =
    {x = x1 -. x2; y = y1 -. y2; z = z1 -. z2}

  let ( *% ) {x; y; z} s =
    {x = x *. s; y = y *. s; z = z *. s}

  let ( /% ) {x; y; z} s =
    {x = x /. s; y = y /. s; z = z /. s}

  let norm {x; y; z} = sqrt (x *. x +. y *. y +. z *. z)
  let norm2 {x; y; z} = (x *. x +. y *. y +. z *. z)
  let normalize v = v /% norm v

  let neg {x; y; z} = {x = -.x; y = -.y; z = -.z}

  let dot {x = x1; y = y1; z = z1} {x = x2; y = y2; z = z2} =
    x1 *. x2 +. y1 *. y2 +. z1 *. z2

  let cross {x = x1; y = y1; z = z1} {x = x2; y = y2; z = z2} =
    { x = y1 *. z2 -. z1 *. y2;
      y = z1 *. x2 -. x1 *. z2;
      z = x1 *. y2 -. y1 *. x2 }

  let to_string {x; y; z} =
    Printf.sprintf "{x = %f; y = %f; z = %f}" x y z

  let dist2 v1 v2 = norm2 (v1 -% v2)

  let direction_to_heading d =
    atan2 (-. d.x) (-. d.z) *. one_rad_in_deg

  let heading_to_direction d =
    let rad = d *. one_deg_in_rad in
    { x = -. sin rad; y = 0.0; z = -. cos rad }
end

module V4 = struct
  type t = {x: float; y: float; z: float; w: float}
  let zero = {x = 0.; y = 0.; z = 0.; w = 0.}

  let v2 {V2. x; y} z w = {x; y; z; w}
  let v3 {V3. x; y; z} w = {x; y; z; w}

  let ( +% ) {x = x1; y = y1; z = z1; w = w1} {x = x2; y = y2; z = z2; w = w2} =
    {x = x1 +. x2; y = y1 +. y2; z = z1 +. z2; w = w1 +. w2}

  let ( -% ) {x = x1; y = y1; z = z1; w = w1} {x = x2; y = y2; z = z2; w = w2} =
    {x = x1 -. x2; y = y1 -. y2; z = z1 -. z2; w = w1 -. w2}

  let ( *% ) {x; y; z; w} s =
    {x = x *. s; y = y *. s; z = z *. s; w = w *. s}

  let ( /% ) {x; y; z; w} s =
    {x = x /. s; y = y /. s; z = z /. s; w = w /. s}

  let to_string {x; y; z; w} =
    Printf.sprintf "{x = %f; y = %f; z = %f; w = %f}" x y z w
end

let v3_of_v4 {V4. x; y; z; w = _} = {V3. x; y; z}

module Q = struct
  type t = {r: float; i: float; j: float; k: float}

  let ( /% ) {r; i; j; k} s =
    {r = r /. s; i = i /. s; j = j /. s; k = k /. s}

  let ( *% ) {r; i; j; k} s =
    {r = r *. s; i = i *. s; j = j *. s; k = k *. s}

  let normalize q =
    let sum = q.r *. q.r +. q.i *. q.i +. q.j *. q.j +. q.k *. q.k in
    if abs_float (1.0 -. sum) < 0.0001 then
      q
    else
      q /% sqrt sum

  let ( +% )
      {r = r1; i = i1; j = j1; k = k1}
      {r = r2; i = i2; j = j2; k = k2} =
    normalize {
      r = r1 +. r2;
      i = i1 +. i2;
      j = j1 +. j2;
      k = k1 +. k2;
    }
  let ( %*% ) r s =
    normalize {
      r = s.r *. r.r -. s.i *. r.i -. s.j *. r.j -. s.k *. r.k;
      i = s.r *. r.i +. s.i *. r.r -. s.j *. r.k +. s.k *. r.j;
      j = s.r *. r.j +. s.i *. r.k +. s.j *. r.r -. s.k *. r.i;
      k = s.r *. r.k -. s.i *. r.j +. s.j *. r.i +. s.k *. r.r;
    }

  let dot
      {r = r1; i = i1; j = j1; k = k1}
      {r = r2; i = i2; j = j2; k = k2} =
    r1 *. r2 +. i1 *. i2 +. j1 *. j2 +. k1 *. k2

  let from_axis_rad rad v =
    let open V3 in
    let rad = rad /. 2. in
    let c = cos rad and s = sin rad in
    {r = c; i = s *. v.x; j = s *. v.y; k = s *. v.z}

  let from_axis_deg deg v =
    from_axis_rad (one_deg_in_rad *. deg) v

  let slerp q r t =
    (* angle between q0-q1 *)
    let cos_half_theta = dot q r in
    (* as found here http://stackoverflow.com/questions/2886606/flipping-issue-when-interpolating-rotations-using-quaternions
       if dot product is negative then one quaternion should be negated, to make
       it take the short way around, rather than the long way
       yeah! and furthermore Susan, I had to recalculate the d.p. after this *)
    let q, cos_half_theta =
      if cos_half_theta < 0.0 then
        let q = q *% (-1.) in
        q, dot q r
      else
        q, cos_half_theta
    in
    (* if qa=qb or qa=-qb then theta = 0 and we can return qa *)
    if abs_float cos_half_theta >= 1.0 then
      q
    else
      (* Calculate temporary values *)
      let sin_half_theta = sqrt (1.0 -. cos_half_theta *. cos_half_theta) in
      (* if theta = 180 degrees then result is not fully defined
         we could rotate around any axis normal to qa or qb *)
      if abs_float sin_half_theta < 0.001 then
        q *% (1.0 -. t) +% r *% t
      else
        let half_theta = acos cos_half_theta in
        let a = sin ((1.0 -. t) *. half_theta) /. sin_half_theta in
        let b = sin (t *. half_theta) /. sin_half_theta in
        q *% a +% r *% b

  let to_string {r; i; j; k} =
    Printf.sprintf "{r = %f; i = %f; j = %f; k = %f}" r i j k

end

module M3 = struct
  type t = {
    a: float; b: float; c: float;
    d: float; e: float; f: float;
    g: float; h: float; i: float;
  }
  let zero = {a = 0.; b = 0.; c = 0.;
              d = 0.; e = 0.; f = 0.;
              g = 0.; h = 0.; i = 0.}
  let identity = {a = 1.; b = 0.; c = 0.;
                  d = 0.; e = 1.; f = 0.;
                  g = 0.; h = 0.; i = 1.}

  let to_string {a; b; c;
                 d; e; f;
                 g; h; i} =
    Printf.sprintf "[[%f; %f; %f]; [%f; %f; %f]; [%f; %f; %f]]"
      a b c d e f g h i

  let buf = Bigarray.(Array1.create float32 c_layout 9)

  let m {a; b; c; d; e; f; g; h; i} =
    let open Bigarray.Array1 in
    unsafe_set buf 0 a;
    unsafe_set buf 1 b;
    unsafe_set buf 2 c;
    unsafe_set buf 3 d;
    unsafe_set buf 4 e;
    unsafe_set buf 5 f;
    unsafe_set buf 6 g;
    unsafe_set buf 7 h;
    unsafe_set buf 8 i;
    buf
end

module M4 = struct
  type t = {
    a: float; b: float; c: float; d: float;
    e: float; f: float; g: float; h: float;
    i: float; j: float; k: float; l: float;
    m: float; n: float; o: float; p: float;
  }
  let zero = {a = 0.; b = 0.; c = 0.; d = 0.;
              e = 0.; f = 0.; g = 0.; h = 0.;
              i = 0.; j = 0.; k = 0.; l = 0.;
              m = 0.; n = 0.; o = 0.; p = 0.}
  let identity = {a = 1.; b = 0.; c = 0.; d = 0.;
                  e = 0.; f = 1.; g = 0.; h = 0.;
                  i = 0.; j = 0.; k = 1.; l = 0.;
                  m = 0.; n = 0.; o = 0.; p = 1.}

  let to_string {a; b; c; d;
                 e; f; g; h;
                 i; j; k; l;
                 m; n; o; p} =
    Printf.sprintf "[[%f; %f; %f; %f];\n [%f; %f; %f; %f];\n [%f; %f; %f; %f];\n [%f; %f; %f; %f]]"
      a b c d e f g h i j k l m n o p

  let buf = Bigarray.(Array1.create float32 c_layout 16)

  let make a b c d e f g h i j k l m n o p =
    {a; b; c; d; e; f; g; h; i; j; k; l; m; n; o; p}

  let m {a; b; c; d; e; f; g; h; i; j; k; l; m; n; o; p} =
    let open Bigarray.Array1 in
    unsafe_set buf 0 a;
    unsafe_set buf 1 b;
    unsafe_set buf 2 c;
    unsafe_set buf 3 d;
    unsafe_set buf 4 e;
    unsafe_set buf 5 f;
    unsafe_set buf 6 g;
    unsafe_set buf 7 h;
    unsafe_set buf 8 i;
    unsafe_set buf 9 j;
    unsafe_set buf 10 k;
    unsafe_set buf 11 l;
    unsafe_set buf 12 m;
    unsafe_set buf 13 n;
    unsafe_set buf 14 o;
    unsafe_set buf 15 p;
    buf

  let act m {V4. x; y; z; w} =
    {V4.
      x = m.a *. x +. m.e *. y +. m.i *. z +. m.m *. w;
      y = m.b *. x +. m.f *. y +. m.j *. z +. m.n *. w;
      z = m.c *. x +. m.g *. y +. m.k *. z +. m.o *. w;
      w = m.d *. x +. m.h *. y +. m.l *. z +. m.p *. w;
    }

  let ( *% ) (m1 : t) (m2 : t) =
    let result : float array = Array.make 16 0. in
    let m1 : float array = Obj.magic m1 in
    let m2 : float array = Obj.magic m2 in
    let open Array in
    for col = 0 to 3 do
      let col = col * 4 in
      for row = 0 to 3 do
        unsafe_set result (col + row)
          ((unsafe_get m1 (row +  0) *. unsafe_get m2 (col + 0)) +.
           (unsafe_get m1 (row +  4) *. unsafe_get m2 (col + 1)) +.
           (unsafe_get m1 (row +  8) *. unsafe_get m2 (col + 2)) +.
           (unsafe_get m1 (row + 12) *. unsafe_get m2 (col + 3)))
      done
    done;
    (Obj.magic result : t)

  (* returns a scalar value with the determinant for a 4x4 matrix
     see http://www.euclideanspace.com/maths/algebra/matrix/functions/determinant/fourD/index.htm *)
  let determinant m =
		m.m *. m.j *. m.g *. m.d -.
		m.i *. m.n *. m.g *. m.d -.
		m.m *. m.f *. m.k *. m.d +.
		m.e *. m.n *. m.k *. m.d +.
		m.i *. m.f *. m.o *. m.d -.
		m.e *. m.j *. m.o *. m.d -.
		m.m *. m.j *. m.c *. m.h +.
		m.i *. m.n *. m.c *. m.h +.
		m.m *. m.b *. m.k *. m.h -.
		m.a *. m.n *. m.k *. m.h -.
		m.i *. m.b *. m.o *. m.h +.
		m.a *. m.j *. m.o *. m.h +.
		m.m *. m.f *. m.c *. m.l -.
		m.e *. m.n *. m.c *. m.l -.
		m.m *. m.b *. m.g *. m.l +.
		m.a *. m.n *. m.g *. m.l +.
		m.e *. m.b *. m.o *. m.l -.
		m.a *. m.f *. m.o *. m.l -.
		m.i *. m.f *. m.c *. m.p +.
		m.e *. m.j *. m.c *. m.p +.
		m.i *. m.b *. m.g *. m.p -.
		m.a *. m.j *. m.g *. m.p -.
		m.e *. m.b *. m.k *. m.p +.
		m.a *. m.f *. m.k *. m.p

  (* returns a 16-element array that is the inverse of a 16-element array (4x4
     matrix). see http://www.euclideanspace.com/maths/algebra/matrix/functions/inverse/fourD/index.htm *)
  let inverse m =
    let det = determinant m in
    (* there is no inverse if determinant is zero (not likely unless scale is broken) *)
    if det = 0.0 then
      (prerr_endline "WARNING. matrix has no determinant. can not invert"; m)
    else
      let inv_det = 1.0 /. det in
      {
        a = inv_det *. (
            m.j *. m.o *. m.h -. m.n *. m.k *. m.h +.
            m.n *. m.g *. m.l -. m.f *. m.o *. m.l -.
            m.j *. m.g *. m.p +. m.f *. m.k *. m.p);
        b = inv_det *. (
            m.n *. m.k *. m.d -. m.j *. m.o *. m.d -.
            m.n *. m.c *. m.l +. m.b *. m.o *. m.l +.
            m.j *. m.c *. m.p -. m.b *. m.k *. m.p);
        c = inv_det *. (
            m.f *. m.o *. m.d -. m.n *. m.g *. m.d +.
            m.n *. m.c *. m.h -. m.b *. m.o *. m.h -.
            m.f *. m.c *. m.p +. m.b *. m.g *. m.p);
        d = inv_det *. (
            m.j *. m.g *. m.d -. m.f *. m.k *. m.d -.
            m.j *. m.c *. m.h +. m.b *. m.k *. m.h +.
            m.f *. m.c *. m.l -. m.b *. m.g *. m.l);
        e = inv_det *. (
            m.m *. m.k *. m.h -. m.i *. m.o *. m.h -.
            m.m *. m.g *. m.l +. m.e *. m.o *. m.l +.
            m.i *. m.g *. m.p -. m.e *. m.k *. m.p);
        f = inv_det *. (
            m.i *. m.o *. m.d -. m.m *. m.k *. m.d +.
            m.m *. m.c *. m.l -. m.a *. m.o *. m.l -.
            m.i *. m.c *. m.p +. m.a *. m.k *. m.p);
        g = inv_det *. (
            m.m *. m.g *. m.d -. m.e *. m.o *. m.d -.
            m.m *. m.c *. m.h +. m.a *. m.o *. m.h +.
            m.e *. m.c *. m.p -. m.a *. m.g *. m.p);
        h = inv_det *. (
            m.e *. m.k *. m.d -. m.i *. m.g *. m.d +.
            m.i *. m.c *. m.h -. m.a *. m.k *. m.h -.
            m.e *. m.c *. m.l +. m.a *. m.g *. m.l);
        i = inv_det *. (
            m.i *. m.n *. m.h -. m.m *. m.j *. m.h +.
            m.m *. m.f *. m.l -. m.e *. m.n *. m.l -.
            m.i *. m.f *. m.p +. m.e *. m.j *. m.p);
        j = inv_det *. (
            m.m *. m.j *. m.d -. m.i *. m.n *. m.d -.
            m.m *. m.b *. m.l +. m.a *. m.n *. m.l +.
            m.i *. m.b *. m.p -. m.a *. m.j *. m.p);
        k = inv_det *. (
            m.e *. m.n *. m.d -. m.m *. m.f *. m.d +.
            m.m *. m.b *. m.h -. m.a *. m.n *. m.h -.
            m.e *. m.b *. m.p +. m.a *. m.f *. m.p);
        l = inv_det *. (
            m.i *. m.f *. m.d -. m.e *. m.j *. m.d -.
            m.i *. m.b *. m.h +. m.a *. m.j *. m.h +.
            m.e *. m.b *. m.l -. m.a *. m.f *. m.l);
        m = inv_det *. (
            m.m *. m.j *. m.g -. m.i *. m.n *. m.g -.
            m.m *. m.f *. m.k +. m.e *. m.n *. m.k +.
            m.i *. m.f *. m.o -. m.e *. m.j *. m.o);
        n = inv_det *. (
            m.i *. m.n *. m.c -. m.m *. m.j *. m.c +.
            m.m *. m.b *. m.k -. m.a *. m.n *. m.k -.
            m.i *. m.b *. m.o +. m.a *. m.j *. m.o);
        o = inv_det *. (
            m.m *. m.f *. m.c -. m.e *. m.n *. m.c -.
            m.m *. m.b *. m.g +. m.a *. m.n *. m.g +.
            m.e *. m.b *. m.o -. m.a *. m.f *. m.o);
        p = inv_det *. (
            m.e *. m.j *. m.c -. m.i *. m.f *. m.c +.
            m.i *. m.b *. m.g -. m.a *. m.j *. m.g -.
            m.e *. m.b *. m.k +. m.a *. m.f *. m.k);
      }

  let transpose m =
    { a = m.a; b = m.e; c = m.i; d = m.m;
      e = m.b; f = m.f; g = m.j; h = m.n;
      i = m.c; j = m.g; k = m.k; l = m.o;
      m = m.d; n = m.h; o = m.l; p = m.p }

  let translate {V3. x; y; z} =
    {identity with m = x; n = y; o = z}

  let rotate_x_deg deg =
    let rad = deg *. one_deg_in_rad in
    let c = cos rad and s = sin rad in
    {identity with f = c; j = -.s; g = s; k = c}

  let rotate_y_deg deg =
    let rad = deg *. one_deg_in_rad in
    let c = cos rad and s = sin rad in
    {identity with a = c; i = s; c = -.s; k = c}

  let rotate_z_deg deg =
    let rad = deg *. one_deg_in_rad in
    let c = cos rad and s = sin rad in
    {identity with a = c; e = -.s; b = s; f = c}

  let rotate ?(x=0.) ?(y=0.) ?(z=0.) () =
    let radx = x *. one_deg_in_rad in
    let rady = y *. one_deg_in_rad in
    let radz = z *. one_deg_in_rad in
    let cy = cos rady and sy = sin rady in
    let cx = cos radx and sx = sin radx in
    let cz = cos radz and sz = sin radz in
    {identity with f = cx *. cz; j = -.sx; g = sx; k = cx *. cy;
                   a = cy *. cz; i = sy; c = -.sy; e = -.sz; b = sz}

  let scale {V3. x; y; z} =
    {identity with a = x; f = y; k = z}

    (*-----------------------VIRTUAL CAMERA MATRIX FUNCTIONS----------------------*)

  (* returns a view matrix using the opengl lookAt style. COLUMN ORDER. *)
  let look_at cam_pos targ_pos up =
    let mul  = ( *% ) in
    let open V3 in
    (* inverse translation *)
    let p = translate (neg cam_pos) in
    (* distance vector *)
    let d = targ_pos -% cam_pos in
    (* forward vector *)
    let f = normalize d in
    (* right vector *)
    let r = normalize (cross f up) in
    (* real up vector *)
    let u = normalize (cross r f) in
    let ori = {identity with
               a =    r.x; e =    r.y; i =    r.z;
               b =    u.x; f =    u.y; j =    u.z;
               c = -. f.x; g = -. f.y; k = -. f.z} in
    mul ori p

  (* returns a perspective function mimicking the opengl projection style. *)
  let perspective fovy aspect near far =
    let fov_rad = fovy *. one_deg_in_rad in
    let range = tan (fov_rad /. 2.0) *. near in
    let sx = (2.0 *. near) /. (range *. aspect +. range *. aspect) in
    let sy = near /. range in
    let sz = -. (far +. near) /. (far -. near) in
    let pz = -. (2.0 *. far *. near) /. (far -. near) in
    {zero with
     a = sx; f = sy; k = sz;
     o = pz; l = -1.0}

  let of_quat {Q. r = w; i = x; j = y; k = z} =
    {
      a = 1.0 -. 2.0 *. y *. y -. 2.0 *. z *. z;
      b = 2.0 *.   x *. y      +. 2.0 *. w *. z;
      c = 2.0 *.   x *. z      -. 2.0 *. w *. y;
      d = 0.0;

      e = 2.0 *.   x *. y      -. 2.0 *. w *. z;
      f = 1.0 -. 2.0 *. x *. x -. 2.0 *. z *. z;
      g = 2.0 *.   y *. z      +. 2.0 *. w *. x;
      h = 0.0;

      i = 2.0 *.   x *. z      +. 2.0 *. w *. y;
      j = 2.0 *.   y *. z      -. 2.0 *. w *. x;
      k = 1.0 -. 2.0 *. x *. x -. 2.0 *. y *. y;
      l = 0.0;

      m = 0.0; n = 0.0; o = 0.0; p = 1.0;
    }
end

type vec2 = V2.t
type vec3 = V3.t
type vec4 = V4.t
type mat3 = M3.t
type mat4 = M4.t
type quat = Q.t

type t = { x: float; y: float; z: float }

(* Compute multiplication of a vector by another vector *)
val (@*): t -> t -> t

(* Compute multiplication of a vector by by a float *)
val (@*.): t -> float -> t

(* Compute addition of a vector and another vector *)
val (@+): t -> t -> t

(*  Compute addition of a vector and a float *)
val (@+.): t -> float -> t         

(* Compute subtraction of a vector from another vector *)
val (@-): t -> t -> t

(* Compute subtraction of a float from a vector *)
val (@-.): t -> float -> t

(* Compute division of a vector by another vector *)
val (@/): t -> t -> t

(* Compute division of a vector by a float *)
val (@/.): t -> float -> t

(* Invert a vector *)
val negate: t -> t

(* Computes dot product (or inner product) of two vectors.
   Produces a scalar which tells you how close to being orthoganal the vecs are,
   where 0 is orthoganal, positive indicates closer than orthoganal (i.e. that they project on one another) and negative the opposite *)
val dot: t -> t -> float

(* Compute cross product of two vectors.
   Produces a vector perpendicular to the plane defined by v1 and v2.
   Used in lighting and finding angles between two vectors.
   Assuming that v1 and v2 are in the XZ plane, then the direction of the cross product vector will
   point upwards if the rotation from v1 to v2 is counter clockwise, and downwards if the rotation is clockwise. *)
val cross: t -> t -> t

(* Compute the squared length of a vector *)
val squared_length: t -> float

(* Compute the length of a vector *)
val length: t -> float

(* Normalize a vector *)
val normalize: t -> t

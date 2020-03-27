type t = { x: float; y: float; z: float }

val (@*): t -> t -> t

val (@*.): t -> float -> t
         
val (@+): t -> t -> t

val (@+.): t -> float -> t         
      
val (@-): t -> t -> t

val (@-.): t -> float -> t
         
val (@/): t -> t -> t

val (@/.): t -> float -> t

val negate: t -> t

val dot: t -> t -> float

val cross: t -> t -> t

val squared_length: t -> float

val length: t -> float

val normalize: t -> t

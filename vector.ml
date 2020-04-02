type t = { x: float; y: float; z: float }

let (@*) v1 v2 =
  { x = v1.x *. v2.x;
    y = v1.y *. v2.y;
    z = v1.z *. v2.z }
         
let (@*.) { x; y; z } f =
  { x = x *. f;
    y = y *. f;
    z = z *. f }
         
let (@+) v1 v2 =
  { x = v1.x +. v2.x;
    y = v1.y +. v2.y;
    z = v1.z +. v2.z }

let (@+.) { x; y; z } f =
  { x = x +. f;
    y = y +. f;
    z = z +. f }
         
let (@-) v1 v2 =
  { x = v1.x -. v2.x;
    y = v1.y -. v2.y;
    z = v1.z -. v2.z }

let (@-.) { x; y; z } f =
  { x = x -. f;
    y = y -. f;
    z = z -. f }
         
let (@/) v1 v2 =
  { x = v1.x /. v2.x;
    y = v1.y /. v2.y;
    z = v1.z /. v2.z }

let (@/.) { x; y; z } f =
  { x = x /. f;
    y = y /. f;
    z = z /. f }

let negate { x; y; z } =
  { x = (-. x);
    y = (-. y);
    z = (-. z) }

let dot v1 v2 =
  let xs = v1.x *. v2.x in 
  let ys = v1.y *. v2.y in 
  let zs = v1.z *. v2.z in
  xs +. ys +. zs

let cross v1 v2 =
  { x = (v1.y *. v2.z) -. (v1.z *. v2.y);
    y = (v1.z *. v2.x) -. (v1.x *. v2.z);
    z = (v1.x *. v2.y) -. (v1.y *. v2.x) }

let squared_length { x; y; z} =
  (x *. x) +. (y *. y) +. (z *. z)

let length v = squared_length v |> sqrt

let normalize v =
  let l = length v in
  if l = 0. then v else v @/. l

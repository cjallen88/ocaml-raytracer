open Vector

type fb = { data: Vector.t array; width: int; height: int }

type shape = Sphere of Vector.t * float
type hittable = { shape: shape; colour: Vector.t; specular_component: float }
type light =
  | Point_light of Vector.t * float
  | Ambient_light of float
type scene = { objects : hittable list; lights: light list }
type ray = { origin: Vector.t; direction: Vector.t } 

(* reduces `list` with `fn`, accessing each element with key_fn *)
let find_by fn key_fn list =
  match list with
  | [] -> None
  | [b] -> Some b
  | hd::tl ->
  let folder (acc: 'a) (x: 'a) : 'a =
    if fn (key_fn x) (key_fn acc)
    then x
    else acc
  in
  Some (List.fold_left folder hd tl)

(* Returns intersect entrace and hit distance
See https://www.scratchapixel.com/lessons/3d-basic-rendering/minimal-ray-tracer-rendering-simple-shapes/ray-sphere-intersection*)
let intersect ray obj =
  let { shape; _ } = obj in
  match shape with
  | Sphere (centre, radius) ->
     let o_to_c = centre @- ray.origin in
     let tca = dot o_to_c ray.direction in
     let sphere_behind_ray = tca < 0. in
     if sphere_behind_ray
     then
       let hit_dist = length o_to_c in
       if hit_dist <= radius  (* is origin inside sphere? *)
       then Some (obj, hit_dist)
       else None
     else (* else centre of sphere projects on the array *)
       let dist_sqrd = (dot o_to_c o_to_c) -. (tca *. tca) in
       let radius_sqrd = radius *. radius in
       if dist_sqrd > radius_sqrd
       then None
       else
         let thc = sqrt (radius_sqrd -. dist_sqrd) in
         let t0 = tca -. thc in
         let t1 = tca +. thc in
         let hit_dist = min t0 t1 in
         Some (obj, hit_dist)

(* Returns closest of all the hits, along with the hit point and normal *)
let intersect_scene ray objects =
  let hits = List.filter_map (intersect ray) objects in
  let closest = find_by (<) (fun (_, hit_dist) -> hit_dist) hits in
  match closest with
  | None -> None
  | Some (obj, hit_dist) ->
     let hit_point = ray.origin @+ (ray.direction @*. hit_dist) in
     let hit_normal = match obj.shape with
       | Sphere (centre, _) -> hit_point @- centre |> normalize in
     Some (obj, hit_dist, hit_point, hit_normal)

(* Returns the colour of the ray that intersected an object
See https://github.com/ssloy/tinyraytracer/commit/9a728fff2bbebb1eedd86e1ac89f657d43191609
https://www.youtube.com/watch?v=5apJJKd4z-
http://learnwebgl.brown37.net/09_lights/lights_specular.htm*)
let hit_colour ray hit_obj hit_point hit_normal { objects; lights } =  
  let diffuse_intensity light_intensity light_dir =
    light_intensity *. (max 0. (min 1. (dot hit_normal light_dir)))
  in
  let specular_intensity light_dir =
    let reflect = light_dir @- ((hit_normal @*. dot light_dir hit_normal) @*. 2.) in
    (* sets the intensity on the back half of the object to 0 *)
     Float.pow (dot reflect ray.direction |> max 0.) 50.
  in
  let shadowed light_dir light_dist =
    let shadow_origin = if length (light_dir @* hit_normal) < 0.
                        then hit_point @- (hit_normal @*. 1e-3)
                        else hit_point @+ (hit_normal @*. 1e-3)
    in
    let shadow_ray = { origin = shadow_origin; direction = light_dir } in
    let shadow_hits = List.filter_map (intersect shadow_ray) objects in
    List.exists (fun (_, shadow_hit_dist) ->
        shadow_hit_dist < light_dist)
      shadow_hits
  in
  let accumulate_lighting (di_acc, si_acc, amb_acc) = function
    | Ambient_light intensity ->
       (di_acc, si_acc, amb_acc +. intensity)
    | Point_light (pos, intensity) ->
       let light_to_hp = pos @- hit_point in
       let light_dir = normalize light_to_hp in
       let light_dist = length light_to_hp in
       if shadowed light_dir light_dist
       then (di_acc, si_acc, amb_acc)
       else
         let di = di_acc +. diffuse_intensity intensity light_dir in
         let si = si_acc +. specular_intensity light_dir in
         (di, si, amb_acc)
  in
  let (di, si, amb) = List.fold_left accumulate_lighting (0., 0., 0.) lights in
  Fun.(hit_obj.colour
       |> flip (@*.) di
       |> flip (@+.) (si *. hit_obj.specular_component)
       |> flip (@+.) amb)

let cast_ray ray scene =
  let background_colour = { x = 0. ; y = 0. ; z = 0. } in
  match intersect_scene ray scene.objects with
  | Some (closest_obj, _, hit_point, hit_normal) ->
     hit_colour ray closest_obj hit_point hit_normal scene
  | None -> background_colour

(* calculates the 3d coords to cast a ray at, from the raster grid the image will be drawn with
See https://www.scratchapixel.com/lessons/3d-basic-rendering/ray-tracing-generating-camera-rays/generating-camera-rays
antialiasing: https://raytracing.github.io/books/RayTracingInOneWeekend.html#antialiasing *)
let raster_to_camera_coord axis_pos axis_scale y_coord =
  let degrees_to_radians x = Float.pi *. (x /. 180.) in
  (* let radians_to_degrees x = (radians *. 180.) / pi in *)
  (* Normalized Device Coordinates, mapping raster space to world space [0,1] *)
  let ndc = (Float.of_int axis_pos +. Random.float 1.0) /. Float.of_int axis_scale in
  (* convert x ndc to screenspace [-1,1] *)
  let screenspace_coord = if y_coord
                          then 1. -. (ndc *. 2.)
                          else (ndc *. 2.) -. 1. in
  let fov_angle = 90. in
  let scale = degrees_to_radians (fov_angle *. 0.5) |> Float.tan in
  screenspace_coord *. scale

let render height width scene =
  let no_cells = height * width in
  let data = Array.make no_cells { x=0.; y = 0.; z = 0. } in
  let no_samples = 50 in
  for j = 0 to height - 1 do
    for i = 0 to width - 1 do
      let total_colour = ref { x = 0.; y = 0.; z = 0. } in
      for _ = 0 to no_samples - 1 do
        (* map each x/y coord to ndc (map to real world) to calculate direction of ray *)
        let x = (raster_to_camera_coord i width false
                 *. ((Float.of_int width) /. (Float.of_int height))) in  (* correct for aspect ratio, assuming width > height *)
        let y = raster_to_camera_coord j height true in
        let direction = normalize { x; y; z = -. 1. } in
        let ray = { origin = { x = 0.; y = 0.; z = 0. };
                    direction } in
        total_colour := !total_colour @+ cast_ray ray scene;
      done;
      data.(i + (j * width)) <- !total_colour @/. Float.of_int no_samples
    done;
  done;
  { data; width; height} 

(* Calculate colours from given vectors, clamps max values to 255
to prevent issues when diffuse light intensity is too high, and prints to file *)
let print_fb filename { data; width; height } =
  let oc = open_out filename in
  let to_rgb_channel f =
      ( f *. 255. ) |> Int.of_float |> min 255
  in
  try
    Printf.fprintf oc "P3\n%i %i\n255\n" width height;
    Array.iter (fun v ->
        Printf.fprintf oc "%i %i %i\n"
          (to_rgb_channel v.x)
          (to_rgb_channel v.y)
          (to_rgb_channel v.z))
      data;
    close_out oc
  with e ->
    close_out_noerr oc;
    raise e

let () =
  let scene =
    { objects = [ { shape = Sphere ({ x = -0.7; y = -0.4; z = -9.0 }, 3.);
                    colour = { x = 0.0; y = 0.0; z = 1.0 }; 
                    specular_component = 0.4 };
                  { shape = Sphere ({ x = 0.8; y = 1.1; z = -3.0 }, 1.);
                    colour = { x = 0.0; y = 1.0; z = 0.0 };
                    specular_component = 0.4 };
                  { shape = Sphere ({ x = 0.5; y = 0.5; z = -2.0 }, 0.2);
                    colour = { x = 1.0; y = 1.0; z = 0.0 };
                    specular_component = 0.4 };
                  { shape = Sphere ({ x = 0.2; y = 0.2; z = -7.0 }, 1.4);
                    colour = { x = 1.; y = 0.0; z = 0.0 };
                    specular_component = 0.4 };
                  { shape = Sphere ({ x = 0.0; y = -14.0; z = -9.0 }, 12.);
                    colour = { x = 1.; y = 0.5; z = 0.0 };
                    specular_component = 0.4 }
                ];
      lights = [ Point_light ({ x = -20.; y = 20.; z = 20. }, 0.5);
                 Point_light ({ x = 30.; y = 50.; z = -25. }, 0.5);
                 Point_light ({ x = 30.; y = 20.; z = 30. }, 0.5); 
                 Ambient_light (0.1) ] }
  in
  render 1000 1000 scene |> print_fb "out.ppm" 

open Vector

(********************
 * TYPE DEFINITIONS *
 ********************)

(* framebuffer type to hold a rendered scene, and its dimensions *)
type fb = { data: Vector.t array; width: int; height: int }

(* renderable shapes *)
type shape = Sphere of Vector.t * float

(* materials affect the colour of a pixel when its ray hits a shape *)
type material = { diffuse_colour: Vector.t;
                  specular_exp: float;
                  diffuse_albedo: float;
                  specular_albedo: float;
                  reflective_albedo: float;
                  refractive_albedo: float;
                  refractive_index: float }

(* a hittable is an object in the world to be rendered *)
type hittable = { shape: shape; material: material }

(* lights are not rendered themselves, but affect the brightness of hittables
   and create shadows *)
type light =
  | Point_light of Vector.t * float
  | Ambient_light of float

(* scenes are collections of things to render, and lights *)
type scene = { objects : hittable list; lights: light list }

(* rays are cast at the scene to determine pixel colours *)
type ray = { origin: Vector.t; direction: Vector.t } 

(***********
 * GLOBALS *
 ***********)

(* We limit reflection depth for performance reasons *)
let reflection_depth_limit = 4
let background_colour = { x = 0.2 ; y = 0.7 ; z = 0.8 }

(****************
 * UTILITY FUNS *
 ****************)

(* reduces `list` with `fn`, accessing each element with key_fn 
   example: find_by (<) (fun (_, hit_dist) -> hit_dist) hits *)
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

(***********************
 * MAIN IMPLEMENTATION *
 ***********************)

(* If a ray hits the obj, returns intersect entrace and hit distance
   See https://www.scratchapixel.com/lessons/3d-basic-rendering/minimal-ray-tracer-rendering-simple-shapes/ray-sphere-intersection*)
let intersect ray obj =
  match obj.shape with
  | Sphere (centre, radius) ->
    let o_to_c = centre @- ray.origin in
    let tca = dot o_to_c ray.direction in
    let dist_sqrd = (dot o_to_c o_to_c) -. (tca *. tca) in
    let radius_sqrd = radius *. radius in
    if dist_sqrd > radius_sqrd
    then None
    else
      let thc = sqrt (radius_sqrd -. dist_sqrd) in
      let t0 = ref (tca -. thc) in
      let t1 = tca +. thc in
      if !t0 < 0. then t0 := t1;
      if !t0 < 0. then None else Some (obj, !t0)

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
let rec hit_colour ray hit_obj hit_point hit_normal scene depth =
  let reflect light_dir hit_normal = light_dir @- ((hit_normal @*. dot light_dir hit_normal) @*. 2.) in
  (* offsets a ray's origin to prevent accidental occlusion *)
  let offset_origin ray_direction =
    let outside = (dot ray_direction hit_normal) < 0. in
    let (@+/-) = if outside then (@-) else (@+) in
    hit_point @+/- (hit_normal @*. 1e-3)
  in
  let reflect_colour =
    let reflected_dir = normalize (reflect ray.direction hit_normal) in
    let reflect_ray = { origin = offset_origin reflected_dir; direction = reflected_dir } in
    cast_ray reflect_ray scene (depth + 1)
  in
  let refract_colour =
    (* refraction using Snell's law, will often be called leaving air, which is RI 1. *)
    let rec refract light_dir hit_normal material_refractive_index air_refractive_index =
      let cosi = dot light_dir hit_normal |> max (-1.) |> min 1. |> (~-.) in
      if cosi < 0. (* if the ray comes from the inside the object, swap the air and the media *)
      then refract light_dir (negate hit_normal) air_refractive_index material_refractive_index
      else
        let eta = air_refractive_index /. material_refractive_index in
        let k = 1. -. eta *. eta *. (1. -. cosi *. cosi) in
        if k < 0. (* total internal reflection *)
        then { x = 0.; y = 0.; z = 0. }
        else (light_dir @*. eta) @+ (hit_normal @*. (eta *. cosi -. sqrt k)) 
    in
    let refracted_dir = normalize (refract ray.direction hit_normal hit_obj.material.refractive_index 1.) in
    let refract_ray = { origin = offset_origin refracted_dir; direction = refracted_dir } in
    cast_ray refract_ray scene (depth + 1)
  in
  let diffuse_intensity light_intensity light_dir =
    light_intensity *. (max 0. (min 1. (dot hit_normal light_dir)))
  in
  let specular_intensity light_intensity light_dir =
    let reflect = reflect light_dir hit_normal in
    (* sets the intensity on the back half of the object to 0 *)
    light_intensity *. (Float.pow (dot reflect ray.direction |> max 0.) hit_obj.material.specular_exp)
  in
  let shadowed light_dir light_dist =
    (* offset the shadow ray's origin to avoid occlusion *)
    let shadow_ray = { origin = offset_origin light_dir; direction = light_dir } in
    let shadow_hits = List.filter_map (intersect shadow_ray) scene.objects in
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
        let si = si_acc +. specular_intensity intensity light_dir in
        (di, si, amb_acc)
  in
  let (di, si, amb) = List.fold_left accumulate_lighting (0., 0., 0.) scene.lights in
  Fun.(hit_obj.material.diffuse_colour
       |> flip (@*.) (di *. hit_obj.material.diffuse_albedo)
       |> flip (@+.) (si *. hit_obj.material.specular_albedo) 
       |> flip (@+) (reflect_colour @*. hit_obj.material.reflective_albedo)
       |> flip (@+) (refract_colour @*. hit_obj.material.refractive_albedo)
       |> flip (@+.) amb)

and cast_ray ray scene depth =
  if depth = reflection_depth_limit
  then background_colour
  else
    match intersect_scene ray scene.objects with
    | Some (closest_obj, _, hit_point, hit_normal) ->
      hit_colour ray closest_obj hit_point hit_normal scene depth
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

(* Render a scene to a framebuffer *)
let render height width scene =
  let no_cells = height * width in
  let data = Array.make no_cells { x=0.; y = 0.; z = 0. } in
  let no_samples = 50 in
  for j = 0 to height - 1 do
    for i = 0 to width - 1 do
      let total_colour = ref { x = 0.; y = 0.; z = 0. } in
      (* supersampling for basic antaliasing, each ray per sample has a slight random adjustment,
         which we average out per pixel *)
      for _ = 0 to no_samples - 1 do
        (* map each x/y coord to ndc (map to real world) to calculate direction of ray *)
        let x = (raster_to_camera_coord i width false
                 (* correct for aspect ratio, assuming width > height *)
                 *. ((Float.of_int width) /. (Float.of_int height))) in  
        let y = raster_to_camera_coord j height true in
        let direction = normalize { x; y; z = -. 1. } in
        let ray = { origin = { x = 0.; y = 0.; z = 0. };
                    direction } in
        total_colour := !total_colour @+ cast_ray ray scene 0;
      done;
      data.(i + (j * width)) <- !total_colour @/. Float.of_int no_samples
    done;
  done;
  { data; width; height} 

(* prints a framebuffer to a file.
   Calculates colours from given vectors, clamps max values to 255
   to prevent issues when diffuse light intensity is too high, and prints to file *)
let print_fb filename { data; width; height } =
  let oc = open_out filename in
  let to_rgb_channel f =
    ( f *. 255. ) |> Int.of_float |> min 255
  in
  let print_pixel p = 
    Printf.fprintf oc "%i %i %i\n"
      (to_rgb_channel p.x)
      (to_rgb_channel p.y)
      (to_rgb_channel p.z)
  in
  try
    Printf.fprintf oc "P3\n%i %i\n255\n" width height;
    Array.iter print_pixel data;
    close_out oc
  with e ->
    close_out_noerr oc;
    raise e

(***************
 * ENTRY POINT *
 ***************)

let () =
  (* Set up materials *)
  let ivory = { diffuse_colour = { x = 0.4; y = 0.4; z = 0.3 };
                diffuse_albedo = 0.6;
                specular_albedo = 0.3;
                reflective_albedo = 0.1;
                refractive_albedo = 0.;
                specular_exp = 50.;
                refractive_index = 1. } in
  let red_rubber = { diffuse_colour = { x = 0.3; y = 0.1; z = 0.1 };
                     diffuse_albedo = 0.9;
                     specular_albedo = 0.1;
                     reflective_albedo = 0.;
                     refractive_albedo = 0.;
                     specular_exp = 10.;
                     refractive_index = 1. } in
  let mirror = { diffuse_colour = { x = 1.; y = 1.; z = 1. };
                 diffuse_albedo = 0.;
                 specular_albedo = 10.;
                 reflective_albedo = 0.8;
                 refractive_albedo = 0.;
                 specular_exp = 1425.;
                 refractive_index = 1. } in
  let glass = { diffuse_colour = { x = 0.6; y = 0.7; z = 0.8 };
                diffuse_albedo = 0.;
                specular_albedo = 0.5;
                reflective_albedo = 0.1;
                refractive_albedo = 0.8;
                specular_exp = 125.;
                refractive_index = 1.5 } in
  (* Construct scene and render *)
  let scene =
    { objects = [ { shape = Sphere ({ x = -3.; y = 0.; z = -16.0 }, 2.); material = ivory; };
                  { shape = Sphere ({ x = -1.; y = -1.5; z = -12.0 }, 2.); material = glass; };
                  { shape = Sphere ({ x = 1.5; y = -0.5; z = -18.0 }, 3.); material = red_rubber; };
                  { shape = Sphere ({ x = 7.; y = 5.; z = -18.0 }, 4.); material = mirror; };
                ];
      lights = [ Point_light ({ x = -20.; y = 20.; z = 20. }, 1.5);
                 Point_light ({ x = 30.; y = 50.; z = -25. }, 1.8);
                 Point_light ({ x = 30.; y = 20.; z = 30. }, 1.7); 
                 Ambient_light (0.0)
               ] }
  in render 1000 1000 scene |> print_fb "out.ppm" 

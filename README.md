# ocaml-raytracer
A raytracer implementation writen in OCaml

This is a toy raytracer implementation, ported from my initial Common Lisp implementation.

It uses dune, you can test this by running

dune exec ./raytracer.exe

It currently implements:

* spheres only
* reflection
* refraction
* shadows
* diffuse & specular lighting

I plan to implement:

* implement other object types, including meshes
* fresnel ration based refraction / reflection
* file based configuration

Here is a converted PNG of the image it produces:

![Raytraced Image](https://github.com/cjallen88/ocaml-raytracer/blob/master/demo.png)

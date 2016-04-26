#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"ppx_optcomp"
  [ oasis_lib "ppx_optcomp"
  ; file "META" ~section:"lib"
  ; oasis_exe "ppx-optcomp" ~dest:"ppx-optcomp"
  ]

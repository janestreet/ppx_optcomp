  $ ./pp_optcomp.exe - -impl <<-EOF
  > [%% if defined abc ]
  > let _ = "abc is defined"
  > [%% else]
  > let _ = "abc is NOT defined"
  > [%%endif]
  File "-", line 1, characters 15-18:
  Error: optcomp: doesn't know about abc.
  You need to either define it or undefine it with #undef.
  Optcomp doesn't accept variables it doesn't know about to avoid typos.
  [1]

  $ ./pp_optcomp.exe -cookie ppx_optcomp.env='env ~abc:(Defined 1)' - -impl <<-EOF
  > [%%if defined abc ]
  > let _ = "abc is defined"
  > [%%else]
  > let _ = "abc is NOT defined"
  > [%%endif]
  let _ = "abc is defined"

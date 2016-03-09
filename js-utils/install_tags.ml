let package_name = "ppx_optcomp"

let sections =
  [ ("lib",
    [ ("built_lib_ppx_optcomp", None)
    ],
    [ ("META", None)
    ])
  ; ("bin",
    [ ("built_exec_ppx-optcomp", Some "ppx-optcomp")
    ],
    [])
  ]

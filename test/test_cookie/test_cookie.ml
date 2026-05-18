[%%if defined abc]

let x = "abc is defined"

[%%else]

let x = "abc is NOT defined"

[%%endif]

let%expect_test _ =
  print_string x;
  [%expect {| abc is defined |}]
;;

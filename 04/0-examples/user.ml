type user_info = {
  name : string; pass : string; birth : int;
}

exception Invalid_user

let get_user_info () =
  let prompt m = print_string m; read_line () in
  let name = prompt "이름? " in
  let pass = prompt "패스워드? " in
  let birth = int_of_string (prompt "몇년생? ") in
  if 2012 - birth + 1 < 20 then (
    print_endline "미성년자 출입금지!!";
    raise Invalid_user
  );
  { name; pass; birth }

let diamond n =
  let line i =
    for j = 1 to n - i do
      print_string " "
    done;
    for j = 1 to i * 2 - 1 do
      print_string "*"
    done;
    print_string "\n"
  in
  for i = 1 to n do line i done;
  for i = n - 1 downto 1 do line i done


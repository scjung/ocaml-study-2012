type person = {
  family_name : string;
  given_name : string;
  age : int
}

let rec kims persons =
  match persons with
  | [] -> []
  | ({ family_name = "Kim"; _ } as p) :: t -> p :: kims t
  | _ :: t -> kims t

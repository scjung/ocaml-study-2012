type time = { min : int; sec : int }

let add_time t1 t2 =
  let sec = t1.sec + t2.sec in
  { min = t1.min + t2.min + (sec / 60); sec = sec mod 60 }

type song = {
  title : string; artist : string; time : time
}

let rec playingtime songs =
  match songs with
  | [] -> { min = 0; sec = 0 }
  | { time; _ } :: t -> add_time time (playingtime t)

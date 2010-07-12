Random.self_init ()

type cell = Full | Empty | OutOfBounds
type kind = Earth | Sea | Island | Lake | None

let xsize = 200
let ysize = 120
let mult = 5

let universe = Array.init ysize (function
                                   | 0 -> Array.make xsize Full
                                   | _ -> Array.make xsize Empty)

let status () = Array.make_matrix xsize ysize None

let get_cell x y =
  try universe.(y).(x)
  with Invalid_argument _ -> OutOfBounds

let put_cell x y value =
  try universe.(y).(x) <- value
  with Invalid_argument _ -> ()

let erode () =
  let x = Random.int xsize in
  let y = Random.int ysize in
  match get_cell x y with
    | OutOfBounds -> false
    | Empty -> false
    | Full ->
        if y = 0 then begin put_cell x (y + 1) Full; true end
        else
          let d1 = Random.int 2 * 2 - 1 in
          let d2 = Random.int 2 in
          let newx = x + d1 * d2 in
          let newy = y + d1 * (1 - d2) in
          let cell = get_cell newx newy in
            match cell with
              | Full -> false
              | OutOfBounds -> put_cell x y Empty; true
              | Empty -> put_cell x y Empty ; put_cell newx newy Full; true

let rec find_component current what status mark =
  match current with
    | [] -> status
    | (x_cur, y_cur) :: tl ->
      let () = status.(x_cur).(y_cur) <- mark in
      let neighbours = [ (x_cur - 1, y_cur);
			 (x_cur + 1, y_cur);
			 (x_cur, y_cur - 1);
			 (x_cur, y_cur + 1) ] in
      let check (x, y) =
	get_cell x y = what &&
	status.(x).(y) != mark &&
	not (List.mem (x, y) current) in
      let to_add = List.filter check neighbours in
      find_component (tl @ to_add)  what status mark

let find_earth () = find_component [(0,0)] Full (status ()) Earth

let dark_green = Graphics.rgb 0 128 0

let is_island x y =
  get_cell (x-1) y = Full ||
  get_cell (x+1) y = Full ||
  get_cell x (y-1) = Full ||
  get_cell x (y+1) = Full

let paint_universe () =
  let status = find_earth () in
  for i = 0 to xsize - 1 do
    for j = 0 to ysize - 1 do
      Graphics.set_color (
	if status.(i).(j) = Earth then dark_green else
	  if get_cell i j = Full then begin
	    if is_island i j then Graphics.green else Graphics.yellow
	  end
	  else Graphics.blue) ;
      Graphics.fill_rect (mult*i) (mult*j) mult mult
    done
  done;
  Graphics.synchronize ()

let rec iterate () =
  if erode () then paint_universe () else () ;
  iterate ()

;;
let () = Graphics.open_graph (" " ^ (string_of_int (mult*xsize)) ^ "x" ^ (string_of_int (mult*ysize))) in
let () = Graphics.auto_synchronize false in
iterate ()

Random.self_init ()

type cell = Full | Empty | OutOfBounds
type kind = Earth | Sea | Island | Lake | None

let xsize = 100
let ysize = 40
let mult = 10
let div = 9

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

let rec erode () =
  let x = Random.int xsize in
  let y = Random.int ysize in
  match get_cell x y with
    | OutOfBounds -> erode ()
    | Empty -> erode ()
    | Full ->
        if y = 0 then begin put_cell x (y + 1) Full; (x,y); end
        else
          let d1 = Random.int 2 * 2 - 1 in
          let d2 = Random.int 2 in
          let newx = x + d1 * d2 in
          let newy = y + d1 * (1 - d2) in
          let cell = get_cell newx newy in
            match cell with
              | Full -> erode()
              | OutOfBounds -> put_cell x y Empty; (x,y)
              | Empty -> put_cell x y Empty ; put_cell newx newy Full; (x,y)

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

let rec calc_d d big_x big_y i j =
  let d' = d + (if (get_cell i j = Full) then 1 else 0) in
  if i < big_x*div + div - 1
  then calc_d d' big_x big_y (i+1) j
  else begin
    if j < big_y*div + div - 1 then calc_d d' big_x big_y (big_x*div) (j+1)
    else d'
  end
      
let calculate_density big_x big_y =
  calc_d 0 big_x big_y (big_x*div) (big_y*div)

let get_color big_x big_y =
  let d = calculate_density big_x big_y in
  if (d < div * div / 2)
  then Graphics.rgb 0 0 (63 + 192*2*d/(div*div))
  else Graphics.rgb 0 (63 + 192*2*(div*div-d)/(div*div)) 0    

let rec calc_ad d x y i j =
  let d' = d + (if (y+j) < 0 or (get_cell (x+i) (y+j) = Full) then 1 else 0) in
  if i < div/2
  then calc_ad d' x y (i+1) j
  else begin
    if j < div/2 then calc_ad d' x y (-div/2) (j+1)
    else d'
  end

let calculate_average_density x y =
  calc_ad 0 x y (-div/2) (-div/2)

let get_average_color x y =
  let d = calculate_average_density x y in
  if d = div * div / 2 then Graphics.rgb 192 192 0
  else if d = div * div / 2 + 1 then Graphics.rgb 255 255 0 else begin
    if (d <= div * div / 2)
    then Graphics.rgb 0 0 (63 + 192 * 2 * d / (div * div))
    else Graphics.rgb 0 (63 + 192 * 2 * (div * div - d)/(div * div)) 0
  end
(*
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
*)

(*
let paint_universe big_x big_y =
  Graphics.set_color (get_color big_x big_y);
  Graphics.fill_rect (mult * big_x) (mult * big_y) mult mult
*)

let paint_universe x y =
  if x >= 0 && y >= 0 && x < xsize && y < ysize then begin
    Graphics.set_color (get_average_color x y);
    Graphics.fill_rect (x*mult) (y*mult) mult mult
  end
  else ()

let paint_full_universe () =
  for i = 0 to xsize - 1 do
    for j = 0 to ysize - 1 do
      paint_universe i j
    done
  done;
  Graphics.synchronize ()

let rec iterate () =
  let i, j = erode () in
  for k = i - div/2 - 1 to i + div/2 + 1 do
    for l = j - div/2 - 1 to j + div/2 + 1 do
      paint_universe k l
    done
  done ;
  Graphics.synchronize () ;
  iterate ()

;;

let () = Graphics.open_graph (" " ^ (string_of_int (mult*xsize)) ^ "x" ^ (string_of_int (mult*ysize))) in
let () = Graphics.auto_synchronize false in
paint_full_universe () ;
iterate ()

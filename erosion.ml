Random.self_init ()

type cell = Full | Empty | OutOfBounds
type kind = Earth | Sea | Island | Lake | None

let xsize = 90
let ysize = 40
let mult = 15

let scale = 3

let div = 2 * scale + 1

let square = div * div
let half = square / 2
let half1 = half + 1

let universe = Array.init ysize (function
                                   | 0 -> Array.make xsize Full
                                   | _ -> Array.make xsize Empty)

let get_cell x y =
  if x > xsize - 1 || y > ysize - 1 || x < 0 || y < 0 then OutOfBounds
  else universe.(y).(x)

let put_cell x y value =
  if x > xsize - 1 || y > ysize - 1 || x < 0 || y < 0
  then ()
  else universe.(y).(x) <- value

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

let rec calc_ad d x y i j =
  let d' = d + (if (y+j) < 0 or (get_cell (x+i) (y+j) = Full) then 1 else 0) in
  if i < scale
  then calc_ad d' x y (i+1) j
  else begin
    if j < scale then calc_ad d' x y (-scale) (j+1)
    else d'
  end


let calculate_average_density x y =
  calc_ad 0 x y (-scale) (-scale)

let get_average_color x y =
  let d = calculate_average_density x y in
  if d = half then Graphics.rgb 192 192 0
  else if d = half1 then Graphics.rgb 255 255 0 else begin
    if d < half
    then Graphics.rgb 0 0 (63 + 192 * 2 * d / square)
    else Graphics.rgb 0 (63 + 192 * 2 * (square - d)/square) 0
  end

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
  done

let rec iterate () =
  let i, j = erode () in
  for k = i - scale - 1 to i + scale + 1 do
    for l = j - scale - 1 to j + scale + 1 do
      paint_universe k l
    done
  done;
  iterate ()

;;

let () = Graphics.open_graph (" " ^ (string_of_int (mult*xsize)) ^ "x" ^ (string_of_int (mult*ysize))) in
let () = paint_full_universe () in
iterate ()

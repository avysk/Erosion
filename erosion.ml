Random.self_init () ;;

type cell = Full | Empty | OutOfBounds
type kind = Earth | Sea | Island | Lake | None

let xsize = 4
let ysize = 3

let universe = Array.init ysize (function
                                   | 0 -> Array.make xsize Full
                                   | _ -> Array.make xsize Empty)

let status = Array.make ysize (Array.make xsize None)

let get_cell x y =
  try universe.(y).(x)
  with Invalid_argument _ -> OutOfBounds

let put_cell x y value =
  try universe.(y).(x) <- value
  with Invalid_argument _ -> ()

let erode _ =
  let x = Random.int xsize in
  let y = Random.int ysize in
  match get_cell x y with
    | OutOfBounds -> ()
    | Empty -> ()
    | Full ->
        if y = 0 then put_cell x (y + 1) Full
        else
          let d1 = Random.int 2 * 2 - 1 in
          let d2 = Random.int 2 in
          let newx = x + d1 * d2 in
          let newy = y + d1 * (1 - d2) in
          let cell = get_cell newx newy in
            match cell with
              | Full -> ()
              | OutOfBounds -> put_cell x y Empty
              | Empty -> put_cell x y Empty ; put_cell newx newy Full

let rec find_component component =
  let x_cur, y_cur = List.hd component in
  let x_dec = x_cur - 1 in
  let x_inc = x_cur + 1 in
  let y_dec = y_cur - 1 in
  let y_inc = y_cur + 1 in
  let check x y l = not (List.mem (x, y) l) && get_cell x y = Full in
  let tmp1 =
    if check x_dec y_dec component
    then find_component (x_dec, y_dec) :: component
    else component in
  let tmp2 =
    if check x_dec y_inc tmp1
    then find_component (x_dec, y_inc) :: tmp1
    else tmp1 in
  let tmp3 = 
    if check x_inc y_dec tmp2
    then find_component (x_inc, y_dec) :: tmp2
    else tmp2 in
    tmp3 


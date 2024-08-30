open Expression

let (let*) = Option.bind

let rec eval (e : expr) : int option =
  match e with
  | Num x -> Some x
  | Plus (x, y) ->
      let* x_num = eval x in
      let* y_num = eval y in
      Some (x_num + y_num)
  | Minus (x, y) ->
      let* x_num = eval x in
      let* y_num = eval y in
      if x_num - y_num >= 0 then Some (x_num - y_num) else None
  | Times (x, y) ->
      let* x_num = eval x in
      let* y_num = eval y in
      Some (x_num * y_num)
  | Divide (x, y) ->
      let* x_num = eval x in
      let* y_num = eval y in
      if y_num != 0 && x_num mod y_num = 0 then Some (x_num / y_num) else None


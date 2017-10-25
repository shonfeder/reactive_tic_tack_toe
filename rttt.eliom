[%%shared
  open Eliom_lib
  open Eliom_content
  open Html.D
  open Aux
]

module State = struct
  let games = ref []
  let game_id = ref 0
  let new_game () =
    game_id := !game_id + 1;
    games   := game_id :: !games;
    !game_id

  let lookup_game id =
    if List.mem (ref id) (!games)
    then Some id
    else None
end

module Rttt = struct
  module Info = struct
    let application_name = "rttt"
    let global_data_path = None
  end
  module App = Eliom_registration.App (Info)
end

module Service = struct
  open Eliom_service
  module Param = Eliom_parameter

  let main =
    create
      ~path:(Path [])
      ~meth:(Get Param.unit)
      ()

  let game =
    create
      ~path:(Path ["game"])
      ~meth:(Get (Param.int "id"))
      ()

  let games =
    create
      ~path:(Path ["games"; ""])
      ~meth:(Get Param.unit)
      ()

end

[%%shared
module Game = struct

  type coord = int * int
  [@@deriving json]

  module Piece = struct

    type t =
      | X
      | O
    [@@deriving json]

    let switch
      : t -> t = function
      | X -> O
      | O -> X

    let compare
      : t -> t -> int
      = fun a b ->
        match a, b with
        | X, O -> 1
        | O, X -> -1
        | _    -> 0
  end

  module Square = struct
    type t = Piece.t option
    [@@deriving json]

    let empty : t = None

    let is_empty
      : t    -> bool = function
      | None -> true
      | _    -> false

    let place
      : t      -> Piece.t -> (t, Piece.t) result = function
      | None   -> fun p   -> Ok (Some p)
      | Some p -> fun _   -> Error p
  end

  module Board = struct

    type row = Square.t list
    [@@deriving json]
    type t   = Square.t list list
    [@@deriving json]
    type three_in_a_row = Piece.t * coord list
    [@@deriving json]

    exception Board

    let empty : t =
      let row = [None; None; None] in
      [row; row; row]

    let square
      : t -> coord -> Square.t
      = fun b (x,y) ->
        if x < 0 || x > 2 || y < 0 || y > 2
        then raise Board
        else List.nth (List.nth b y) x

    let place
      : t -> Piece.t -> coord -> (t, Piece.t) result =
      fun b pc (x,y) ->
        let place_in_col index sq =
          if index = x then Square.place sq pc else Ok sq
        in
        let place_in_row index row =
          if index = y
          then List.mapi place_in_col row
               |> Monad.Result.(sequence return)
          else Ok row
        in
        List.mapi place_in_row b
        |> Monad.Result.(sequence return)

    let has_free_space
      : t -> bool
      = fun b -> List.map (List.for_all Option.is_some) b |> List.for_all id

    let three_in_a_row
      : t -> three_in_a_row option
      = fun b ->
        let single_kind_of_piece pieces =
            match List.sort_uniq Piece.compare pieces with
            | [pc] -> Some pc
            | _    -> None
        in
        let in_a_row line =
          let coords  = List.map fst line in
          let squares = List.map snd line in
          let open Monad.Option
          in
          sequence return squares
          >>= single_kind_of_piece
          >>= fun piece -> Some (piece, coords)
        in
        (* TODO Refactor out the matrix of coords into a value generated once *)
        let range = range 0 2 in
        let rev_range = List.rev range in
        let rows =
          List.map (fun y -> (List.map (fun x -> (x,y)) range)) range
        in
        let cols =
          List.map (fun x -> (List.map (fun y -> (x,y)) range)) range
        in
        let diagonals =
          [ List.map (fun x -> (x, x)) range
          ; List.map (fun x -> (x, x)) rev_range ]
        in
        let all_lines = rows @ cols @ diagonals in
        let a_line_of_coords_and_squares coords =
          List.map (fun coord -> coord, square b coord) coords
        in
        let lines_of_coords_and_squares =
          List.map a_line_of_coords_and_squares all_lines
        in
        match
          List.map in_a_row lines_of_coords_and_squares
          |> List.filter Option.is_some
        with
        | [three_in_a_row] -> three_in_a_row
        | _ -> raise Board
  end

  module Turn = struct
    type status =
      | Move    of Piece.t
      | Victory of Board.three_in_a_row
      | Draw
    [@@deriving json]

    type t = { status : status
             ; board  : Board.t }
    [@@deriving json]

    let next
      : t -> t = function
      | {status=Move pc; board} ->
        let status =
          if Board.has_free_space board
          then Draw
          else match Board.three_in_a_row board with
            | None     -> Move (Piece.switch pc)
            | Some row -> Victory row
        in
        {status; board}
      | turn -> turn

    let first : t =
      { status = Move Piece.X
      ; board  = Board.empty }
  end

  type t = Turn.t list
  [@@deriving json]

  let move
    : t -> Piece.t -> coord -> (t, t) result
    = fun game pc coord ->
      let make_move turn = match turn with
        | Turn.{status = Move pc; board} ->
          begin
            match Board.place board pc coord with
            | Ok board -> Ok (Turn.(next {turn with board}) :: game)
            | Error _  -> Error game
          end
        | _ -> Error game
      in
      match game with
      | []        -> Ok [Turn.first]
      | turn :: _ -> make_move turn
end
]

[%%client
open Eliom_content.Html

let split s =
  let len = String.length s in
  let rec aux acc = function
    | 0 -> acc
    | n -> aux (s.[n - 1] :: acc) (pred n)
  in aux [] len

let value_signal, set_value = React.S.create "initial"

let value_len = React.S.map String.length value_signal

let content_signal : Html_types.div_content_fun elt React.signal =
  let char_to_paragraph c = F.p [F.pcdata (Printf.sprintf "%c" c)] in
  let stack_chars str =
    let chars = split str in
    F.div (List.map char_to_paragraph chars)
  in
  React.S.map stack_chars value_signal

let make_color len =
  let d = (len * 10) mod 255 in
  Printf.sprintf "color: rgb(%d,%d,%d)" d d d

let make_client_nodes () =
  let color = [R.a_style (React.S.map make_color value_len)] in
  [ D.p [R.pcdata value_signal]
  ; D.p ~a:color [R.pcdata value_signal]
  ; R.node content_signal ]

]

let make_input () =
  let open Html in
  let inp = D.Raw.input ~a:[F.a_input_type `Text] () in
  let _ =
    [%client
    (let open Lwt_js_events in
     async (fun () ->
       let inp = To_dom.of_input ~%inp in
       keyups inp (fun _ _ ->
         let s = Js.to_string (inp##.value) in
         set_value s;
         Lwt.return ()))
     : unit)
    ]
  in
  inp

module Handler = struct
  let basic_page = Eliom_tools.F.html ~css:[["css"; "rttt.css"]]

  let main _get _post =
    let open Eliom_content.Html in
    let inp = make_input () in
    let cldiv = C.node [%client D.div (make_client_nodes ())] in
    let body = let open Html.F in
      body [ h1 [pcdata "Reactive Tic-Tac-Toe"]
           ; inp
           ; F.h2 [pcdata "Client side reactive nodes"]
           ; cldiv ]
    in
    Lwt.return @@ basic_page
      ~title:"rttt"
      body

  let game id _post =
    let body =
      let open Html.F in
      match State.lookup_game id with
      | None ->
        body [h1 [pcdata "Sorry! That game does not exist."]]
      | Some id ->
        let game_number = Printf.sprintf "Game #%i" id in
        body [h1 [pcdata game_number]]
    in
    Lwt.return @@ basic_page
      ~title:("Reactive Tic-Tac-Toe Game")
      body

  let games _get _post =
    let games = "tbd" in
    let body = let open Html.F in
      body [ h1 [pcdata "Ongoing Games"]
           ; p  [pcdata games]]
    in
    Lwt.return @@ basic_page
      ~title:("Ongoing Games")
      body
end

let () =
  Rttt.App.register
    ~service:Service.main
    Handler.main

let () =
  Rttt.App.register
    ~service:Service.game
    Handler.game

let () =
  Rttt.App.register
    ~service:Service.games
    Handler.games

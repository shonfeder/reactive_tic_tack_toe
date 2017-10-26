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

  type t = { guesses   : int
           ; word      : string
           ; correct   : char option array
           ; incorrect : char list}
  [@@deriving show]

  let max_guesses = 6

  let start
    : string -> t
    = fun word ->
      let correct =
        let len = String.length word in
        Array.make len None
      in
      { guesses = 0
      ; word
      ; correct
      ; incorrect = []}

  (* let guess *)
  (*   : t -> char -> t *)
  (*   = fun game char -> *)
  (*     if List.mem char game.turn *)
  (*     then  *)
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

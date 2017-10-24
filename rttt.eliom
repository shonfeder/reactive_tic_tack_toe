[%%shared
  open Eliom_lib
  open Eliom_content
  open Html.D
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

module Handler = struct
  let basic_page = Eliom_tools.F.html ~css:[["css";"rttt.css"]]

  let main _get _post =
    let body = let open Html.F in
      body [h1 [pcdata "Reactive Tic-Tac-Toe"]]
    in
    Lwt.return @@ basic_page
      ~title:"rttt"
      body

  let game id _post =
    let game_number = Printf.sprintf "Game #%i" id in
    let body = let open Html.F in
      body [h1 [pcdata game_number]]
    in
    Lwt.return @@ basic_page
      ~title:("Reactive Tic-Tac-Toe " ^ game_number)
      body
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

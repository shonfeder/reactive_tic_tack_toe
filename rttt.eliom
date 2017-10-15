[%%shared
    open Eliom_lib
    open Eliom_content
    open Html.D
]

module Rttt = struct
  module Info = struct
    let application_name = "rttt"
    let global_data_path = None
  end
  module App = Eliom_registration.App (Info)
end

let main_service =
  let open Eliom_service in
  create ~path:(Path []) ~meth:(Get Eliom_parameter.unit) ()

let main_handler _get _post =
  let body = let open Html.F in
    body [h1 [pcdata "Welcome from Eliom's distillery!"]]
  in
  Lwt.return
    (Eliom_tools.F.html
       ~title:"rttt"
       ~css:[["css";"rttt.css"]]
       body)

let () =
  Rttt.App.register
    ~service:main_service
    main_handler

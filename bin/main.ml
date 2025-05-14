let greet name count =
  for _ = 1 to count do
    Printf.printf "Hello, %s!\n" name
  done

let name_arg =
  let doc = "Name to greet." in
  Cmdliner.Arg.(
    required
    & opt (some string) None
    & info ["n"; "name"] ~docv:"NAME" ~doc
  )

let count_arg = 
  let doc = "Number of times to greet." in
  Cmdliner.Arg.(
    value
    & opt int 1
    & info ["c"; "count"] ~docv:"COUNT" ~doc
  )

let () =
  let (greet_cmd : unit Cmdliner.Cmd.t) =
    let doc = "Print a greeting message." in
    Cmdliner.Cmd.v (Cmdliner.Cmd.info "greet" ~doc) (Cmdliner.Term.(const greet $ name_arg $ count_arg))
  in
  exit (Cmdliner.Cmd.eval greet_cmd)

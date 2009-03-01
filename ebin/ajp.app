{application, ajp,
  [{description, "AJP portocol binding for Erlang"},
  {vsn, "1.0"},
  {modules, [ajp_sup, ajp_server, ajp, gen_ajp_handler, test_ajp_mount]},
  {registered, [ajp_server]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {ajp_app,[]}}]
}.


-module(ddate_iface_http_templates).

-export([compile/0]).

% Compile all templates
compile() ->
    lager:info("Compiling template files."),
    {ok, _} = erlydtl:compile_file(
        code:priv_dir(ddate_iface_http) ++
        "/templates/index.html", ddate_tmpl_index).


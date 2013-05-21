-type regex_post() :: string().
-type operator() :: char(). % currently
-type letter() :: char(). % currently



-type proto_exp() :: {proto_exp, IsFinal::boolean(), dict()}.
 % differs from re_automata1() in value type
-type transition() :: {string(), dict()}. % dict of exp_index() => set(exp_index()) - transition function
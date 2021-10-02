module V = Primitive.Value;;
module T = Primitive.Tp;;
module I = Primitive.Invocation;;
type pertubation = {prog: Language.Oplang.t; acache: Language.Arg_solving.cache}
type non_trivial_info = int list
type t = {sigma: V.t list -> bool;
          client: I.inspector -> V.t list -> (non_trivial_info * (V.t list) option);
          library_inspector: I.inspector;
          phi: V.t list -> bool;
          tps: T.t list;
          i_err: V.t list;
          i_err_non_trivial_info: non_trivial_info;
          op_pool: string list;
          sampling_rounds: int;
          cur_p: pertubation}

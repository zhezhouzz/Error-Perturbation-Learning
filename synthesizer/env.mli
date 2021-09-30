module V = Primitive.Value;;
module T = Primitive.Tp;;
type pertubation = {prog: Language.Oplang.t; acache: Language.Arg_solving.cache}
type t = {sigma: V.t list -> bool;
          client: V.t list -> (V.t list) option;
          phi: V.t list -> bool;
          tps: T.t list;
          i_err: V.t list;
          op_pool: string list;
          sampling_rounds: int;
          cur_p: pertubation}

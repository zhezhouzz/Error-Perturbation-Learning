module V = Primitive.Value
module T = Primitive.Tp
module BB = Language.Bblib

type pertubation = {
  prog : Language.Oplang.t;
  acache : Language.Arg_solving.cache;
}

type non_trivial_info = int list

type sample = V.t list

type t = {
  sigma : V.t list -> bool;
  client : BB.inspector -> V.t list -> non_trivial_info * V.t list option;
  library_inspector : BB.inspector;
  phi : V.t list -> bool;
  tps : T.t list;
  i_err : V.t list;
  i_err_non_trivial_info : non_trivial_info;
  init_sampling_set : sample list;
  op_pool : string list;
  preds : string list;
  sampling_rounds : int;
  p_size : int;
  cur_p : pertubation option;
}

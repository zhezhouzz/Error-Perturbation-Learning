type imp = {
  imp_name : string;
  imp_itps : Tp.t list;
  imp_otps : Tp.t list;
  imp_exec : Value.t list -> Value.t list option;
  nondet : bool;
}

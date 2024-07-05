
module MyLis = Arc_lis.Make(Domain_arc.MyDomain)

let _ =
  Refining.debug := true;
  MyLis.main ()

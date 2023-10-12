
module MyBatch = Batch.Make(Domain_arc.MyDomain)

let _ = MyBatch.main ()

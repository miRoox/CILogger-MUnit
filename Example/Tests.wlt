BeginTestSection["Example"]

VerificationTest[(* 1 *)
  Expand[(1 + x)^3]
	,
  1 + 3 x + 3 x^2 + x^3
]

VerificationTest[(* 2 *)
  Sin[E] < Cos[E]
	,
	True	
]

VerificationTest[(* 3 *)
  1/0
	,
	ComplexInfinity	
]

VerificationTest[(* 4 *)
  RandomInteger[{1, 10}]
	,
  _Integer
	,
	SameTest->MatchQ
]

VerificationTest[(* 5 *)
	Range[10]
	,
  {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
	,
	MemoryConstraint->Quantity[10^(-2), "Kilobytes"]
]

VerificationTest[(* 6 *)
  Sin[60 Degree]
	,
  Sqrt[3]/2
	,
	TestID->"myTestID"
]

VerificationTest[(* 7 *)
  Simplify[2 Sin[10 x + 11 y + z] Cos[10 x + 10 y + 10 t]]
	,
  2 Cos[10 t + 10 x + 10 y] Sin[10 x + 11 y + z]
	,
	TimeConstraint->Quantity[1, "Seconds"]
]

VerificationTest[(* 8 *)
	FileBaseName[$TestFileName]
	,
	"Tests"
]

EndTestSection[]

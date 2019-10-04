// define type T
def T = []

// binds block to tok's namespace
T => {
  // binds block to parameter list
  // because self is not extant at compile time, evaluation is deferred to run time
  // fnLit := closure
  def a: (&T) -> Bool = (self: &T) => {

  }
}

// where :: is the namespace access operator
Tok::a = Tok::a

// where . is the reflexive parameter operator
tok.a = Tok::a(tok)

let members = T::members
// reflexive view 
let functions = T::Self.filter!(a => a is Arc::Function)
let name = T::__type_info::name
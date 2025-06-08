import gleam/dict.{type Dict}
import gleam/io
import gleam/string
import internal/id_gen

pub type ID =
  Int

pub type LambdaTerm {
  Application(into: LambdaTerm, sub: LambdaTerm)
  Lambda(id: ID, body: LambdaTerm)
  Variable(id: ID)
}

pub type ReductionStrategy {
  ReduceInto
  ReduceSub
  Both
}

pub fn try_reduce_once(
  term: LambdaTerm,
  using strategy: ReductionStrategy,
) -> LambdaTerm {
  try_reduce_loop(in: term, strategy:)
}

fn try_reduce_loop(
  in term: LambdaTerm,
  strategy strategy: ReductionStrategy,
) -> LambdaTerm {
  case term, strategy {
    Application(into: Lambda(id:, body:), sub:), _ ->
      substitute(into: body, for: id, put: sub)

    Application(into:, sub:), ReduceInto ->
      Application(into: into |> try_reduce_loop(strategy:), sub:)
    Application(into:, sub:), ReduceSub ->
      Application(into:, sub: sub |> try_reduce_loop(strategy:))
    Application(into:, sub:), Both ->
      Application(
        into: into |> try_reduce_loop(strategy:),
        sub: sub |> try_reduce_loop(strategy:),
      )

    Lambda(id:, body:), _ ->
      Lambda(id:, body: body |> try_reduce_loop(strategy:))

    Variable(id: _), _ -> term
  }
}

fn substitute(
  into term: LambdaTerm,
  for id: ID,
  put sub_term: LambdaTerm,
) -> LambdaTerm {
  build_term_refactor(for: sub_term)
  //  |> substitute_loop()

  todo
}

fn substitute_search_loop(into term: LambdaTerm)

fn use_term_refactor(
  to term: LambdaTerm,
  using builder: Dict(ID, Nil),
) -> LambdaTerm {
  todo
}

fn build_term_refactor(for term: LambdaTerm) -> Dict(ID, Nil) {
  build_term_refactor_back(from: term, acc: dict.new())
}

fn build_term_refactor_back(
  from term: LambdaTerm,
  acc acc: Dict(ID, Nil),
) -> Dict(ID, Nil) {
  case term {
    Application(into:, sub:) ->
      build_term_refactor_back(from: into, acc:)
      |> build_term_refactor_back(from: sub)
    Lambda(id:, body:) ->
      build_term_refactor_back(
        from: body,
        acc: acc |> dict.insert(for: id, insert: Nil),
      )
    Variable(_) -> acc
  }
}

pub fn main() -> Nil {
  io.println("Hello from lambda!")

  // REMINDER TO SELF: BUILD RRFACTOR DICT ONCE, THEN MAP WITH NEXT ID

  Nil
}

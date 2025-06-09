import gleam/dict.{type Dict}
import gleam/io
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}
import internal/id.{type ID, StringID}

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
  try_reduce_tree(in: term, strategy:)
}

fn try_reduce_tree(
  in term: LambdaTerm,
  strategy strategy: ReductionStrategy,
) -> LambdaTerm {
  case term, strategy {
    Application(into: Lambda(id:, body:), sub:), _ ->
      substitute(into: body, for: id, put: sub)

    Application(into:, sub:), ReduceInto ->
      Application(into: into |> try_reduce_tree(strategy:), sub:)
    Application(into:, sub:), ReduceSub ->
      Application(into:, sub: sub |> try_reduce_tree(strategy:))
    Application(into:, sub:), Both ->
      Application(
        into: into |> try_reduce_tree(strategy:),
        sub: sub |> try_reduce_tree(strategy:),
      )

    Lambda(id:, body:), _ ->
      Lambda(id:, body: body |> try_reduce_tree(strategy:))

    Variable(id: _), _ -> term
  }
}

fn substitute(
  into term: LambdaTerm,
  for id: ID,
  put sub_term: LambdaTerm,
) -> LambdaTerm {
  let refactorer = build_term_refactor(for: sub_term)
  substitute_search_tree(into: term, search_id: id, sub_term:, refactorer:)
}

fn substitute_search_tree(
  into term: LambdaTerm,
  search_id search_id: ID,
  sub_term sub_term: LambdaTerm,
  refactorer refactorer: Dict(ID, Nil),
) -> LambdaTerm {
  case term {
    Application(into:, sub:) ->
      Application(
        into: into |> substitute_search_tree(search_id:, sub_term:, refactorer:),
        sub: sub |> substitute_search_tree(search_id:, sub_term:, refactorer:),
      )
    Lambda(id:, body:) ->
      Lambda(
        id:,
        body: body |> substitute_search_tree(search_id:, sub_term:, refactorer:),
      )
    Variable(id:) if id == search_id ->
      sub_term |> use_term_refactor(refactorer)
    Variable(id:) -> Variable(id:)
  }
}

fn use_term_refactor(
  to term: LambdaTerm,
  using refactorer: Dict(ID, Nil),
) -> LambdaTerm {
  let ready_refactorer =
    refactorer |> dict.map_values(fn(_, _) { id.grab_next() })

  use_term_refactorer_tree(term, ready_refactorer)
}

fn use_term_refactorer_tree(
  to term: LambdaTerm,
  refactorer refactorer: Dict(ID, ID),
) -> LambdaTerm {
  case term {
    Application(into:, sub:) ->
      Application(
        into: into |> use_term_refactorer_tree(refactorer:),
        sub: sub |> use_term_refactorer_tree(refactorer),
      )
    Lambda(id:, body:) ->
      Lambda(id:, body: body |> use_term_refactorer_tree(refactorer))
    Variable(id:) -> Variable(dict.get(refactorer, id) |> result.unwrap(or: id))
  }
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

pub fn are_exactly_same(a: LambdaTerm, b: LambdaTerm) -> Bool {
  make_basic(a) == make_basic(b)
}

fn make_basic(term: LambdaTerm) -> LambdaTerm {
  id.comparison_grabber_reset()

  make_basic_tree(term, acc: dict.new())
}

fn make_basic_tree(term: LambdaTerm, acc acc: Dict(ID, ID)) -> LambdaTerm {
  case term {
    Application(into:, sub:) -> {
      let into = into |> make_basic_tree(acc:)
      let sub = sub |> make_basic_tree(acc:)

      Application(into:, sub:)
    }
    Lambda(id:, body:) -> {
      let new_id = id.comparison_grab_next()
      Lambda(
        id: new_id,
        body: body |> make_basic_tree(acc: acc |> dict.insert(id, new_id)),
      )
    }
    Variable(id:) -> {
      echo id
      echo acc
      let assert Ok(new_id) = dict.get(acc, id)
        as "All variables should be bound to a lambda"
      Variable(id: new_id)
    }
  }
}

pub fn try_reduce_fully(
  reduce term: LambdaTerm,
  max_tries max_tries: Option(Int),
  using strategy: ReductionStrategy,
) -> Result(LambdaTerm, LambdaTerm) {
  try_reduce_fully_loop(
    term:,
    remaining_tries: max_tries,
    visited: set.new() |> set.insert(term),
    strategy:,
  )
}

fn try_reduce_fully_loop(
  term term: LambdaTerm,
  remaining_tries remaining_tries: Option(Int),
  strategy strategy: ReductionStrategy,
  visited visited: Set(LambdaTerm),
) -> Result(LambdaTerm, LambdaTerm) {
  echo term
  case remaining_tries {
    Some(tries) if tries < 0 -> Error(term)
    _ -> {
      let next = try_reduce_once(term, using: strategy)
      case visited |> set.contains(next |> make_basic) {
        True -> Ok(next)
        False ->
          try_reduce_fully_loop(
            term: next,
            remaining_tries: remaining_tries |> option.map(fn(x) { x - 1 }),
            strategy:,
            visited: visited |> set.insert(next |> make_basic),
          )
      }
    }
  }
}

pub fn main() -> Nil {
  io.println("Hello from lambda!")

  let lamb =
    Application(
      into: Lambda(id: StringID("a"), body: Variable(id: StringID("a"))),
      sub: Lambda(id: StringID("b"), body: Variable(StringID("b"))),
    )

  echo lamb
    |> try_reduce_fully(None, Both)

  Nil
}

import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}
import gleam/string
import internal/id.{type ID, NumericalID, StringID}

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
    Lambda(id:, body:) -> {
      let assert Ok(new_id) = dict.get(refactorer, id)
      Lambda(id: new_id, body: body |> use_term_refactorer_tree(refactorer))
    }
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
        body: body
          |> make_basic_tree(acc: acc |> dict.insert(for: id, insert: new_id)),
      )
    }
    Variable(id:) -> {
      let assert Ok(new_id) = dict.get(acc, id)
        as {
        "All variables should be bound to a lambda.\nFound `"
        <> string.inspect(id)
        <> "` which was not present in acc:\n"
        <> string.inspect(acc)
      }
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

pub fn to_string(term: LambdaTerm) -> String {
  to_string_back(term, False)
}

fn to_string_back(term term: LambdaTerm, was_lambda was_lambda: Bool) -> String {
  case term, was_lambda {
    Application(into:, sub:), _ -> {
      to_string_back(term: into, was_lambda: False)
      <> " ("
      <> to_string_back(term: sub, was_lambda: False)
      <> ")"
    }
    Lambda(id:, body:), False ->
      "(λ"
      <> id_to_string(id)
      <> "."
      <> to_string_back(term: body, was_lambda: True)
      <> ")"
    Lambda(id:, body:), True ->
      id_to_string(id) <> "." <> to_string_back(term: body, was_lambda: True)
    Variable(id:), _ -> id_to_string(id)
  }
}

fn id_to_string(id: ID) -> String {
  case id {
    NumericalID(count) -> int.to_string(count)
    StringID(name) -> name
  }
}

pub fn main() -> Nil {
  io.println("Hello from lambda!")

  let lamb =
    Application(
      into: Lambda(id: StringID("a"), body: Variable(id: StringID("a"))),
      sub: Lambda(id: StringID("b"), body: Variable(id: StringID("b"))),
    )

  lamb
  |> to_string
  |> io.println

  lamb
  |> try_reduce_fully(None, Both)
  |> result.unwrap_both
  |> to_string
  |> io.println

  let lamb2 =
    Application(
      into: Lambda(
        id: StringID("true"),
        body: Application(
          into: Lambda(
            id: StringID("false"),
            body: Application(
              into: Variable(StringID("true")),
              sub: Variable(StringID("false")),
            ),
          ),
          sub: Lambda(
            id: StringID("y0"),
            body: Lambda(id: StringID("y1"), body: Variable(StringID("y1"))),
          ),
        ),
      ),
      sub: Lambda(
        id: StringID("x0"),
        body: Lambda(id: StringID("x1"), body: Variable(StringID("x0"))),
      ),
    )
  lamb2 |> to_string |> io.println
  lamb2
  |> try_reduce_fully(None, Both)
  |> result.unwrap_both
  |> to_string
  |> io.println

  Nil
}

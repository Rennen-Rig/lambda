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

/// Try a single reduction step
///
pub fn try_reduce_once(
  term: LambdaTerm,
  using strategy: ReductionStrategy,
) -> LambdaTerm {
  try_reduce_tree(in: term, strategy:)
}

/// The back end for reducing terms.
/// Reducrsively splits terms until an application onto
/// a lambda is found, then substitutes accordingly
///
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

/// Substitutes a term into the body of a lambda,
/// at every instance of an id matching that of the
/// lambda.
///
///
/// Worka by first traversing the new term, so thaf
/// can be alpha-converted more easily.
///
/// This happens so there are never any id collisions
/// if a term appears twice in another
///
fn substitute(
  into term: LambdaTerm,
  for id: ID,
  put sub_term: LambdaTerm,
) -> LambdaTerm {
  let empty_convert_map = build_alpha_convert_map(for: sub_term)
  substitute_search_tree(
    into: term,
    search_id: id,
    sub_term:,
    empty_convert_map:,
  )
}

/// Finds the variables matching the id to be substituted.
/// Then performs the substitution
///
fn substitute_search_tree(
  into term: LambdaTerm,
  search_id search_id: ID,
  sub_term sub_term: LambdaTerm,
  empty_convert_map empty_convert_map: Dict(ID, Nil),
) -> LambdaTerm {
  case term {
    Application(into:, sub:) ->
      Application(
        into: into
          |> substitute_search_tree(search_id:, sub_term:, empty_convert_map:),
        sub: sub
          |> substitute_search_tree(search_id:, sub_term:, empty_convert_map:),
      )
    Lambda(id:, body:) ->
      Lambda(
        id:,
        body: body
          |> substitute_search_tree(search_id:, sub_term:, empty_convert_map:),
      )
    Variable(id:) if id == search_id ->
      sub_term |> perform_alpha_convert(empty_convert_map)
    Variable(id:) -> Variable(id:)
  }
}

/// Alpha converts a term, by filling a dictionary of all
/// terms to alpha convert with new IDs
///
fn perform_alpha_convert(
  to term: LambdaTerm,
  using empty_convert_map: Dict(ID, Nil),
) -> LambdaTerm {
  let convert_map =
    empty_convert_map |> dict.map_values(fn(_, _) { id.grab_next() })
  // fill the map with the next free IDs

  perform_alpha_convert_back(to: term, convert_map:)
}

/// The recursive part of `perform_alpha_convert`
///
fn perform_alpha_convert_back(
  to term: LambdaTerm,
  convert_map convert_map: Dict(ID, ID),
) -> LambdaTerm {
  case term {
    Application(into:, sub:) ->
      Application(
        into: into |> perform_alpha_convert_back(convert_map:),
        sub: sub |> perform_alpha_convert_back(convert_map:),
      )

    Lambda(id:, body:) -> {
      let assert Ok(new_id) = dict.get(convert_map, id)
      Lambda(id: new_id, body: body |> perform_alpha_convert_back(convert_map))
    }
    Variable(id:) ->
      Variable(id: dict.get(convert_map, id) |> result.unwrap(or: id))
  }
}

/// Builds an empty dictionary of all the variables bound inside
/// a term. This is used for alpha conversion
///
fn build_alpha_convert_map(for term: LambdaTerm) -> Dict(ID, Nil) {
  build_alpha_convert_map_back(from: term, acc: dict.new())
}

/// The recursive part of `build_alpha_convert_map`
///
fn build_alpha_convert_map_back(
  from term: LambdaTerm,
  acc acc: Dict(ID, Nil),
) -> Dict(ID, Nil) {
  case term {
    Application(into:, sub:) ->
      build_alpha_convert_map_back(from: into, acc:)
      |> build_alpha_convert_map_back(from: sub)
    Lambda(id:, body:) ->
      build_alpha_convert_map_back(
        from: body,
        acc: acc |> dict.insert(for: id, insert: Nil),
      )
    Variable(_) -> acc
  }
}

/// Checks if two lambda terms can be made the same using only
/// alpha conversion
pub fn are_exactly_same(a: LambdaTerm, b: LambdaTerm) -> Bool {
  make_basic(a) == make_basic(b)
}

/// Alpha converts all the terms in a lambda term so that all
/// IDs are ordered from 0+
///
fn make_basic(term: LambdaTerm) -> LambdaTerm {
  id.comparison_grabber_reset()

  make_basic_tree(term, acc: dict.new())
}

/// The recursive part of `make_basic`.
/// Works by keeping a dictionary of all tbe bound variables
/// found so far, and changing variables accordingly.
///
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

/// Repeatedly tries to beta reduce a term, until it either
/// runs out of tries or cycles.
///
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

/// The recursive part or `try_reduce_fully`
/// Works by storing a set of all the terms that the term was
/// previously, then checking against thosd to find cycles.
///
fn try_reduce_fully_loop(
  term term: LambdaTerm,
  remaining_tries remaining_tries: Option(Int),
  strategy strategy: ReductionStrategy,
  visited visited: Set(LambdaTerm),
) -> Result(LambdaTerm, LambdaTerm) {
  term |> to_string |> io.println
  io.println("")

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

/// Converts a lambda term to a human-readable string
///
pub fn to_string(term: LambdaTerm) -> String {
  to_string_back(term, False)
}

/// The recursive part of `to_string`
///
fn to_string_back(term term: LambdaTerm, was_lambda was_lambda: Bool) -> String {
  case term, was_lambda {
    Application(into:, sub:), False -> {
      to_string_back(term: into, was_lambda: False)
      <> " ("
      <> to_string_back(term: sub, was_lambda: False)
      <> ")"
    }
    Application(into:, sub:), True -> {
      "("
      <> to_string_back(term: into, was_lambda: False)
      <> " "
      <> to_string_back(term: sub, was_lambda: False)
      <> ")"
    }
    Lambda(id:, body:), False ->
      "λ"
      <> id_to_string(id)
      <> "."
      <> to_string_back(term: body, was_lambda: True)
    Lambda(id:, body:), True ->
      id_to_string(id) <> "." <> to_string_back(term: body, was_lambda: True)
    Variable(id:), _ -> id_to_string(id)
  }
}

/// Converts an ID to a string, for use in converting lambdas
/// to strings
///
fn id_to_string(id: ID) -> String {
  case id {
    NumericalID(count) -> int.to_string(count)
    StringID(name) -> name
  }
}

pub fn main() -> Nil {
  io.println("Hello from lambda!")

  let lamb3 =
    do_wrap(
      around: Application(
        into: Application(
          into: Variable(StringID("and")),
          sub: Application(
            into: Variable(StringID("not")),
            sub: Variable(StringID("false")),
          ),
        ),
        sub: Variable(StringID("true")),
      ),
      with: [
        #(
          "and",
          Lambda(
            id: StringID("z0"),
            body: Lambda(
              id: StringID("z1"),
              body: Application(
                into: Application(
                  into: Variable(StringID("z0")),
                  sub: Variable(StringID("z1")),
                ),
                sub: Variable(StringID("false")),
              ),
            ),
          ),
        ),
        #(
          "not",
          Lambda(
            id: StringID("n0"),
            body: Application(
              into: Application(
                into: Variable(StringID("n0")),
                sub: Variable(StringID("false")),
              ),
              sub: Variable(StringID("true")),
            ),
          ),
        ),
        #(
          "true",
          Lambda(
            id: StringID("x0"),
            body: Lambda(id: StringID("x1"), body: Variable(StringID("x0"))),
          ),
        ),
        #(
          "false",
          Lambda(
            id: StringID("y0"),
            body: Lambda(id: StringID("y1"), body: Variable(StringID("y1"))),
          ),
        ),
      ],
    )

  lamb3
  |> try_reduce_fully(None, Both)

  Nil
}

fn do_wrap(
  around term: LambdaTerm,
  with definitions: List(#(String, LambdaTerm)),
) -> LambdaTerm {
  do_wrap_lambdas(term:, lams: definitions)
}

fn do_wrap_lambdas(
  term term: LambdaTerm,
  lams lams: List(#(String, LambdaTerm)),
) -> LambdaTerm {
  case lams {
    [#(str_id, lambda), ..rest] ->
      do_wrap_lambdas(
        term: Application(
          into: Lambda(id: StringID(str_id), body: term),
          sub: lambda,
        ),
        lams: rest,
      )
    [] -> term
  }
}

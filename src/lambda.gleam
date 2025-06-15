import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}
import gleam/string
import internal/id.{type ID, NumericalID, StringID}
import lap

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
  //term |> to_string |> io.println
  //io.println("")

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

  let timer = lap.start_in_milliseconds("make definitions")

  let defs = [
    #("Nil", {
      let i = id.grab_next()
      Lambda(id: i, body: Variable(i))
    }),
    #("True", {
      let first = id.grab_next()
      let second = id.grab_next()
      Lambda(id: first, body: Lambda(id: second, body: Variable(first)))
    }),
    #("False", {
      let first = id.grab_next()
      let second = id.grab_next()
      Lambda(id: first, body: Lambda(id: second, body: Variable(second)))
    }),
    #("not", {
      let input = id.grab_next()
      Lambda(
        id: input,
        body: Application(
          into: Application(
            into: Variable(input),
            sub: Variable(StringID("False")),
          ),
          sub: Variable(StringID("True")),
        ),
      )
    }),
    #("and", {
      let input1 = id.grab_next()
      let input2 = id.grab_next()
      Lambda(
        id: input1,
        body: Lambda(
          id: input2,
          body: Application(
            into: Application(into: Variable(input1), sub: Variable(input2)),
            sub: Variable(StringID("False")),
          ),
        ),
      )
    }),
    #("Pair", {
      let first = id.grab_next()
      let second = id.grab_next()
      let selector = id.grab_next()

      Lambda(
        id: first,
        body: Lambda(
          id: second,
          body: Lambda(
            id: selector,
            body: Application(
              into: Application(into: Variable(selector), sub: Variable(first)),
              sub: Variable(second),
            ),
          ),
        ),
      )
    }),
    #("Error", {
      Application(
        into: Variable(StringID("Pair")),
        sub: Variable(StringID("False")),
      )
    }),
    #("Ok", {
      Application(
        into: Variable(StringID("Pair")),
        sub: Variable(StringID("True")),
      )
    }),
    #("successor", {
      let n = id.grab_next()
      let f = id.grab_next()
      let x = id.grab_next()

      Lambda(
        id: n,
        body: Lambda(
          id: f,
          body: Lambda(
            id: x,
            body: Application(
              into: Application(into: Variable(n), sub: Variable(f)),
              sub: Application(into: Variable(f), sub: Variable(x)),
            ),
          ),
        ),
      )
    }),
    #("pred_fallible", {
      let n = id.grab_next()
      let r = id.grab_next()

      Lambda(
        id: n,
        body: Application(
          into: Application(
            into: Variable(n),
            // for n times, do thing below

            sub: Lambda(
              // take in r
              id: r,
              body: Application(
                into: Application(
                  // conditional, based on if r is Ok
                  into: Application(
                    into: Variable(r),
                    sub: Variable(StringID("True")),
                  ),
                  // if r is ok, do this
                  sub: Application(
                    into: Variable(StringID("Ok")),
                    sub: Application(
                      into: Variable(StringID("successor")),
                      sub: Application(
                        into: Variable(r),
                        sub: Variable(StringID("False")),
                      ),
                    ),
                  ),
                ),
                // if r is an error, do this
                sub: Application(
                  into: Variable(StringID("Ok")),
                  sub: make_number(0),
                ),
              ),
            ),
          ),
          // inital value below

          sub: Application(
            into: Variable(StringID("Error")),
            sub: Variable(StringID("Nil")),
          ),
        ),
      )
    }),
    #("add", {
      let n = id.grab_next()
      let m = id.grab_next()
      let f = id.grab_next()
      let x = id.grab_next()

      Lambda(
        id: n,
        body: Lambda(
          id: m,
          body: Lambda(
            id: f,
            body: Lambda(
              id: x,
              body: Application(
                into: Application(into: Variable(n), sub: Variable(f)),
                sub: Application(
                  into: Application(into: Variable(m), sub: Variable(f)),
                  sub: Variable(x),
                ),
              ),
            ),
          ),
        ),
      )
    }),
    #("multiply", {
      let n = id.grab_next()
      let m = id.grab_next()
      let f = id.grab_next()

      Lambda(
        id: n,
        body: Lambda(
          id: m,
          body: Lambda(
            id: f,
            body: Application(
              into: Variable(n),
              sub: Application(into: Variable(m), sub: Variable(f)),
            ),
          ),
        ),
      )
    }),
    #("factorial", {
      let n = id.grab_next()
      let acc = id.grab_next()

      Lambda(
        id: n,
        body: Application(
          into: Application(
            into: Application(
              into: Variable(n),
              sub: Lambda(
                id: acc,
                body: Application(
                  into: Application(
                    into: Variable(StringID("Pair")),
                    sub: Application(
                      into: Application(
                        into: Variable(StringID("multiply")),
                        sub: Application(
                          into: Variable(acc),
                          sub: Variable(StringID("True")),
                        ),
                      ),
                      sub: Application(
                        into: Variable(acc),
                        sub: Variable(StringID("False")),
                      ),
                    ),
                    // #(old_prod * old_next_index, _)
                  ),
                  sub: Application(
                    into: Variable(StringID("successor")),
                    sub: Application(
                      into: Variable(acc),
                      sub: Variable(StringID("False")),
                    ),
                  ),
                  // #(_, succ(old_next_index))
                ),
              ),
            ),
            sub: Application(
              into: Application(
                into: Variable(StringID("Pair")),
                sub: make_number(1),
              ),
              sub: make_number(1),
            ),
            // sub in starting val of #(1, 1) for #(product, next_index)
          ),
          sub: Variable(StringID("True")),
        ),
      )
    }),
  ]

  let timer = timer |> lap.time("create term")

  let lamb3 =
    Lambda(
      id: StringID("x"),
      body: Application(
        into: Application(
          into: Variable(StringID("pred_fallible")),
          sub: Variable(StringID("x")),
        ),
        sub: Variable(StringID("False")),
      ),
    )
    |> do_wrap(with: defs)

  let timer = timer |> lap.time("reduce term")

  let reduced =
    lamb3
    |> try_reduce_fully(None, Both)
    |> result.unwrap_both

  io.println("\nReduced to")
  reduced
  |> to_string
  |> io.println

  io.println("\nLikely more readable as")
  reduced
  |> make_basic
  |> to_string
  |> io.println

  reduced
  |> try_interpret_number
  |> result.map(fn(x) {
    io.println("\nInterpretted as number: " <> int.to_string(x))
  })
  |> result.map_error(fn(_) { io.println("Failed to interpret as a number") })

  let timer = timer |> lap.time("end")
  timer |> lap.sort_max |> lap.pretty_print |> io.println

  Nil
}

/// Wraps a lambda term in bindings, so that a term can be defined
/// and used inside another without manually formatting it
/// 
fn do_wrap(
  around term: LambdaTerm,
  with definitions: List(#(String, LambdaTerm)),
) -> LambdaTerm {
  do_wrap_lambdas(term:, lams: list.reverse(definitions))
}

/// The recursive part of `do_wrap`
/// 
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

fn make_number(from n: Int) -> LambdaTerm {
  let assert True = n >= 0
    as {
    "I only know how to make naturals!\n`"
    <> int.to_string(n)
    <> "` is not natural`"
  }

  let f_id = id.grab_next()
  let x_id = id.grab_next()

  make_number_loop(remaining_loops: n, f_id:, x_id:, acc: Variable(x_id))
}

fn make_number_loop(
  remaining_loops remaining_loops: Int,
  f_id f_id: ID,
  x_id x_id: ID,
  acc acc: LambdaTerm,
) -> LambdaTerm {
  case remaining_loops {
    0 -> Lambda(id: f_id, body: Lambda(id: x_id, body: acc))
    _ ->
      make_number_loop(
        remaining_loops: remaining_loops - 1,
        f_id:,
        x_id:,
        acc: Application(into: Variable(f_id), sub: acc),
      )
  }
}

fn try_interpret_number(from term: LambdaTerm) -> Result(Int, Nil) {
  case term {
    Lambda(id: f, body: Lambda(id: x, body: rest)) ->
      try_interpret_number_loop(from: rest, f:, x:, acc: 0)
    _ -> Error(Nil)
  }
}

fn try_interpret_number_loop(
  from term: LambdaTerm,
  f f: ID,
  x x: ID,
  acc acc: Int,
) -> Result(Int, Nil) {
  case term {
    Application(into: Variable(id:), sub: rest) if id == f ->
      try_interpret_number_loop(from: rest, f:, x:, acc: acc + 1)
    Variable(id:) if id == x -> Ok(acc)
    _ -> Error(Nil)
  }
}

import envoy
import gleam/int
import gleam/result

pub type ID {
  NumericalID(Int)
  StringID(String)
}

/// Returns the next id, and updates state so that all
/// ids taken are unique.
/// 
pub fn grab_next() -> ID {
  let current =
    envoy.get("lambda_tracker")
    |> result.map_error(fn(_) {
      envoy.set("lambda_tracker", "0")
      0
    })
    |> result.map(fn(id_str) {
      let assert Ok(val) = int.parse(id_str)
      val
    })
    |> result.unwrap_both

  envoy.set("lambda_tracker", int.to_string(current + 1))
  NumericalID(current)
}

/// Resets the counter use for comparisons. Used so that two terms
/// can be refactored, so they can be compared regardless of alpha
/// conversion
/// 
pub fn comparison_grabber_reset() -> Nil {
  envoy.unset("comparison_tracker")
}

/// Gets the next value from the counter for comparisons, and increments
/// the counter.
/// 
pub fn comparison_grab_next() -> ID {
  let current =
    envoy.get("comparison_tracker")
    |> result.map_error(fn(_) {
      envoy.set("comparison_tracker", "0")
      0
    })
    |> result.map(fn(id_str) {
      let assert Ok(val) = int.parse(id_str)
      val
    })
    |> result.unwrap_both

  envoy.set("comparison_tracker", int.to_string(current + 1))
  NumericalID(current)
}

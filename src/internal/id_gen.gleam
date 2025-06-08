import envoy
import gleam/int
import gleam/result

/// Returns the next id, and updates state so that all
/// ids taken are unique.
/// 
pub fn grab_next() -> Int {
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
  current
}

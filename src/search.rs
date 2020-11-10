/// Iterative deepening loop.
///
/// Calls the given function repeatedly with successive depth values, until the function returns
/// `None`, indicating that the loop should terminate. Returns the last `Some` value returned by
/// the given function, or `None` if the function immediately returns `None`.
pub fn iter_deepen<F>(mut func: F) -> Option<f64>
where
    F: FnMut(u64) -> Option<f64>,
{
    (0..).filter_map(|depth| func(depth)).last()
}

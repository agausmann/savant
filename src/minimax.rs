use std::f64;

/// Depth-limited negamax. Calculates an evaluation of a board state using a recursive algorithm
/// that evaluates child states and assumes optimal play.
///
/// # Parameters
///
/// - `state` - The state to evaluate.
///
/// - `actions` - An action generation function.
///
///   Accepts a state and a callback closure, and calls the callback for each new state generated
///   by the actions available in the given state.
///
///   If this is a terminal state, no actions should be available, and the callback should not be
///   called.
///
/// - `heuristic` - A heuristic state evaluation.
///
///   This is used to evaluate terminal states and states at the maximum depth.
///
///   It should return a value that approximates the value/utility of the given state from the
///   perspective of the player who is _next to move_, with more positive values being more
///   favorable to that player.
///
/// - `depth` - The maximum recursion depth.
///
///   When depth reaches zero, the function will not recurse, and will instead return the heuristic
///   for the given state.
pub fn dl_negamax<S, A, H>(state: &S, actions: &A, heuristic: &mut H, depth: u64) -> f64
where
    A: Fn(&S, &mut dyn FnMut(&S)),
    H: FnMut(&S) -> f64,
{
    if depth == 0 {
        heuristic(state)
    } else {
        let mut max_eval = -f64::INFINITY;
        let mut visited = false;
        actions(state, &mut |child| {
            max_eval = max_eval.max(-dl_negamax(child, actions, heuristic, depth - 1));
            visited = true;
        });
        if visited {
            max_eval
        } else {
            heuristic(state)
        }
    }
}

/// Depth-limited negamax with alpha-beta pruning. An extension of [`dl_negamax`] that prunes
/// branches of the game tree which are not worth searching.
///
/// # Parameters
///
/// Except for the differences listed here, the parameters are equivalent to those given to
/// [`dl_negamax`].
///
/// - `actions` - The action generation function.
///
///   The callback passed to this function now returns a [`ControlFlow`] value, indicating whether
///   it wishes to `Continue` to the next state to evaluate, or `Break` and stop evaluating states.
///
/// - `alpha` - The minimum score assured of the current player.
///
///   Can be initialized to `-f64::INFINITY` if unknown.
///
/// - `beta` - The maximum score assured of the current player.
///
///   Can be initialized to `f64::INFINITY` if unknown.
pub fn dl_negamax_ab<S, A, H>(
    state: &S,
    actions: &A,
    heuristic: &mut H,
    depth: u64,
    mut alpha: f64,
    beta: f64,
) -> f64
where
    A: Fn(&S, &mut dyn FnMut(&S) -> ControlFlow),
    H: FnMut(&S) -> f64,
{
    if depth == 0 {
        heuristic(state)
    } else {
        let mut max_eval = -f64::INFINITY;
        let mut visited = false;
        actions(state, &mut |child| {
            max_eval = max_eval.max(-dl_negamax_ab(
                child,
                actions,
                heuristic,
                depth - 1,
                -beta,
                -alpha,
            ));
            alpha = alpha.max(max_eval);
            visited = true;
            if alpha < beta {
                ControlFlow::Continue
            } else {
                ControlFlow::Break
            }
        });
        if visited {
            max_eval
        } else {
            heuristic(state)
        }
    }
}

pub enum ControlFlow {
    Continue,
    Break,
}

"""Robust straight-line fit y = a + b*x with the intercept held at a >= 0.

Iteratively reweighted least squares: a Huber pass first (convex, so it finds the bulk of the data
from any start), then a Tukey bisquare pass, which gives points far from the line zero weight, so
they stop pulling on it at all. Those are the outliers. Residuals are scaled by their median
absolute size (MAD / 0.6745, about one standard deviation for normal noise).

The bound on a is exact: the weighted least squares objective is a convex quadratic in (a, b), so
if its unconstrained minimum has a < 0, the constrained minimum lies on a = 0, a fit through the
origin.

Standard library only, so the tool runs with any python3.
"""

from statistics import median

HUBER_C = 1.345  # 95% efficiency for normal noise
BISQUARE_C = 4.685  # Same; residuals beyond C scales get zero weight
MAX_ITER = 100
TOL = 1e-12


def _wls(xs, ys, ws):
    """Weighted least squares line with a >= 0. Returns (a, b, at_bound)."""
    s = sx = sy = sxx = sxy = 0.0
    for x, y, w in zip(xs, ys, ws):
        s += w
        sx += w * x
        sy += w * y
        sxx += w * x * x
        sxy += w * x * y
    if s <= 0:
        return 0.0, 0.0, False
    det = s * sxx - sx * sx
    if det > 1e-12 * max(1.0, s * sxx):
        b = (s * sxy - sx * sy) / det
        a = (sy - b * sx) / s
    else:  # All x equal: no slope to find.
        a, b = sy / s, 0.0
    if a < 0:
        return 0.0, (sxy / sxx if sxx > 0 else 0.0), True
    return a, b, False


def _scale(residuals, floor):
    return max(median(abs(r) for r in residuals) / 0.6745, floor)


def _huber(u):
    return 1.0 if abs(u) <= HUBER_C else HUBER_C / abs(u)


def _bisquare(u):
    return (1 - (u / BISQUARE_C) ** 2) ** 2 if abs(u) < BISQUARE_C else 0.0


def robust_line(xs, ys, scale_floor=0.016):
    """Fit y = a + b*x, a >= 0, ignoring outliers.

    scale_floor keeps the residual scale from collapsing to zero when most points sit exactly on
    a line, which would make every other point an outlier. The default is the battery's energy
    reading step, about 16 mWh: residuals smaller than that aren't measurable.

    Returns None for fewer than two points, else a dict with intercept, slope, at_bound (the
    unconstrained intercept would be negative), scale, weights (0 = outlier), outlier flags and
    counts.
    """
    n = len(xs)
    if n < 2:
        return None
    ws = [1.0] * n
    a, b, at_bound = _wls(xs, ys, ws)
    for weight in (_huber, _bisquare):
        for _ in range(MAX_ITER):
            res = [y - a - b * x for x, y in zip(xs, ys)]
            s = _scale(res, scale_floor)
            new_ws = [weight(r / s) for r in res]
            if sum(new_ws) <= 0:
                break
            ws = new_ws
            a2, b2, at_bound = _wls(xs, ys, ws)
            done = abs(a2 - a) < TOL and abs(b2 - b) < TOL
            a, b = a2, b2
            if done:
                break
    res = [y - a - b * x for x, y in zip(xs, ys)]
    s = _scale(res, scale_floor)
    ws = [_bisquare(r / s) for r in res]
    outlier = [w == 0 for w in ws]
    return {
        "intercept": a,
        "slope": b,
        "at_bound": at_bound,
        "scale": s,
        "weights": ws,
        "outlier": outlier,
        "n": n,
        "n_outliers": sum(outlier),
    }

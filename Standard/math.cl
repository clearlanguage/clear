import "math.h" as "_cmath"


function sin(float64 x) -> float64:
    return _cmath.sin(x)

function cos(float64 x) -> float64:
    return _cmath.cos(x)

function tan(float64 x) -> float64:
    return _cmath.tan(x)

function asin(float64 x) -> float64:
    return _cmath.asin(x)

function acos(float64 x) -> float64:
    return _cmath.acos(x)

function atan(float64 x) -> float64:
    return _cmath.atan(x)

function atan2(float64 y, float64 x) -> float64:
    return _cmath.atan2(y, x)

function sinh(float64 x) -> float64:
    return _cmath.sinh(x)

function cosh(float64 x) -> float64:
    return _cmath.cosh(x)

function tanh(float64 x) -> float64:
    return _cmath.tanh(x)

function exp(float64 x) -> float64:
    return _cmath.exp(x)

function log(float64 x) -> float64:
    return _cmath.log(x)

function log10(float64 x) -> float64:
    return _cmath.log10(x)

function pow(float64 base, float64 exponent) -> float64:
    return _cmath.pow(base, exponent)

function sqrt(float64 x) -> float64:
    return _cmath.sqrt(x)

function ceil(float64 x) -> float64:
    return _cmath.ceil(x)

function floor(float64 x) -> float64:
    return _cmath.floor(x)

function abs(float64 x) -> float64:
    return _cmath.fabs(x)


function logb(float64 base,float64 x ) -> float64:
    return log(x) / log(base)
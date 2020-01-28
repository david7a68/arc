def float2 : [x: float, y: float]

def float2x2 : [
    float, float,
    float, float,
]

def identity_2x2 : float2x2 = [
    1, 0
    0, 1
]

def float2_multiply := (v: float2, s: float) -> {
    result : float2 = [
        v.x * s
        v.y * s
    ]

    return result
}

def float2x2_multiply : (float2x2, float) -> float2x3 = (m22, v) -> {
    result : float2x2;

    i := 0
    loop {if i < m22.length {
        result[i] = m22[i] * v;
        i = i + 1;
    } else { break }}

    return result;
}

def float2x2_multiply := (m22: float2x2, v: float2) -> float2 {
    return [
        m22[0] * v[0] + m22[1] * v[1]
        m22[2] * v[0] * m22[3] * v[1]
    ]
}

def float2x2_dot : (float2x2, float2x2) -> float;
def float2x2_dot := (a, b) -> a[0] * b[0] + a[1] * b[1];

def abs := (f: float) -> {
    if f < 0
        { return (-f, true) }
    else
        { return (f, false) }
}

def float3 : [x: float, y: float, z: float]

def float3x3 : [
    float, float, float
    float, float, float
    float, float, float
]

def identity_3x3 : float3x3 = [
    1, 0, 0
    0, 1, 0
    0, 0, 1
]

def float3_multiply := (v: float3, s: float) -> {
    result : float3 = [
        v.x * s
        v.y * s
        v.z * s
    ]

    return result
}

def float3x3_multiply : (float3x3, float) -> float3x3 = (m33, v) -> {
    result : float3x3
    
    i := 0
    loop {if (i < m33.length) {
        result[i] = m33[i] * v
    } else { break }}

    return result
}

def float3x3_multiply := (m33: float3x3, v: float3) -> float3 {
    return [
        m33[0] * v[0] + m33[1] * v[1] + m33[2] * v[2]
        m33[3] * v[0] + m33[4] * v[1] + m33[5] * v[2]
        m33[6] * v[0] + m33[7] * v[1] + m33[8] * v[2]
    ]
}

def float3x3_cross : (float3x3, float3x3) -> float3x3

def abs := (f: float) -> if f < 0 { (-f, true) } else { (f, false) }
